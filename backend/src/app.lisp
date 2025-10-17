;;;; app.lisp - Clack application builder for CollabCanvas
;;;; This file creates the Clack application using Lack middleware

(in-package #:collabcanvas)

;;; Clack Environment Helper Functions

(defun get-env-header (env header-name)
  "Get a header from Clack environment.
   header-name should be a keyword like :x-session-id
   Headers are stored in a hash table at :headers key"
  (let ((headers (getf env :headers)))
    (when headers
      ;; Try lowercase with hyphens (standard HTTP header format)
      (gethash (string-downcase (string header-name)) headers))))

(defun get-env-cookie (env cookie-name)
  "Get a cookie value from Clack environment"
  (let* ((headers (getf env :headers))
         (cookie-header (when headers
                         (gethash "cookie" headers))))
    (when cookie-header
      (let ((cookies (cl-ppcre:split "; ?" cookie-header)))
        (dolist (cookie cookies)
          (let ((parts (cl-ppcre:split "=" cookie)))
            (when (string= (first parts) cookie-name)
              (return-from get-env-cookie (second parts)))))))))

(defun get-client-ip-from-env (env)
  "Extract client IP address from Clack environment, considering proxy headers.
   Returns IP address string (IPv4 or IPv6)."
  (or (get-env-header env :x-forwarded-for)
      (get-env-header env :x-real-ip)
      (getf env :remote-addr)))

(defun parse-env-body (env)
  "Parse JSON body from Clack environment"
  (let* ((content-length (getf env :content-length))
         (input-stream (getf env :raw-body)))
    (when (and content-length (> content-length 0) input-stream)
      (let* ((body-bytes (make-array content-length :element-type '(unsigned-byte 8)))
             (bytes-read (read-sequence body-bytes input-stream))
             (body-string (flexi-streams:octets-to-string body-bytes :end bytes-read)))
        (handler-case
            ;; Parse JSON and convert string/symbol keys to keyword keys
            (let* ((parsed (jonathan:parse body-string :as :alist))
                   (result (mapcar (lambda (pair)
                                    (cons (intern (string-upcase (string (car pair))) :keyword)
                                          (cdr pair)))
                                  parsed)))
              result)
          (error (e)
            (format t "[ERROR] Failed to parse JSON body: ~A~%" e)
            nil))))))

(defun clack-json-response (data &key (status 200))
  "Return Clack response with JSON data.
   Returns (status headers body-list) format."
  (list status
        '(:content-type "application/json")
        (list (to-json-string data))))

(defun clack-error-response (message &key (status 400))
  "Return Clack error response"
  (clack-json-response `((:error . ,message)) :status status))

(defun clack-success-response (data)
  "Return Clack success response"
  (clack-json-response `((:success . t) (:data . ,data))))

(defun add-cors-headers (response)
  "Add CORS headers to a Clack response"
  (let ((status (first response))
        (headers (second response))
        (body (third response)))
    (list status
          (append headers
                  (list :access-control-allow-origin *cors-origin*
                        :access-control-allow-methods "GET, POST, PUT, DELETE, OPTIONS"
                        :access-control-allow-headers "Content-Type, Authorization, X-Session-ID"
                        :access-control-allow-credentials "true"))
          body)))

(defun handle-websocket-upgrade (env)
  "Handle WebSocket upgrade request.
   Extracts canvas-id from path, creates connection, returns 101 Switching Protocols."
  (let* ((path-info (getf env :path-info))
         (canvas-id (when (cl-ppcre:scan "^/ws/" path-info)
                     (third (cl-ppcre:split "/" path-info)))))

    (unless canvas-id
      (return-from handle-websocket-upgrade
        (clack-error-response "Invalid WebSocket path - expected /ws/<canvas-id>" :status 400)))

    ;; Create WebSocket connection using websocket-driver
    (let* ((conn-id (format nil "ws-~A-~A" canvas-id (get-universal-time)))
           (ws (websocket-driver:make-server env)))

      (format t "[WS] Upgrade request for canvas: ~A (conn: ~A)~%" canvas-id conn-id)

      ;; Create connection structure
      (let ((conn (make-ws-connection
                   :id conn-id
                   :websocket ws
                   :canvas-id canvas-id
                   :connected-at (get-universal-time))))

        ;; Register connection
        (register-ws-connection conn)

        ;; Set up WebSocket callbacks using websocket-driver
        (websocket-driver:on :open ws
          (lambda ()
            (format t "[WS] Connection opened: ~A~%" conn-id)))

        (websocket-driver:on :message ws
          (lambda (message)
            (handle-ws-message conn-id message)))

        (websocket-driver:on :close ws
          (lambda (&key code reason)
            (declare (ignore code reason))
            (format t "[WS] Connection closed: ~A~%" conn-id)
            (handle-ws-disconnect conn-id)))

        ;; Start the WebSocket connection
        (websocket-driver:start-connection ws)

        ;; Return function for async response handling
        (lambda (responder)
          (declare (ignore responder))
          ws)))))

(defun make-app ()
  "Build and return the Clack application without middleware."
  (lambda (env)
    (let ((request-method (getf env :request-method))
          (path-info (getf env :path-info)))

      ;; Check for WebSocket upgrade request - return directly if WebSocket
      (if (cl-ppcre:scan "^/ws/" path-info)
          (handle-websocket-upgrade env)

          ;; Otherwise, route requests to appropriate handlers and add CORS
          (add-cors-headers
           (cond
             ;; Health check endpoint
             ((string= path-info "/health")
              (handle-health env))

             ;; API endpoints
             ((string= path-info "/api/logout")
              (handle-logout env))

             ((string= path-info "/api/session")
              (handle-session-check env))

             ((string= path-info "/api/user/color")
              (handle-update-color env))

             ((cl-ppcre:scan "^/api/canvas/state" path-info)
              (handle-canvas-state env))

             ;; Auth0 OAuth endpoints
             ((string= path-info "/auth0/login")
              (handle-auth0-login-clack env))

             ((string= path-info "/auth0/callback")
              (handle-auth0-callback-clack env))

             ((string= path-info "/auth0/link")
              (handle-auth0-link-clack env))

             ((string= path-info "/api/auth/metrics")
              (handle-auth-metrics-clack env))

             ;; OPTIONS handler for CORS preflight
             ((eq request-method :options)
              (list 200 '(:content-type "text/plain") '("")))

             ;; 404 Not Found
             (t
              (list 404
                    '(:content-type "application/json")
                    (list (to-json-string
                           '((:error . "Not Found"))))))))))))

;;; HTTP Handler Implementations (Clack format)

(defun handle-health (env)
  "Health check endpoint handler."
  (declare (ignore env))
  (clack-json-response
   `((:status . "ok")
     (:service . "collabcanvas")
     (:timestamp . ,(get-universal-time)))))

(defun handle-logout (env)
  "User logout endpoint handler - Clack format.
   Gets session from cookie or Authorization header"
  (let ((session-id (or (get-env-cookie env "session")
                        (get-env-header env :authorization))))
    (if session-id
        (progn
          (logout-user session-id)
          ;; TODO: Clear session cookie when we implement cookie support
          (clack-success-response '((:message . "Logged out successfully"))))
        (clack-error-response "No active session" :status 401))))

(defun handle-session-check (env)
  "Check if current session is valid - Clack format"
  ;; Debug: Print ALL headers from the hash table
  (format t "~%[BACKEND DEBUG] ALL HEADERS IN HASH TABLE:~%")
  (let ((headers (getf env :headers)))
    (when headers
      (maphash (lambda (key value)
                 (format t "  ~A: ~A~%" key value))
               headers)))
  (format t "~%")

  (let ((session-id (or (get-env-cookie env "session")
                        (get-env-header env :x-session-id)
                        (get-env-header env :authorization))))
    (format t "[BACKEND] Session check request~%")
    (format t "[BACKEND] Session ID from cookie: ~A~%" (get-env-cookie env "session"))
    (format t "[BACKEND] Session ID from X-Session-ID header: ~A~%" (get-env-header env :x-session-id))
    (format t "[BACKEND] Session ID from Authorization header: ~A~%" (get-env-header env :authorization))
    (format t "[BACKEND] Using session-id: ~A~%" session-id)

    (if-let ((session (validate-session session-id)))
      (progn
        (format t "[BACKEND] Session valid! Session data: ~A~%" session)
        ;; Add 'valid' field for frontend compatibility
        (clack-success-response (acons :valid t session)))
      (progn
        (format t "[BACKEND] Session invalid or expired~%")
        (clack-error-response "Invalid or expired session" :status 401)))))

(defun handle-canvas-state (env)
  "Canvas state retrieval endpoint handler - Clack format.
   Expects ?canvasId=xxx query parameter"
  (let* ((query-string (getf env :query-string))
         (canvas-id (when query-string
                     (second (cl-ppcre:split "=" query-string)))))
    (if canvas-id
        (let ((state (load-canvas-state canvas-id)))
          (if state
              (clack-json-response state)
              (clack-json-response '((:objects . #())))))
        (clack-error-response "Missing canvasId parameter"))))

(defun handle-update-color (env)
  "Update user's preferred color - Clack format.
   Expects JSON body: {\"color\": \"#3498db\"}"
  (let* ((session-id (or (get-env-cookie env "session")
                         (get-env-header env :x-session-id)
                         (get-env-header env :authorization)))
         (body (parse-env-body env))
         (color (cdr (assoc :color body))))

    (unless session-id
      (return-from handle-update-color
        (clack-error-response "Not authenticated" :status 401)))

    (unless color
      (return-from handle-update-color
        (clack-error-response "Missing color parameter" :status 400)))

    (if-let ((session (validate-session session-id)))
      (let ((user-id (cdr (assoc :user-id session))))
        (set-user-color user-id color)
        (clack-success-response `((:color . ,color))))
      (clack-error-response "Invalid or expired session" :status 401))))

;;; Auth0 OAuth Handler Implementations (Clack format)

(defun handle-auth0-login-clack (env)
  "Auth0 OAuth login initiation - Clack format.
   Redirects to Auth0 authorization endpoint with state CSRF protection.
   Rate limited to prevent abuse."
  ;; Check rate limit first
  (let ((client-ip (get-client-ip-from-env env)))
    (unless (check-rate-limit client-ip)
      (format t "[WARN] Rate limit exceeded for IP: ~A on /auth0/login~%" client-ip)
      (return-from handle-auth0-login-clack
        (clack-error-response "Too many requests. Please try again later." :status 429))))

  (handler-case
      (let* ((query-string (getf env :query-string))
             (connection (when query-string
                          (let ((params (mapcar (lambda (pair)
                                                 (let ((kv (cl-ppcre:split "=" pair)))
                                                   (cons (first kv) (second kv))))
                                               (cl-ppcre:split "&" query-string))))
                            (cdr (assoc "connection" params :test #'string=)))))
             (auth0-response (build-auth0-login-redirect connection))
             (redirect-url (cdr (assoc :redirect-url auth0-response))))
        (if redirect-url
            ;; Return 302 redirect
            (list 302
                  (list :location redirect-url
                        :access-control-allow-origin *cors-origin*
                        :access-control-allow-credentials "true")
                  '(""))
            ;; Error case
            (clack-error-response "Failed to initiate OAuth flow" :status 500)))
    (error (e)
      (format t "[ERROR] Auth0 login error: ~A~%" e)
      (clack-error-response (format nil "Authentication error: ~A" e) :status 500))))

(defun handle-auth0-callback-clack (env)
  "Auth0 OAuth callback handler - Clack format.
   Exchanges authorization code for tokens, validates JWT, creates session.
   Rate limited to prevent abuse."
  ;; Check rate limit first
  (let ((client-ip (get-client-ip-from-env env)))
    (unless (check-rate-limit client-ip)
      (format t "[WARN] Rate limit exceeded for IP: ~A on /auth0/callback~%" client-ip)
      (return-from handle-auth0-callback-clack
        (list 302
              (list :location "/?error=rate_limit_exceeded"
                    :access-control-allow-origin *cors-origin*)
              '("")))))

  (handler-case
      (let* ((query-string (getf env :query-string))
             (params (when query-string
                      (mapcar (lambda (pair)
                                (let ((kv (cl-ppcre:split "=" pair)))
                                  (cons (intern (string-upcase (first kv)) :keyword)
                                        (second kv))))
                              (cl-ppcre:split "&" query-string))))
             (code (cdr (assoc :code params)))
             (state (cdr (assoc :state params)))
             (error-param (cdr (assoc :error params))))

        (cond
          (error-param
           ;; OAuth error from Auth0
           (let ((error-description (cdr (assoc :error--description params))))
             (list 302
                   (list :location (format nil "/?error=~A~@[&error_description=~A~]"
                                          error-param error-description)
                         :access-control-allow-origin *cors-origin*)
                   '(""))))

          ((not (and code state))
           (clack-error-response "Missing code or state parameter" :status 400))

          (t
           ;; Process OAuth callback
           (let ((result (handle-auth0-callback-internal code state)))
             (if (and result (cdr (assoc :session-id result)))
                 ;; Success - redirect to frontend with session cookie
                 (let ((session-id (cdr (assoc :session-id result))))
                   (list 302
                         (list :location "/"
                               :set-cookie (format nil "session=~A; Path=/~A; SameSite=Lax"
                                                  session-id
                                                  (if *use-secure-cookies* "; Secure" ""))
                               :access-control-allow-origin *cors-origin*
                               :access-control-allow-credentials "true")
                         '("")))
                 ;; Error case
                 (list 302
                       (list :location "/?error=authentication_failed"
                             :access-control-allow-origin *cors-origin*)
                       '("")))))))
    (error (e)
      (format t "[ERROR] Auth0 callback error: ~A~%" e)
      (list 302
            (list :location (format nil "/?error=server_error&error_description=~A"
                                   (quri:url-encode (format nil "~A" e)))
                  :access-control-allow-origin *cors-origin*)
            '("")))))

(defun handle-auth0-link-clack (env)
  "Auth0 account linking handler - Clack format.
   Links existing user account with Auth0 OAuth account.
   Rate limited to prevent abuse."
  ;; Check rate limit first
  (let ((client-ip (get-client-ip-from-env env)))
    (unless (check-rate-limit client-ip)
      (format t "[WARN] Rate limit exceeded for IP: ~A on /auth0/link~%" client-ip)
      (return-from handle-auth0-link-clack
        (clack-error-response "Too many requests. Please try again later." :status 429))))

  (handler-case
      (let* ((session-id (or (get-env-cookie env "session")
                            (get-env-header env :x-session-id)))
             (query-string (getf env :query-string))
             (params (when query-string
                      (mapcar (lambda (pair)
                                (let ((kv (cl-ppcre:split "=" pair)))
                                  (cons (intern (string-upcase (first kv)) :keyword)
                                        (second kv))))
                              (cl-ppcre:split "&" query-string))))
             (code (cdr (assoc :code params)))
             (state (cdr (assoc :state params))))

        (unless session-id
          (return-from handle-auth0-link-clack
            (clack-error-response "Not authenticated" :status 401)))

        (unless (and code state)
          (return-from handle-auth0-link-clack
            (clack-error-response "Missing code or state parameter" :status 400)))

        (let ((result (handle-auth0-link-internal session-id code state)))
          (if (cdr (assoc :success result))
              (clack-success-response result)
              (clack-error-response (cdr (assoc :error result)) :status 400))))
    (error (e)
      (format t "[ERROR] Auth0 link error: ~A~%" e)
      (clack-error-response (format nil "Link error: ~A" e) :status 500))))

(defun handle-auth-metrics-clack (env)
  "Auth metrics endpoint handler - Clack format.
   Returns authentication metrics and statistics."
  (declare (ignore env))
  (handler-case
      (let ((metrics (get-auth-metrics)))
        (clack-json-response metrics))
    (error (e)
      (format t "[ERROR] Auth metrics error: ~A~%" e)
      (clack-error-response (format nil "Metrics error: ~A" e) :status 500))))
