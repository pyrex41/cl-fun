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
  (let ((cookie-header (getf env :http-cookie)))
    (when cookie-header
      (let ((cookies (cl-ppcre:split "; ?" cookie-header)))
        (dolist (cookie cookies)
          (let ((parts (cl-ppcre:split "=" cookie)))
            (when (string= (first parts) cookie-name)
              (return-from get-env-cookie (second parts)))))))))

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
             ((string= path-info "/api/register")
              (handle-register env))

             ((string= path-info "/api/login")
              (handle-login env))

             ((string= path-info "/api/logout")
              (handle-logout env))

             ((string= path-info "/api/session")
              (handle-session-check env))

             ((cl-ppcre:scan "^/api/canvas/state" path-info)
              (handle-canvas-state env))

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

(defun handle-register (env)
  "User registration endpoint handler - Clack format.
   Expects JSON body with :email, :username, :password"
  (let ((data (parse-env-body env)))
    (unless data
      (return-from handle-register
        (clack-error-response "Invalid request body")))

    (let ((email (cdr (assoc :email data)))
          (username (cdr (assoc :username data)))
          (password (cdr (assoc :password data))))

      (unless (and email username password)
        (return-from handle-register
          (clack-error-response "Email, username, and password are required")))

      (handler-case
          (let ((result (register-user email username password)))
            (clack-success-response result))
        (error (e)
          (clack-error-response (format nil "~A" e)))))))

(defun handle-login (env)
  "User login endpoint handler - Clack format.
   Expects JSON body with :email/:username and :password"
  (let ((data (parse-env-body env)))
    (unless data
      (return-from handle-login
        (clack-error-response "Invalid request body")))

    (let ((email-or-username (or (cdr (assoc :email data))
                                 (cdr (assoc :username data))))
          (password (cdr (assoc :password data))))

      (unless (and email-or-username password)
        (return-from handle-login
          (clack-error-response "Email/username and password are required")))

      (handler-case
          (let ((result (login-user email-or-username password)))
            ;; TODO: Set session cookie when we implement cookie support
            (clack-success-response result))
        (error (e)
          (clack-error-response (format nil "~A" e)))))))

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
