;;;; app.lisp - Clack application builder for CollabCanvas
;;;; This file creates the Clack application using Lack middleware

(in-package #:collabcanvas)

;;; Clack Environment Helper Functions

(defun get-env-header (env header-name)
  "Get a header from Clack environment.
   header-name should be a keyword like :x-session-id"
  (getf env (intern (format nil "HTTP-~A" (string-upcase header-name)) :keyword)))

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
      (let ((body-string (make-string content-length)))
        (read-sequence body-string input-stream)
        (handler-case
            (jonathan:parse body-string :as :alist)
          (error (e)
            (format t "[ERROR] Failed to parse JSON body: ~A~%" e)
            nil))))))

(defun clack-json-response (data &key (status 200))
  "Return Clack response with JSON data.
   Returns (status headers body-list) format."
  (list status
        '(:content-type "application/json")
        (list (jonathan:to-json data))))

(defun clack-error-response (message &key (status 400))
  "Return Clack error response"
  (clack-json-response `((:error . ,message)) :status status))

(defun clack-success-response (data)
  "Return Clack success response"
  (clack-json-response `((:success . t) (:data . ,data))))

(defun handle-websocket-upgrade (env)
  "Handle WebSocket upgrade request.
   Extracts canvas-id from path, creates connection, returns 101 Switching Protocols."
  (let* ((path-info (getf env :path-info))
         (canvas-id (when (cl-ppcre:scan "^/ws/" path-info)
                     (third (cl-ppcre:split "/" path-info)))))

    (unless canvas-id
      (return-from handle-websocket-upgrade
        (clack-error-response "Invalid WebSocket path - expected /ws/<canvas-id>" :status 400)))

    ;; Create WebSocket connection
    (let* ((conn-id (format nil "ws-~A-~A" canvas-id (get-universal-time)))
           (ws (getf env :websocket)))

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

        ;; Return 101 Switching Protocols
        (list 101 '() '())))))

(defun make-app ()
  "Build and return the Clack application with middleware stack.
   Uses Lack middleware for CORS, static files, and request handling."
  (lack:builder
   ;; CORS middleware - allow cross-origin requests
   (:cors
    :allow-origin *cors-origin*
    :allow-methods '(:GET :POST :PUT :DELETE :OPTIONS)
    :allow-headers '("Content-Type" "Authorization" "X-Session-ID")
    :allow-credentials t)

   ;; Static file serving middleware (for potential future use)
   ;; :static :path "/static/" :root "./public/"

   ;; Main application handler
   (lambda (env)
     (let ((request-method (getf env :request-method))
           (path-info (getf env :path-info)))

       ;; Check for WebSocket upgrade request
       (when (and (cl-ppcre:scan "^/ws/" path-info)
                  (getf env :websocket))
         (return-from nil (handle-websocket-upgrade env)))

       ;; Route requests to appropriate handlers
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
                (list (jonathan:to-json
                       '(:|error| "Not Found"))))))))))

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
  (let ((session-id (or (get-env-cookie env "session")
                        (get-env-header env :authorization))))
    (if-let ((session (validate-session session-id)))
      (clack-success-response session)
      (clack-error-response "Invalid or expired session" :status 401))))

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
