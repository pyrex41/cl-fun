;; src/main.lisp
;; Complete working implementation for CollabCanvas backend
(in-package #:collabcanvas)

(defvar *server* nil "HTTP server instance")
(defvar *websocket-acceptor* nil "WebSocket acceptor instance")

;;; CORS Middleware
(defun add-cors-headers ()
  "Add CORS headers to response"
  (setf (hunchentoot:header-out :access-control-allow-origin) 
        collabcanvas.config:*cors-origin*)
  (setf (hunchentoot:header-out :access-control-allow-methods) 
        "GET, POST, PUT, DELETE, OPTIONS")
  (setf (hunchentoot:header-out :access-control-allow-headers) 
        "Content-Type, Authorization, X-Session-ID")
  (setf (hunchentoot:header-out :access-control-allow-credentials) "true"))

;;; HTTP API Endpoints

(hunchentoot:define-easy-handler (health :uri "/health") ()
  (setf (hunchentoot:content-type*) "application/json")
  (jonathan:to-json '(:|status| "ok" :|service| "collabcanvas")))

(hunchentoot:define-easy-handler (api-register :uri "/api/register") ()
  (add-cors-headers)
  (setf (hunchentoot:content-type*) "application/json")
  (handler-case
      (let* ((body (hunchentoot:raw-post-data :force-text t))
             (data (jonathan:parse body :as :hash-table))
             (email (gethash "email" data))
             (password (gethash "password" data))
             (username (gethash "username" data)))
        (if (and email password username)
            (let ((result (collabcanvas.auth:register-user email password username)))
              (jonathan:to-json result :from :alist))
            (jonathan:to-json '((:|success| . nil) 
                               (:|message| . "Missing required fields")))))
    (error (e)
      (jonathan:to-json `((:|success| . nil) 
                         (:|message| . ,(format nil "Error: ~A" e)))))))

(hunchentoot:define-easy-handler (api-login :uri "/api/login") ()
  (add-cors-headers)
  (setf (hunchentoot:content-type*) "application/json")
  (handler-case
      (let* ((body (hunchentoot:raw-post-data :force-text t))
             (data (jonathan:parse body :as :hash-table))
             (email (gethash "email" data))
             (password (gethash "password" data)))
        (if (and email password)
            (let ((result (collabcanvas.auth:login-user email password)))
              (jonathan:to-json result :from :alist))
            (jonathan:to-json '((:|success| . nil) 
                               (:|message| . "Missing email or password")))))
    (error (e)
      (jonathan:to-json `((:|success| . nil) 
                         (:|message| . ,(format nil "Error: ~A" e)))))))

(hunchentoot:define-easy-handler (api-logout :uri "/api/logout") ()
  (add-cors-headers)
  (setf (hunchentoot:content-type*) "application/json")
  (handler-case
      (let* ((body (hunchentoot:raw-post-data :force-text t))
             (data (jonathan:parse body :as :hash-table))
             (session-id (gethash "sessionId" data)))
        (if session-id
            (let ((result (collabcanvas.auth:logout-user session-id)))
              (jonathan:to-json result :from :alist))
            (jonathan:to-json '((:|success| . nil) 
                               (:|message| . "Missing sessionId")))))
    (error (e)
      (jonathan:to-json `((:|success| . nil) 
                         (:|message| . ,(format nil "Error: ~A" e)))))))

(hunchentoot:define-easy-handler (api-canvas-state :uri "/api/canvas/state") (canvasId)
  (add-cors-headers)
  (setf (hunchentoot:content-type*) "application/json")
  (handler-case
      (if canvasId
          (let ((state (collabcanvas.database:load-canvas-state canvasId)))
            (if state
                state  ; Already JSON string
                (jonathan:to-json '((:|objects| . #())))))
          (jonathan:to-json '((:|error| . "Missing canvasId"))))
    (error (e)
      (jonathan:to-json `((:|error| . ,(format nil "Error: ~A" e)))))))

;;; OPTIONS handler for CORS preflight
(hunchentoot:define-easy-handler (options-handler :uri "/api/*") ()
  (add-cors-headers)
  (setf (hunchentoot:return-code*) 200)
  "")

;;; WebSocket Dispatch
(defun websocket-dispatch (request)
  "Dispatch WebSocket requests to appropriate canvas rooms"
  (let ((uri (hunchentoot:request-uri request)))
    (when (cl-ppcre:scan "^/ws/" uri)
      (let ((canvas-id (second (cl-ppcre:split "/" uri))))
        (when canvas-id
          (format t "Creating/joining room for canvas: ~A~%" canvas-id)
          (collabcanvas.websocket:get-or-create-room canvas-id))))))

;;; Server Lifecycle

(defun ensure-data-directory ()
  "Ensure data directory exists"
  (ensure-directories-exist 
   (merge-pathnames "data/" 
                   (asdf:system-source-directory :collabcanvas))))

(defun start (&key (port collabcanvas.config:*port*))
  "Start the CollabCanvas server"
  (format t "~%=== Starting CollabCanvas ===~%")
  
  ;; Ensure data directory exists
  (ensure-data-directory)
  
  ;; Initialize database
  (format t "Initializing database...~%")
  (collabcanvas.database:init-db)
  
  ;; Start HTTP server
  (format t "Starting HTTP server on port ~A...~%" port)
  (setf *server* (make-instance 'hunchentoot:easy-acceptor
                               :port port
                               :access-log-destination nil
                               :message-log-destination t))
  (hunchentoot:start *server*)
  
  ;; Start WebSocket server  
  (format t "Starting WebSocket server...~%")
  (setf *websocket-acceptor* 
        (make-instance 'hunchensocket:websocket-acceptor
                      :port port))
  
  ;; Set WebSocket dispatch
  (push (lambda (request)
          (websocket-dispatch request))
        (hunchensocket:websocket-dispatch-table *websocket-acceptor*))
  
  (hunchentoot:start *websocket-acceptor*)
  
  (format t "~%✓ CollabCanvas server running!~%")
  (format t "  HTTP API: http://localhost:~A~%" port)
  (format t "  WebSocket: ws://localhost:~A/ws/<canvas-id>~%" port)
  (format t "  Health check: http://localhost:~A/health~%" port)
  (format t "~%Press Ctrl+C to stop~%~%"))

(defun stop ()
  "Stop the CollabCanvas server"
  (format t "~%Stopping CollabCanvas server...~%")
  
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil))
  
  (when *websocket-acceptor*
    (hunchentoot:stop *websocket-acceptor*)
    (setf *websocket-acceptor* nil))
  
  (collabcanvas.database:close-db)
  
  (format t "Server stopped.~%"))

(defun restart ()
  "Restart the server"
  (stop)
  (sleep 1)
  (start))

(defun main ()
  "Main entry point for standalone binary"
  (handler-case
      (progn
        (start)
        ;; Keep running
        (loop (sleep 1)))
    ;; Handle Ctrl+C gracefully
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     ()
      (format t "~%Received interrupt, shutting down...~%")
      (stop)
      (quit))))

;; Export main for use in scripts
(export 'main)
