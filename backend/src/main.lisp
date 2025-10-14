;;;; main.lisp - Main entry point and server setup for CollabCanvas

(in-package #:collabcanvas)

(defparameter *server* nil "HTTP server instance")
(defparameter *websocket-acceptor* nil "WebSocket acceptor instance")

;;; HTTP Request Handlers Setup
(defun setup-routes ()
  "Setup HTTP routes"
  ;; Health check
  (hunchentoot:define-easy-handler (health :uri "/health") ()
    (set-cors-headers)
    (json-response `((:status . "healthy")
                    (:service . "collabcanvas")
                    (:timestamp . ,(current-timestamp)))))

  ;; Test endpoint
  (hunchentoot:define-easy-handler (test-post :uri "/api/test"
                                              :default-request-type :post) ()
    (setf (hunchentoot:content-type*) "application/json")
    (format t "TEST ENDPOINT CALLED~%")
    "{\"test\":\"success\"}")

  ;; Authentication routes - check request method internally
  (hunchentoot:define-easy-handler (register-handler :uri "/api/register") ()
    (set-cors-headers)
    (if (eq (hunchentoot:request-method*) :options)
        (progn
          (setf (hunchentoot:return-code*) 200)
          "")
        (progn
          (format t "REGISTER HANDLER CALLED~%")
          (setf (hunchentoot:content-type*) "application/json")
          (let ((result (handle-register)))
            (format t "REGISTER RESULT: ~A~%" result)
            result))))

  (hunchentoot:define-easy-handler (login-handler :uri "/api/login") ()
    (set-cors-headers)
    (if (eq (hunchentoot:request-method*) :options)
        (progn
          (setf (hunchentoot:return-code*) 200)
          "")
        (progn
          (format t "LOGIN HANDLER CALLED~%")
          (setf (hunchentoot:content-type*) "application/json")
          (let ((result (handle-login)))
            (format t "LOGIN RESULT: ~A~%" result)
            result))))

  (hunchentoot:define-easy-handler (logout-handler :uri "/api/logout") ()
    (set-cors-headers)
    (if (eq (hunchentoot:request-method*) :options)
        (progn
          (setf (hunchentoot:return-code*) 200)
          "")
        (progn
          (setf (hunchentoot:content-type*) "application/json")
          (handle-logout))))

  (hunchentoot:define-easy-handler (session-handler :uri "/api/session") ()
    (set-cors-headers)
    (if (eq (hunchentoot:request-method*) :options)
        (progn
          (setf (hunchentoot:return-code*) 200)
          "")
        (progn
          (setf (hunchentoot:content-type*) "application/json")
          (handle-session-check))))

  ;; Canvas state routes
  (hunchentoot:define-easy-handler (canvas-state-handler :uri "/api/canvas/state") ()
    (set-cors-headers)
    (cond
      ((eq (hunchentoot:request-method*) :options)
       (setf (hunchentoot:return-code*) 200)
       "")
      ((eq (hunchentoot:request-method*) :get)
       (handle-get-canvas-state))
      ((eq (hunchentoot:request-method*) :post)
       (handle-save-canvas-state))
      (t
       (error-response "Method not allowed" :status 405)))))

;;; WebSocket Setup
(defclass canvas-acceptor (hunchensocket:websocket-acceptor
                           hunchentoot:easy-acceptor)
  ()
  (:documentation "Custom acceptor that combines WebSocket and Easy acceptor"))

(defun setup-websocket-dispatch ()
  "Setup WebSocket dispatch table"
  (pushnew 'dispatch-websocket-request
           hunchensocket:*websocket-dispatch-table*))

(defun dispatch-websocket-request (request)
  "Dispatch WebSocket requests to appropriate canvas rooms"
  (let ((uri (hunchentoot:script-name request)))
    (when (cl-ppcre:scan "^/ws/" uri)
      ;; Extract canvas ID from URI like /ws/canvas-abc123
      (let ((canvas-id (third (cl-ppcre:split "/" uri))))
        (when (and canvas-id (valid-canvas-id-p canvas-id))
          (make-instance 'canvas-websocket-resource
                        :canvas-id canvas-id))))))

;;; Server Lifecycle Management
(defun start-server (&key (port *port*) (host *host*))
  "Start the CollabCanvas server"
  (format t "~%=== Starting CollabCanvas Server ===~%")

  ;; Initialize database
  (format t "Initializing database...~%")
  (handler-case
      (progn
        (init-database)
        ;; Initialize connection pool
        (format t "Initializing connection pool (~A connections)...~%" *database-pool-size*)
        (init-database-pool))
    (error (e)
      (format t "Database initialization error: ~A~%" e)
      (return-from start-server nil)))

  ;; Setup routes
  (format t "Setting up HTTP routes...~%")
  (setup-routes)

  ;; Create and start the acceptor (combines HTTP and WebSocket)
  (format t "Starting server on ~A:~A...~%" host port)
  (format t "Frontend path: ~A~%" *frontend-path*)
  (setf *websocket-acceptor*
        (make-instance 'canvas-acceptor
                      :port port
                      :address host
                      :access-log-destination nil
                      :message-log-destination (if *debug-mode*
                                                 *standard-output*
                                                 nil)
                      :document-root (namestring *frontend-path*)))

  ;; Setup static file dispatcher for frontend
  (push (hunchentoot:create-static-file-dispatcher-and-handler
         "/" (merge-pathnames "index.html" *frontend-path*))
        hunchentoot:*dispatch-table*)

  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/assets/" (merge-pathnames "assets/" *frontend-path*))
        hunchentoot:*dispatch-table*)

  ;; Setup WebSocket dispatch
  (setup-websocket-dispatch)

  ;; Start the acceptor
  (hunchentoot:start *websocket-acceptor*)
  (setf *server* *websocket-acceptor*)

  (format t "~%✓ CollabCanvas server started successfully!~%")
  (format t "  HTTP API: http://~A:~A/api~%" host port)
  (format t "  WebSocket: ws://~A:~A/ws/<canvas-id>~%" host port)
  (format t "  Health: http://~A:~A/health~%" host port)
  (format t "  Frontend: http://~A:~A/~%" host port)
  (format t "~%Press Ctrl+C to stop the server~%~%")

  t)

(defun stop-server ()
  "Stop the CollabCanvas server"
  (format t "~%Stopping CollabCanvas server...~%")

  (when *server*
    (handler-case
        (progn
          ;; Persist all dirty canvas states
          (format t "Saving canvas states...~%")
          (maphash (lambda (canvas-id state)
                    (declare (ignore state))
                    (persist-canvas-state canvas-id))
                  *canvas-states*)

          ;; Stop the server
          (hunchentoot:stop *server*)
          (setf *server* nil)
          (setf *websocket-acceptor* nil))
      (error (e)
        (format t "Error stopping server: ~A~%" e))))

  ;; Close connection pool
  (format t "Closing database connection pool...~%")
  (close-database-pool)

  ;; Disconnect database (legacy compatibility)
  (disconnect-database)

  (format t "Server stopped.~%"))

(defun restart-server ()
  "Restart the server"
  (stop-server)
  (sleep 1)
  (start-server))

;;; Main entry point for standalone execution
(defun main ()
  "Main entry point for the application"
  (handler-case
      (progn
        (format t "~%CollabCanvas - Real-time Collaborative Design Tool~%")
        (format t "Version 0.1.0~%")

        ;; Parse command line arguments if needed
        (let ((args #+sbcl (cdr sb-ext:*posix-argv*)
                   #+ccl (cdr ccl:*command-line-argument-list*)
                   #-(or sbcl ccl) nil))
          (declare (ignore args))

          ;; Start the server
          (unless (start-server)
            (format t "Failed to start server~%")
            (return-from main 1))

          ;; Keep the server running
          (loop
            (sleep 1)
            ;; Periodic cleanup tasks
            (when (zerop (mod (get-universal-time) 3600))
              (cleanup-expired-sessions))))

        ;; Return success
        0)

    ;; Handle interrupts (Ctrl+C)
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+lispworks mp:process-interrupt
     ()
      (format t "~%~%Interrupt received, shutting down gracefully...~%")
      (stop-server)
      0)

    ;; Handle other errors
    (error (e)
      (format t "~%Fatal error: ~A~%" e)
      (stop-server)
      1)))

;;; Export public interface
(export '(start-server stop-server restart-server main))