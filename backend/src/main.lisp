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

  ;; Authentication routes
  (hunchentoot:define-easy-handler (register :uri "/api/register"
                                           :default-request-type :post) ()
    (handle-register))

  (hunchentoot:define-easy-handler (login :uri "/api/login"
                                         :default-request-type :post) ()
    (handle-login))

  (hunchentoot:define-easy-handler (logout :uri "/api/logout"
                                          :default-request-type :post) ()
    (handle-logout))

  (hunchentoot:define-easy-handler (session :uri "/api/session"
                                           :default-request-type :get) ()
    (handle-session-check))

  ;; Canvas state routes
  (hunchentoot:define-easy-handler (get-canvas :uri "/api/canvas/state"
                                              :default-request-type :get) ()
    (handle-get-canvas-state))

  (hunchentoot:define-easy-handler (save-canvas :uri "/api/canvas/state"
                                               :default-request-type :post) ()
    (handle-save-canvas-state))

  ;; OPTIONS handler for CORS preflight
  (hunchentoot:define-easy-handler (options :uri "/api/*"
                                           :default-request-type :options) ()
    (set-cors-headers)
    ""))

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
      (init-database)
    (error (e)
      (format t "Database initialization error: ~A~%" e)
      (return-from start-server nil)))

  ;; Setup routes
  (format t "Setting up HTTP routes...~%")
  (setup-routes)

  ;; Create and start the acceptor (combines HTTP and WebSocket)
  (format t "Starting server on ~A:~A...~%" host port)
  (setf *websocket-acceptor*
        (make-instance 'canvas-acceptor
                      :port port
                      :address host
                      :access-log-destination nil
                      :message-log-destination (if *debug-mode*
                                                 *standard-output*
                                                 nil)))

  ;; Setup WebSocket dispatch
  (setup-websocket-dispatch)

  ;; Start the acceptor
  (hunchentoot:start *websocket-acceptor*)
  (setf *server* *websocket-acceptor*)

  (format t "~%✓ CollabCanvas server started successfully!~%")
  (format t "  HTTP API: http://~A:~A/api~%" host port)
  (format t "  WebSocket: ws://~A:~A/ws/<canvas-id>~%" host port)
  (format t "  Health: http://~A:~A/health~%" host port)
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

  ;; Disconnect database
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