;;;; server.lisp - Server lifecycle management for CollabCanvas with Woo
;;;; This file provides start/stop/restart functions for the Woo web server

(in-package #:collabcanvas)

;;; Server State

(defvar *woo-server* nil
  "Woo server instance handle. Set when server is running, nil when stopped.")

;;; Server Lifecycle Functions

(defun start-server (&key (port *port*)
                           (address "0.0.0.0")
                           (debug nil)
                           (worker-num 1)
                           (backlog 128))
  "Start the CollabCanvas server using Woo.

   Parameters:
   - port: Port number to listen on (default: from config)
   - address: Address to bind to (default: 0.0.0.0 for all interfaces)
   - debug: Enable debug mode (default: nil for production)
   - worker-num: Number of worker threads (default: 1 for SQLite)
   - backlog: OS socket backlog size (default: 4096)

   Note: worker-num is set to 1 by default because SQLite requires
   single-writer access. For multi-worker support, migrate to PostgreSQL."

  (when *woo-server*
    (error "Server is already running. Stop it first with (stop-server)."))

  (format t "~%=== Starting CollabCanvas with Woo ===~%")

  ;; Ensure data directory exists
  (ensure-directories-exist
   (merge-pathnames "data/"
                    (asdf:system-source-directory :collabcanvas)))

  ;; Initialize database
  (format t "Initializing database...~%")
  (init-db)

  ;; Build Clack application
  (format t "Building Clack application...~%")
  (let ((app (make-app)))

    ;; Start Woo server with Clackup
    (format t "Starting Woo server on ~A:~A...~%" address port)
    (format t "  Debug mode: ~A~%" debug)
    (format t "  Worker threads: ~A~%" worker-num)
    (format t "  Socket backlog: ~A~%~%" backlog)

    (setf *woo-server*
          (clack:clackup app
                         :server :woo
                         :address address
                         :port port
                         :debug debug
                         :worker-num worker-num
                         :backlog backlog
                         :use-thread nil))  ; Pure async event-driven

    (format t "✓ CollabCanvas server running!~%")
    (format t "  HTTP API: http://~A:~A~%"
            (if (string= address "0.0.0.0") "localhost" address)
            port)
    (format t "  WebSocket: ws://~A:~A/ws/<canvas-id>~%"
            (if (string= address "0.0.0.0") "localhost" address)
            port)
    (format t "  Health check: http://~A:~A/health~%"
            (if (string= address "0.0.0.0") "localhost" address)
            port)
    (format t "~%Press Ctrl+C to stop~%~%")

    ;; Start connection stats monitoring
    (start-stats-logging)

    ;; Initialize and start physics system
    (initialize-physics-system)

    *woo-server*))

(defun stop-server ()
  "Stop the CollabCanvas server.
   Gracefully shuts down the Woo server and closes database connections."

  (unless *woo-server*
    (format t "Server is not running.~%")
    (return-from stop-server nil))

  (format t "~%Stopping CollabCanvas server...~%")

  ;; Stop physics system
  (shutdown-physics-system)

  ;; Stop stats monitoring
  (stop-stats-logging)

  ;; Stop Woo server
  (when *woo-server*
    (clack:stop *woo-server*)
    (setf *woo-server* nil))

  ;; Close database
  (close-db)

  (format t "Server stopped.~%")
  t)

(defun restart-server ()
  "Restart the CollabCanvas server.
   Stops the server if running, waits briefly, then starts it again."
  (format t "Restarting server...~%")
  (stop-server)
  (sleep 1)
  (start-server))

(defun server-running-p ()
  "Check if the server is currently running.
   Returns T if server is running, NIL otherwise."
  (not (null *woo-server*)))

;;; Main Entry Point (for standalone binary or REPL)

(defun main ()
  "Main entry point for standalone binary.
   Starts the server and handles graceful shutdown on interrupt signals."
  (handler-case
      (progn
        (start-server)
        ;; Keep running indefinitely
        (loop (sleep 1)))

    ;; Handle Ctrl+C gracefully
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     ()
      (format t "~%Received interrupt, shutting down...~%")
      (stop-server)
      #+sbcl (sb-ext:quit)
      #+ccl (ccl:quit)
      #+clisp (ext:quit)
      #+ecl (ext:quit))))

;; Export main for use in scripts
(export 'main :collabcanvas)
