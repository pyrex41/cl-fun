;;;; main.lisp - Main entry point for CollabCanvas with Woo
;;;; This file provides the standalone entry point that uses server.lisp

(in-package #:collabcanvas)

;;; Main Entry Point

(defun main ()
  "Main entry point for standalone binary.
   Starts the server and handles graceful shutdown on interrupt signals."
  (handler-case
      (progn
        (format t "~%CollabCanvas - Real-time Collaborative Design Tool~%")
        (format t "Version 0.2.0 (Woo Edition)~%~%")

        ;; Start the Woo server (from server.lisp)
        (start-server)

        ;; Keep running indefinitely with periodic cleanup
        (loop
          (sleep 60)
          ;; Periodic cleanup tasks
          (cleanup-expired-sessions)))

    ;; Handle Ctrl+C gracefully
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     ()
      (format t "~%Received interrupt, shutting down gracefully...~%")
      (stop-server)
      #+sbcl (sb-ext:quit :unix-status 0)
      #+ccl (ccl:quit 0)
      #+clisp (ext:quit 0)
      #+ecl (ext:quit 0))

    ;; Handle other errors
    (error (e)
      (format t "~%Fatal error: ~A~%" e)
      (stop-server)
      #+sbcl (sb-ext:quit :unix-status 1)
      #+ccl (ccl:quit 1)
      #+clisp (ext:quit 1)
      #+ecl (ext:quit 1))))

;; Export main for use in scripts
(export 'main :collabcanvas)
