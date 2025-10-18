;;;; run-load-test.lisp - Script to execute load tests
;;;;
;;;; Usage: ros run-load-test.lisp
;;;;
;;;; This script:
;;;;   1. Loads the CollabCanvas system
;;;;   2. Loads the load-test module
;;;;   3. Runs all test scenarios
;;;;   4. Generates report

(ql:quickload :collabcanvas :silent t)

;; Load test module
(load "tests/load-test.lisp")

;; Run all tests
(in-package :collabcanvas)

(format t "~%Starting CollabCanvas Load Test Suite...~%~%")

;; Ensure database is initialized
(handler-case
    (progn
      (init-database)
      (format t "[INIT] Database initialized~%"))
  (error (e)
    (format t "[WARN] Database init error (may already exist): ~A~%" e)))

;; Run tests
(handler-case
    (progn
      (run-all-load-tests)
      (format t "~%[SUCCESS] All load tests completed successfully!~%~%"))
  (error (e)
    (format t "~%[ERROR] Load test failed: ~A~%" e)
    (format t "Backtrace: ~A~%" (trivial-backtrace:print-backtrace e :output nil))))

;; Exit
(sb-ext:quit)
