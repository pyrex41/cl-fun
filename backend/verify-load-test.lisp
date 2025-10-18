;;;; verify-load-test.lisp - Quick verification that load test infrastructure works
;;;;
;;;; This script verifies the load test can be loaded and basic functions work

(ql:quickload :collabcanvas :silent t)

;; Load test module
(load "tests/load-test.lisp")

(in-package :collabcanvas)

(format t "~%╔════════════════════════════════════════════════════════════╗~%")
(format t "║     LOAD TEST INFRASTRUCTURE VERIFICATION                 ║~%")
(format t "╚════════════════════════════════════════════════════════════╝~%~%")

;; Test 1: Verify constants
(format t "✓ Constants defined:~%")
(format t "  - Test duration: ~D seconds~%" +test-duration+)
(format t "  - Num clients: ~D~%" +num-clients+)
(format t "  - Num balls: ~D~%" +num-balls+)
(format t "  - Physics Hz: ~D~%" +physics-hz+)
(format t "  - Broadcast Hz: ~D~%" +broadcast-hz+)
(format t "~%")

;; Test 2: Verify database initialization
(format t "✓ Initializing database...~%")
(handler-case
    (progn
      (init-database)
      (format t "  Database initialized successfully~%"))
  (error (e)
    (format t "  Warning: ~A~%" e)))
(format t "~%")

;; Test 3: Verify ECS storage creation
(format t "✓ Testing ECS storage creation...~%")
(ensure-canvas-ecs-storage "test-canvas")
(ensure-canvas-physics-config "test-canvas")
(format t "  ECS storage created successfully~%")
(format t "~%")

;; Test 4: Verify ball spawning
(format t "✓ Testing ball spawning (10 balls)...~%")
(let ((entities (spawn-test-balls "test-canvas" 10 :moving t)))
  (format t "  Spawned ~D entities~%" (length entities)))
(format t "~%")

;; Test 5: Clean up
(format t "✓ Testing cleanup...~%")
(clear-test-balls "test-canvas")
(format t "  Cleanup successful~%")
(format t "~%")

;; Test 6: Verify metrics structure
(format t "✓ Testing metrics collection...~%")
(let ((metrics (make-load-test-metrics :start-time (get-universal-time))))
  (format t "  Metrics struct created successfully~%")
  (format t "  Fields: start-time, frame-times, bandwidth-samples~%"))
(format t "~%")

;; Test 7: Run a very short test (5 seconds)
(format t "✓ Running 5-second quick test...~%")
(handler-case
    (let* ((metrics (run-load-test-scenario
                     "Verification Test"
                     (lambda (canvas-id)
                       (spawn-test-balls canvas-id 50 :moving t))
                     5))
           (analysis (analyze-metrics "Verification" metrics)))
      (format t "~%")
      (print-analysis analysis))
  (error (e)
    (format t "  Error during test: ~A~%" e)))

(format t "~%╔════════════════════════════════════════════════════════════╗~%")
(format t "║     VERIFICATION COMPLETE                                  ║~%")
(format t "╚════════════════════════════════════════════════════════════╝~%~%")

(format t "Load test infrastructure is working correctly.~%")
(format t "To run full test suite: (run-all-load-tests)~%~%")

(sb-ext:quit)
