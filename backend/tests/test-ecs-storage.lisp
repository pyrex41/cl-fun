;;;; test-ecs-storage.lisp - Unit tests for ECS Storage Management
;;;;
;;;; Tests the core functions: init-canvas-physics, get-canvas-ecs-storage,
;;;; destroy-canvas-physics, and hash table operations.

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-fast-ecs :silent t))

;;; Load the collabcanvas system
(load (merge-pathnames "backend/collabcanvas.asd"
                       (asdf:system-source-directory :collabcanvas)))

(asdf:load-system :collabcanvas :force t)

(defpackage :collabcanvas/tests/ecs-storage
  (:use :cl)
  (:import-from :collabcanvas
                #:init-canvas-physics
                #:get-canvas-ecs-storage
                #:get-canvas-physics-config
                #:destroy-canvas-physics
                #:list-active-physics-canvases
                #:canvas-physics-active-p
                #:update-canvas-physics-settings
                #:cleanup-all-physics-canvases
                #:physics-canvas-config-gravity-x
                #:physics-canvas-config-gravity-y
                #:physics-canvas-config-simulation-rate
                #:*physics-canvases*)
  (:export #:run-tests))

(in-package :collabcanvas/tests/ecs-storage)

;;; ============================================================================
;;; Test Utilities
;;; ============================================================================

(defvar *test-count* 0)
(defvar *test-passed* 0)
(defvar *test-failed* 0)

(defmacro test (name &body body)
  "Run a test case and track results."
  `(progn
     (incf *test-count*)
     (format t "~%TEST: ~A~%" ,name)
     (handler-case
         (progn
           ,@body
           (incf *test-passed*)
           (format t "  ✓ PASS~%"))
       (error (e)
         (incf *test-failed*)
         (format t "  ✗ FAIL: ~A~%" e)))))

(defmacro assert-equal (expected actual &optional message)
  "Assert that expected equals actual."
  `(let ((exp ,expected)
         (act ,actual))
     (unless (equal exp act)
       (error "~@[~A~%~]Expected: ~A~%Got: ~A"
              ,message exp act))))

(defmacro assert-true (form &optional message)
  "Assert that form evaluates to true."
  `(unless ,form
     (error "~@[~A~%~]Expected true, got NIL" ,message)))

(defmacro assert-nil (form &optional message)
  "Assert that form evaluates to nil."
  `(when ,form
     (error "~@[~A~%~]Expected NIL, got ~A" ,message ,form)))

;;; ============================================================================
;;; Test Cases
;;; ============================================================================

(defun test-basic-initialization ()
  "Test basic canvas physics initialization."
  (test "Basic initialization with defaults"
    (cleanup-all-physics-canvases)

    (let ((storage (init-canvas-physics "test-canvas-1")))
      (assert-true storage "Storage should be created")
      (assert-true (typep storage 'cl-fast-ecs:storage)
                   "Storage should be of type cl-fast-ecs:storage"))))

(defun test-retrieve-storage ()
  "Test retrieving ECS storage."
  (test "Retrieve existing storage"
    (cleanup-all-physics-canvases)

    (let ((storage1 (init-canvas-physics "test-canvas-2")))
      (let ((storage2 (get-canvas-ecs-storage "test-canvas-2")))
        (assert-equal storage1 storage2 "Retrieved storage should match original"))))

  (test "Retrieve non-existent storage"
    (let ((storage (get-canvas-ecs-storage "nonexistent-canvas")))
      (assert-nil storage "Non-existent canvas should return NIL"))))

(defun test-configuration ()
  "Test configuration retrieval and settings."
  (test "Default configuration values"
    (cleanup-all-physics-canvases)

    (init-canvas-physics "test-canvas-3")
    (let ((config (get-canvas-physics-config "test-canvas-3")))
      (assert-true config "Config should exist")
      (assert-equal 0.0 (physics-canvas-config-gravity-x config)
                    "Default gravity-x should be 0.0")
      (assert-equal 9.8 (physics-canvas-config-gravity-y config)
                    "Default gravity-y should be 9.8")
      (assert-equal 60 (physics-canvas-config-simulation-rate config)
                    "Default simulation rate should be 60 Hz")))

  (test "Custom configuration values"
    (cleanup-all-physics-canvases)

    (init-canvas-physics "test-canvas-4"
                         :gravity-x 5.0
                         :gravity-y 19.6
                         :simulation-rate 120)
    (let ((config (get-canvas-physics-config "test-canvas-4")))
      (assert-equal 5.0 (physics-canvas-config-gravity-x config)
                    "Custom gravity-x should be 5.0")
      (assert-equal 19.6 (physics-canvas-config-gravity-y config)
                    "Custom gravity-y should be 19.6")
      (assert-equal 120 (physics-canvas-config-simulation-rate config)
                    "Custom simulation rate should be 120 Hz"))))

(defun test-destroy-canvas ()
  "Test canvas destruction."
  (test "Destroy existing canvas"
    (cleanup-all-physics-canvases)

    (init-canvas-physics "test-canvas-5")
    (assert-true (canvas-physics-active-p "test-canvas-5")
                 "Canvas should be active")

    (let ((result (destroy-canvas-physics "test-canvas-5")))
      (assert-true result "Destroy should return T")
      (assert-nil (canvas-physics-active-p "test-canvas-5")
                  "Canvas should no longer be active")))

  (test "Destroy non-existent canvas"
    (let ((result (destroy-canvas-physics "nonexistent-canvas")))
      (assert-nil result "Destroying non-existent canvas should return NIL"))))

(defun test-multiple-canvases ()
  "Test managing multiple canvases simultaneously."
  (test "Multiple canvases isolation"
    (cleanup-all-physics-canvases)

    (init-canvas-physics "canvas-a" :gravity-y 10.0)
    (init-canvas-physics "canvas-b" :gravity-y 20.0)
    (init-canvas-physics "canvas-c" :gravity-y 30.0)

    (let ((canvases (list-active-physics-canvases)))
      (assert-equal 3 (length canvases) "Should have 3 active canvases"))

    (let ((config-a (get-canvas-physics-config "canvas-a"))
          (config-b (get-canvas-physics-config "canvas-b"))
          (config-c (get-canvas-physics-config "canvas-c")))
      (assert-equal 10.0 (physics-canvas-config-gravity-y config-a))
      (assert-equal 20.0 (physics-canvas-config-gravity-y config-b))
      (assert-equal 30.0 (physics-canvas-config-gravity-y config-c)))))

(defun test-update-settings ()
  "Test updating physics settings."
  (test "Update gravity settings"
    (cleanup-all-physics-canvases)

    (init-canvas-physics "test-canvas-6" :gravity-y 9.8)
    (update-canvas-physics-settings "test-canvas-6" :gravity-y 19.6)

    (let ((config (get-canvas-physics-config "test-canvas-6")))
      (assert-equal 19.6 (physics-canvas-config-gravity-y config)
                    "Gravity should be updated to 19.6")))

  (test "Update simulation rate"
    (cleanup-all-physics-canvases)

    (init-canvas-physics "test-canvas-7" :simulation-rate 60)
    (update-canvas-physics-settings "test-canvas-7" :simulation-rate 120)

    (let ((config (get-canvas-physics-config "test-canvas-7")))
      (assert-equal 120 (physics-canvas-config-simulation-rate config)
                    "Simulation rate should be updated to 120 Hz"))))

(defun test-cleanup ()
  "Test cleanup of all canvases."
  (test "Cleanup all canvases"
    (cleanup-all-physics-canvases)

    (init-canvas-physics "canvas-1")
    (init-canvas-physics "canvas-2")
    (init-canvas-physics "canvas-3")

    (let ((count (cleanup-all-physics-canvases)))
      (assert-equal 3 count "Should cleanup 3 canvases"))

    (assert-equal 0 (length (list-active-physics-canvases))
                  "No canvases should remain active")))

(defun test-thread-safety ()
  "Test thread-safe operations."
  (test "Concurrent canvas initialization"
    (cleanup-all-physics-canvases)

    (let ((threads '()))
      ;; Create 10 canvases concurrently
      (dotimes (i 10)
        (push (bt:make-thread
               (lambda (id)
                 (init-canvas-physics (format nil "thread-canvas-~D" id)
                                      :gravity-y (float id)))
               :arguments (list i))
              threads))

      ;; Wait for all threads to complete
      (dolist (thread threads)
        (bt:join-thread thread))

      ;; Verify all canvases were created
      (assert-equal 10 (length (list-active-physics-canvases))
                    "All 10 canvases should be created"))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-tests ()
  "Run all ECS storage management tests."
  (setf *test-count* 0
        *test-passed* 0
        *test-failed* 0)

  (format t "~%╔════════════════════════════════════════════╗~%")
  (format t "║  ECS Storage Management Tests (Task 2.1)  ║~%")
  (format t "╚════════════════════════════════════════════╝~%")

  ;; Run all test suites
  (test-basic-initialization)
  (test-retrieve-storage)
  (test-configuration)
  (test-destroy-canvas)
  (test-multiple-canvases)
  (test-update-settings)
  (test-cleanup)
  (test-thread-safety)

  ;; Print summary
  (format t "~%╔════════════════════════════════════════════╗~%")
  (format t "║              Test Summary                  ║~%")
  (format t "╠════════════════════════════════════════════╣~%")
  (format t "║  Total:  ~3D tests                          ║~%" *test-count*)
  (format t "║  Passed: ~3D tests                          ║~%" *test-passed*)
  (format t "║  Failed: ~3D tests                          ║~%" *test-failed*)
  (format t "╚════════════════════════════════════════════╝~%")

  (if (zerop *test-failed*)
      (progn
        (format t "~%✓ All tests PASSED!~%~%")
        0)
      (progn
        (format t "~%✗ Some tests FAILED!~%~%")
        1)))

;;; ============================================================================
;;; Main Entry Point
;;; ============================================================================

(defun main ()
  "Main entry point for test suite."
  (handler-case
      (run-tests)
    (error (e)
      (format t "~%FATAL ERROR: ~A~%~%" e)
      1)))

#|

USAGE:
------

Run from command line:
  ros -l backend/tests/test-ecs-storage.lisp \
      -e "(collabcanvas/tests/ecs-storage:main)" \
      -e "(sb-ext:exit)"

Or from REPL:
  (load "backend/tests/test-ecs-storage.lisp")
  (collabcanvas/tests/ecs-storage:run-tests)

WHAT THIS TESTS:
----------------

1. Basic ECS storage initialization
2. Storage retrieval (get-canvas-ecs-storage)
3. Configuration with default and custom values
4. Canvas destruction
5. Multiple isolated canvases
6. Settings updates
7. Cleanup operations
8. Thread-safe concurrent access

SUCCESS CRITERIA:
-----------------

✓ All hash table operations work correctly
✓ ECS storage instances are created properly
✓ Configuration values are stored and retrieved correctly
✓ Canvas isolation is maintained
✓ Thread-safe operations work without conflicts
✓ Cleanup properly removes all canvases

|#
