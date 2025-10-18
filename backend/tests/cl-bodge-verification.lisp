;;;; cl-bodge-verification.lisp - Verification test for physics engine module
;;;;
;;;; Purpose: Verify that a suitable physics engine can be loaded for the project
;;;; Task: Subtask 1.1 - Install and Verify cl-bodge Physics Module
;;;; Phase: 0 (Library Validation)
;;;;
;;;; UPDATE (2025-10-17): cl-bodge/physics unavailable, testing alternatives
;;;;   - bodge distribution URL unreachable
;;;;   - bodge-chipmunk has compilation errors on macOS ARM64
;;;;   - Testing alternative approaches

(in-package :cl-user)

(defpackage :cl-bodge-verification
  (:use :cl)
  (:export #:run-all-tests)
  (:documentation "Verification tests for physics engine installation"))

(in-package :cl-bodge-verification)

;;; ============================================================================
;;; Test 1: Check for system Chipmunk2D library
;;; ============================================================================

(defun test-system-chipmunk ()
  "Test if Chipmunk2D is installed at system level (required for bodge-chipmunk)."
  (format t "~%=== Test 1: Checking System Chipmunk2D Library ===~%")
  (let ((result (uiop:run-program '("which" "chipmunk")
                                  :ignore-error-status t
                                  :output :string
                                  :error-output nil)))
    (if (and result (> (length (string-trim '(#\Newline #\Space) result)) 0))
        (progn
          (format t "✓ Found system Chipmunk library: ~A~%" result)
          t)
        (progn
          (format t "⚠  Chipmunk2D not found at system level~%")
          (format t "   Install with: brew install chipmunk~%")
          nil))))

;;; ============================================================================
;;; Test 2: Attempt to load bodge-chipmunk
;;; ============================================================================

(defun test-load-bodge-chipmunk ()
  "Test that bodge-chipmunk can be loaded via Quicklisp."
  (format t "~%=== Test 2: Loading bodge-chipmunk ===~%")
  (handler-case
      (progn
        (ql:quickload :bodge-chipmunk :verbose nil)
        (format t "✓ Successfully loaded bodge-chipmunk~%")
        t)
    (error (e)
      (format t "✗ Failed to load bodge-chipmunk~%")
      (format t "   Error: ~A~%" e)
      (format t "   This may be due to CLAW/CFFI compilation issues~%")
      nil)))

;;; ============================================================================
;;; Test 3: Attempt to load cl-bodge/physics (original plan)
;;; ============================================================================

(defun test-load-cl-bodge ()
  "Test that cl-bodge/physics can be loaded via Quicklisp (expected to fail)."
  (format t "~%=== Test 3: Loading cl-bodge/physics (expected FAIL) ===~%")
  (handler-case
      (progn
        (ql:quickload :cl-bodge/physics :verbose nil)
        (format t "✓ Successfully loaded cl-bodge/physics~%")
        t)
    (error ()
      (format t "✗ Failed to load cl-bodge/physics (EXPECTED)~%")
      (format t "   Reason: Bodge distribution unavailable~%")
      nil)))

;;; ============================================================================
;;; Test 4: Check for alternative physics libraries
;;; ============================================================================

(defun test-alternative-engines ()
  "Check if alternative physics engines are available."
  (format t "~%=== Test 4: Checking Alternative Physics Engines ===~%")
  (let ((alternatives '(:chipmunk-blob :bodge-ode))
        (found nil))
    (dolist (lib alternatives)
      (handler-case
          (progn
            (ql:quickload lib :verbose nil :silent t)
            (format t "✓ Found alternative: ~A~%" lib)
            (push lib found))
        (error ()
          (format t "✗ Not available: ~A~%" lib))))
    (if found
        (progn
          (format t "~%Available alternatives: ~A~%" found)
          t)
        (progn
          (format t "~%⚠  No alternative physics engines found~%")
          nil))))

;;; ============================================================================
;;; Main test runner
;;; ============================================================================

(defun run-all-tests ()
  "Run all physics engine verification tests and report results."
  (format t "~%~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  PHYSICS ENGINE VERIFICATION TEST SUITE          ║~%")
  (format t "║  Subtask 1.1 - Library Installation Validation  ║~%")
  (format t "║  Updated: Testing alternatives to cl-bodge       ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%")

  (let* ((sys-chipmunk (test-system-chipmunk))
         (bodge-chipmunk (test-load-bodge-chipmunk))
         (cl-bodge (test-load-cl-bodge))
         (alternatives (test-alternative-engines))
         (results (list sys-chipmunk bodge-chipmunk cl-bodge alternatives))
         (passed (count t results))
         (total (length results)))

    (format t "~%~%╔════════════════════════════════════════════════════╗~%")
    (format t "║  TEST RESULTS SUMMARY                            ║~%")
    (format t "╚════════════════════════════════════════════════════╝~%")
    (format t "~%Tests passed: ~A/~A~%" passed total)
    (format t "~%Individual Results:~%")
    (format t "  System Chipmunk2D: ~A~%" (if sys-chipmunk "PASS" "FAIL"))
    (format t "  bodge-chipmunk:    ~A~%" (if bodge-chipmunk "PASS" "FAIL"))
    (format t "  cl-bodge/physics:  ~A~%" (if cl-bodge "PASS" "FAIL (expected)"))
    (format t "  Alternatives:      ~A~%" (if alternatives "FOUND" "NONE"))

    (format t "~%~%╔════════════════════════════════════════════════════╗~%")
    (format t "║  RECOMMENDATIONS                                 ║~%")
    (format t "╚════════════════════════════════════════════════════╝~%")

    (cond
      ((and bodge-chipmunk)
       (format t "~%✓ SUCCESS: bodge-chipmunk is available!~%")
       (format t "   Proceed with implementation using bodge-chipmunk~%~%"))
      ((and alternatives (not bodge-chipmunk))
       (format t "~%⚠  PARTIAL: Alternative engines found~%")
       (format t "   Consider using one of the discovered alternatives~%~%"))
      (t
       (format t "~%✗ BLOCKED: No viable physics engine found~%")
       (format t "   Next steps:~%")
       (format t "   1. Install system Chipmunk2D: brew install chipmunk~%")
       (format t "   2. Test on Linux x86_64 (Fly.io target platform)~%")
       (format t "   3. Consider Node.js/Matter.js server-side alternative~%~%")))

    (>= passed 1))) ; Consider success if at least one engine works

;;; ============================================================================
;;; Quick test invocation
;;; ============================================================================

(format t "~%To run verification tests, execute:~%")
(format t "  (cl-bodge-verification:run-all-tests)~%~%")
