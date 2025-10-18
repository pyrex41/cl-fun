;;;; test-physics-metrics.lisp - Tests for Physics Metrics System
;;;;
;;;; Manual test suite to verify metrics collection and warning system.
;;;; Run in REPL after loading physics-metrics.lisp

(in-package #:collabcanvas)

;;; ============================================================================
;;; Test Helpers
;;; ============================================================================

(defun assert-equal (expected actual message)
  "Simple assertion helper."
  (if (equal expected actual)
      (format t "✓ PASS: ~A~%" message)
      (format t "✗ FAIL: ~A~%  Expected: ~A~%  Actual: ~A~%" message expected actual)))

(defun assert-true (condition message)
  "Assert condition is true."
  (if condition
      (format t "✓ PASS: ~A~%" message)
      (format t "✗ FAIL: ~A~%" message)))

;;; ============================================================================
;;; Test 1: Initialize and Clear Metrics
;;; ============================================================================

(defun test-init-clear ()
  "Test metrics initialization and cleanup."
  (format t "~%=== Test 1: Initialize and Clear Metrics ===~%")

  ;; Initialize metrics
  (init-physics-metrics "test-canvas-1")

  ;; Verify metrics exist
  (let ((metrics (get-physics-metrics "test-canvas-1")))
    (assert-true (not (null metrics))
                "Metrics initialized successfully")
    (assert-equal 0 (cdr (assoc :active-bodies metrics))
                 "Initial active-bodies is 0")
    (assert-equal 0 (cdr (assoc :sleeping-bodies metrics))
                 "Initial sleeping-bodies is 0")
    (assert-equal 0.0 (cdr (assoc :fps metrics))
                 "Initial FPS is 0.0"))

  ;; Clear metrics
  (clear-physics-metrics "test-canvas-1")

  ;; Verify metrics cleared
  (let ((metrics (get-physics-metrics "test-canvas-1")))
    (assert-true (null metrics)
                "Metrics cleared successfully")))

;;; ============================================================================
;;; Test 2: Update Entity Counts
;;; ============================================================================

(defun test-entity-counts ()
  "Test entity count tracking."
  (format t "~%=== Test 2: Entity Count Tracking ===~%")

  ;; Create test ECS storage
  (let ((storage (cl-fast-ecs:make-storage))
        (canvas-id "test-canvas-2"))

    ;; Initialize metrics
    (init-physics-metrics canvas-id)

    (cl-fast-ecs:with-storage (storage)
      ;; Create 3 active balls
      (dotimes (i 3)
        (let ((entity (cl-fast-ecs:make-entity)))
          (cl-fast-ecs:make-component entity 'ball :mass 1.0 :radius 10.0)
          (cl-fast-ecs:make-component entity 'position :x 0.0 :y 0.0)
          (cl-fast-ecs:make-component entity 'velocity :vx 1.0 :vy 1.0)))

      ;; Create 2 sleeping balls
      (dotimes (i 2)
        (let ((entity (cl-fast-ecs:make-entity)))
          (cl-fast-ecs:make-component entity 'ball :mass 1.0 :radius 10.0)
          (cl-fast-ecs:make-component entity 'position :x 0.0 :y 0.0)
          (cl-fast-ecs:make-component entity 'velocity :vx 0.0 :vy 0.0)
          (cl-fast-ecs:make-component entity 'sleeping)))

      ;; Create 1 force field
      (let ((entity (cl-fast-ecs:make-entity)))
        (cl-fast-ecs:make-component entity 'force-field
                                   :field-type :gravity-well
                                   :strength 100.0
                                   :radius 200.0)
        (cl-fast-ecs:make-component entity 'position :x 0.0 :y 0.0)))

    ;; Update metrics
    (bt:with-lock-held (*physics-metrics-lock*)
      (let ((metrics (gethash canvas-id *physics-metrics*)))
        (update-entity-counts canvas-id storage metrics)))

    ;; Verify counts
    (let ((metrics (get-physics-metrics canvas-id)))
      (assert-equal 3 (cdr (assoc :active-bodies metrics))
                   "Active bodies count correct")
      (assert-equal 2 (cdr (assoc :sleeping-bodies metrics))
                   "Sleeping bodies count correct")
      (assert-equal 1 (cdr (assoc :force-fields metrics))
                   "Force fields count correct"))

    ;; Cleanup
    (clear-physics-metrics canvas-id)))

;;; ============================================================================
;;; Test 3: Frame Time and FPS Tracking
;;; ============================================================================

(defun test-frame-time-fps ()
  "Test frame time and FPS calculation."
  (format t "~%=== Test 3: Frame Time and FPS Tracking ===~%")

  (let ((canvas-id "test-canvas-3"))
    ;; Initialize metrics
    (init-physics-metrics canvas-id)

    ;; Update frame time (16.67ms = 60 FPS)
    (bt:with-lock-held (*physics-metrics-lock*)
      (let ((metrics (gethash canvas-id *physics-metrics*)))
        (update-frame-time metrics 16.67)))

    ;; Verify FPS
    (let ((metrics (get-physics-metrics canvas-id)))
      (assert-equal 16.67 (cdr (assoc :frame-time-ms metrics))
                   "Frame time set correctly")
      (assert-true (< (abs (- 60.0 (cdr (assoc :fps metrics)))) 0.1)
                  "FPS calculated correctly (~60)"))

    ;; Update with different frame time (33.33ms = 30 FPS)
    (bt:with-lock-held (*physics-metrics-lock*)
      (let ((metrics (gethash canvas-id *physics-metrics*)))
        (update-frame-time metrics 33.33)))

    ;; Verify new FPS
    (let ((metrics (get-physics-metrics canvas-id)))
      (assert-true (< (abs (- 30.0 (cdr (assoc :fps metrics)))) 0.1)
                  "FPS updated correctly (~30)"))

    ;; Cleanup
    (clear-physics-metrics canvas-id)))

;;; ============================================================================
;;; Test 4: System Timing
;;; ============================================================================

(defun test-system-timing ()
  "Test per-system execution time tracking."
  (format t "~%=== Test 4: System Timing Tracking ===~%")

  (let ((canvas-id "test-canvas-4"))
    ;; Initialize metrics
    (init-physics-metrics canvas-id)

    ;; Update system times
    (bt:with-lock-held (*physics-metrics-lock*)
      (let ((metrics (gethash canvas-id *physics-metrics*)))
        (update-system-time metrics 'apply-forces-system 1.2)
        (update-system-time metrics 'apply-acceleration-system 0.8)
        (update-system-time metrics 'collision-system 2.5)))

    ;; Verify system times
    (let* ((metrics (get-physics-metrics canvas-id))
           (system-times (cdr (assoc :system-times metrics))))
      (assert-equal 1.2 (cdr (assoc "APPLY-FORCES-SYSTEM" system-times :test #'string=))
                   "apply-forces-system time tracked")
      (assert-equal 0.8 (cdr (assoc "APPLY-ACCELERATION-SYSTEM" system-times :test #'string=))
                   "apply-acceleration-system time tracked")
      (assert-equal 2.5 (cdr (assoc "COLLISION-SYSTEM" system-times :test #'string=))
                   "collision-system time tracked"))

    ;; Cleanup
    (clear-physics-metrics canvas-id)))

;;; ============================================================================
;;; Test 5: Broadcast Metrics
;;; ============================================================================

(defun test-broadcast-metrics ()
  "Test network broadcast metrics tracking."
  (format t "~%=== Test 5: Broadcast Metrics Tracking ===~%")

  (let ((canvas-id "test-canvas-5"))
    ;; Initialize metrics
    (init-physics-metrics canvas-id)

    ;; Simulate 3 broadcasts
    (bt:with-lock-held (*physics-metrics-lock*)
      (let ((metrics (gethash canvas-id *physics-metrics*)))
        (update-broadcast-metrics metrics 1024)  ; 1 KB
        (update-broadcast-metrics metrics 2048)  ; 2 KB
        (update-broadcast-metrics metrics 512)))  ; 0.5 KB

    ;; Verify metrics
    (let ((metrics (get-physics-metrics canvas-id)))
      (assert-equal 3 (cdr (assoc :broadcast-count metrics))
                   "Broadcast count tracked")
      (assert-equal 512 (cdr (assoc :delta-message-bytes metrics))
                   "Last message size tracked")
      (assert-equal 3584 (cdr (assoc :total-bytes-sent metrics))
                   "Total bytes accumulated (1024+2048+512)"))

    ;; Cleanup
    (clear-physics-metrics canvas-id)))

;;; ============================================================================
;;; Test 6: Warning System
;;; ============================================================================

(defun test-warning-system ()
  "Test performance warning threshold detection."
  (format t "~%=== Test 6: Warning System ===~%")

  (let ((canvas-id "test-canvas-6"))
    ;; Initialize metrics
    (init-physics-metrics canvas-id)

    ;; Set frame time above threshold (trigger warning)
    (bt:with-lock-held (*physics-metrics-lock*)
      (let ((metrics (gethash canvas-id *physics-metrics*)))
        (update-frame-time metrics 20.0)  ; > 16ms threshold
        (check-performance-thresholds canvas-id metrics)))

    ;; Verify warning counter incremented
    (let ((metrics (get-physics-metrics canvas-id)))
      (assert-equal 1 (cdr (assoc :high-latency-warnings metrics))
                   "High latency warning triggered"))

    ;; Set frame time below threshold (no warning)
    (bt:with-lock-held (*physics-metrics-lock*)
      (let ((metrics (gethash canvas-id *physics-metrics*)))
        (update-frame-time metrics 15.0)  ; < 16ms threshold
        (check-performance-thresholds canvas-id metrics)))

    ;; Verify warning counter unchanged
    (let ((metrics (get-physics-metrics canvas-id)))
      (assert-equal 1 (cdr (assoc :high-latency-warnings metrics))
                   "No additional warning when below threshold"))

    ;; Cleanup
    (clear-physics-metrics canvas-id)))

;;; ============================================================================
;;; Test 7: Metrics Summary
;;; ============================================================================

(defun test-metrics-summary ()
  "Test human-readable metrics summary."
  (format t "~%=== Test 7: Metrics Summary ===~%")

  (let ((canvas-id "test-canvas-7"))
    ;; Initialize metrics
    (init-physics-metrics canvas-id)

    ;; Set some values
    (bt:with-lock-held (*physics-metrics-lock*)
      (let ((metrics (gethash canvas-id *physics-metrics*)))
        (setf (physics-metrics-active-bodies metrics) 42)
        (setf (physics-metrics-sleeping-bodies metrics) 8)
        (setf (physics-metrics-force-fields metrics) 3)
        (update-frame-time metrics 16.67)))

    ;; Get summary
    (let ((summary (get-metrics-summary canvas-id)))
      (assert-true (search "Active:42" summary)
                  "Summary includes active bodies")
      (assert-true (search "Sleep:8" summary)
                  "Summary includes sleeping bodies")
      (assert-true (search "Fields:3" summary)
                  "Summary includes force fields")
      (format t "Summary: ~A~%" summary))

    ;; Cleanup
    (clear-physics-metrics canvas-id)))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(defun run-all-metrics-tests ()
  "Run complete metrics test suite."
  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  Physics Metrics Test Suite                       ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%")

  (test-init-clear)
  (test-entity-counts)
  (test-frame-time-fps)
  (test-system-timing)
  (test-broadcast-metrics)
  (test-warning-system)
  (test-metrics-summary)

  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  All Tests Complete!                               ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%"))

;;; Run tests when loaded
(format t "~%Physics Metrics Tests loaded.~%")
(format t "Run: (run-all-metrics-tests)~%")
