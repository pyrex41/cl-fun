;;;; quick-profile.lisp - Quick System-Level Profiling
;;;;
;;;; Simplified profiling that focuses on individual system performance
;;;; without requiring full server infrastructure.
;;;;
;;;; Usage:
;;;;   sbcl --load backend/tests/quick-profile.lisp
;;;;
;;;; Task 5.4 - Performance Profiling

(require :asdf)
(require :sb-sprof)

;; Load the system
(ql:quickload :collabcanvas :silent t)

(in-package #:collabcanvas)

(format t "~%~%")
(format t "╔══════════════════════════════════════════════════════════════╗~%")
(format t "║       QUICK PHYSICS PROFILING                                ║~%")
(format t "║       Task 5.4 - System Performance Analysis                ║~%")
(format t "╚══════════════════════════════════════════════════════════════╝~%")
(format t "~%")

;;; ============================================================================
;;; Test Scenario Setup
;;; ============================================================================

(defun create-test-storage (num-balls num-force-fields)
  "Create ECS storage with test entities."

  (let ((storage (cl-fast-ecs:make-storage)))

    (cl-fast-ecs:with-storage (storage)

      ;; Create balls in grid layout
      (let ((grid-size (ceiling (sqrt num-balls)))
            (spacing 60.0))

        (dotimes (i num-balls)
          (let* ((row (floor i grid-size))
                 (col (mod i grid-size))
                 (x (+ 100.0 (* col spacing)))
                 (y (+ 100.0 (* row spacing)))
                 (entity (cl-fast-ecs:make-entity)))

            ;; Add ball components
            (cl-fast-ecs:make-component entity 'position
                                        :x (coerce x 'single-float)
                                        :y (coerce y 'single-float))

            (cl-fast-ecs:make-component entity 'velocity
                                        :vx (coerce (- (random 100.0) 50.0) 'single-float)
                                        :vy (coerce (- (random 100.0) 50.0) 'single-float))

            (cl-fast-ecs:make-component entity 'acceleration
                                        :ax 0.0
                                        :ay 0.0)

            (cl-fast-ecs:make-component entity 'ball
                                        :radius 10.0
                                        :mass 1.0
                                        :restitution 0.8))))

      ;; Create force fields
      (when (> num-force-fields 0)
        (dotimes (i num-force-fields)
          (let* ((angle (* i (/ (* 2 pi) num-force-fields)))
                 (radius 300.0)
                 (x (+ 400.0 (* radius (cos angle))))
                 (y (+ 400.0 (* radius (sin angle))))
                 (entity (cl-fast-ecs:make-entity))
                 (field-type (case (mod i 3)
                              (0 :fan)
                              (1 :gravity-well)
                              (2 :magnet))))

            (cl-fast-ecs:make-component entity 'position
                                        :x (coerce x 'single-float)
                                        :y (coerce y 'single-float))

            (cl-fast-ecs:make-component entity 'force-field
                                        :field-type field-type
                                        :strength 50.0
                                        :radius 200.0
                                        :direction (coerce angle 'single-float))))))

    storage))

;;; ============================================================================
;;; System Profiling
;;; ============================================================================

(defun profile-system (system-name storage num-iterations)
  "Profile a single system execution."

  (format t "~%[PROFILE] System: ~A (~D iterations)~%" system-name num-iterations)
  (force-output)

  (cl-fast-ecs:with-storage (storage)

    (sb-sprof:with-profiling (:max-samples 5000
                              :report :flat
                              :loop nil
                              :show-progress nil)

      (dotimes (i num-iterations)
        (case system-name
          (collision-system
           (cl-fast-ecs:run-system 'collision-system :dt 0.016))

          (apply-forces-system
           (cl-fast-ecs:run-system 'apply-forces-system :dt 0.016))

          (apply-acceleration-system
           (cl-fast-ecs:run-system 'apply-acceleration-system
                                   :dt 0.016
                                   :gravity-x 0.0
                                   :gravity-y 98.0))

          (apply-velocity-system
           (cl-fast-ecs:run-system 'apply-velocity-system :dt 0.016))

          (check-sleeping-system
           (cl-fast-ecs:run-system 'check-sleeping-system))

          (t
           (error "Unknown system: ~A" system-name)))))))

(defun profile-all-systems (num-balls num-force-fields num-iterations)
  "Profile all physics systems."

  (format t "~%Scenario: ~D balls, ~D force fields~%" num-balls num-force-fields)
  (format t "Iterations per system: ~D~%~%" num-iterations)

  (let ((storage (create-test-storage num-balls num-force-fields)))

    ;; Profile each system
    (profile-system 'apply-forces-system storage num-iterations)
    (profile-system 'apply-acceleration-system storage num-iterations)
    (profile-system 'apply-velocity-system storage num-iterations)
    (profile-system 'collision-system storage num-iterations)
    (profile-system 'check-sleeping-system storage num-iterations)))

;;; ============================================================================
;;; Scenario Profiling
;;; ============================================================================

(defun run-profiling-scenarios ()
  "Run all profiling scenarios."

  (format t "~%Starting profiling scenarios...~%")
  (format t "This will take approximately 2-3 minutes.~%~%")
  (force-output)

  ;; Scenario 1: Baseline (100 balls)
  (format t "~%~%========================================~%")
  (format t "SCENARIO 1: Baseline (100 balls)~%")
  (format t "========================================~%")
  (profile-all-systems 100 0 1000)

  ;; Scenario 2: Target (500 balls)
  (format t "~%~%========================================~%")
  (format t "SCENARIO 2: Target (500 balls)~%")
  (format t "========================================~%")
  (profile-all-systems 500 0 1000)

  ;; Scenario 3: Complex (500 balls + 5 force fields)
  (format t "~%~%========================================~%")
  (format t "SCENARIO 3: Complex (500 balls + 5 force fields)~%")
  (format t "========================================~%")
  (profile-all-systems 500 5 1000)

  (format t "~%~%")
  (format t "╔══════════════════════════════════════════════════════════════╗~%")
  (format t "║       PROFILING COMPLETE                                     ║~%")
  (format t "╚══════════════════════════════════════════════════════════════╝~%")
  (format t "~%"))

;;; ============================================================================
;;; Timing Benchmark
;;; ============================================================================

(defun benchmark-systems (num-balls num-force-fields num-ticks)
  "Simple timing benchmark for all systems."

  (format t "~%~%========================================~%")
  (format t "TIMING BENCHMARK~%")
  (format t "========================================~%")
  (format t "Configuration: ~D balls, ~D force fields~%" num-balls num-force-fields)
  (format t "Simulating ~D ticks (60 Hz) = ~,1F seconds~%~%"
          num-ticks (/ num-ticks 60.0))

  (let ((storage (create-test-storage num-balls num-force-fields)))

    (cl-fast-ecs:with-storage (storage)

      (let ((start-time (get-internal-real-time)))

        ;; Run physics loop
        (dotimes (tick num-ticks)

          ;; Run all systems
          (cl-fast-ecs:run-system 'apply-forces-system :dt 0.016)
          (cl-fast-ecs:run-system 'apply-acceleration-system
                                  :dt 0.016
                                  :gravity-x 0.0
                                  :gravity-y 98.0)
          (cl-fast-ecs:run-system 'apply-velocity-system :dt 0.016)
          (cl-fast-ecs:run-system 'collision-system :dt 0.016)
          (cl-fast-ecs:run-system 'check-sleeping-system)

          ;; Progress indicator every 60 ticks (1 second)
          (when (zerop (mod tick 60))
            (format t ".")
            (force-output)))

        (let* ((end-time (get-internal-real-time))
               (elapsed (/ (- end-time start-time) internal-time-units-per-second))
               (ticks-per-second (/ num-ticks elapsed))
               (target-hz 60)
               (performance-ratio (/ ticks-per-second target-hz)))

          (format t "~%~%Results:~%")
          (format t "  Total time: ~,2F seconds~%" elapsed)
          (format t "  Ticks per second: ~,1F~%" ticks-per-second)
          (format t "  Target: ~D Hz~%" target-hz)
          (format t "  Performance: ~,1Fx realtime~%" performance-ratio)

          (if (>= performance-ratio 1.0)
              (format t "  ✓ PASSED: Can maintain 60 Hz~%")
              (format t "  ✗ FAILED: Cannot maintain 60 Hz (~,1F%% of target)~%"
                      (* performance-ratio 100.0))))))))

;;; ============================================================================
;;; Main Entry Point
;;; ============================================================================

;; Run benchmarks
(benchmark-systems 100 0 3600)   ; 100 balls, 60 seconds
(benchmark-systems 500 0 3600)   ; 500 balls, 60 seconds
(benchmark-systems 500 5 3600)   ; 500 balls + force fields, 60 seconds

;; Run detailed profiling
(run-profiling-scenarios)

(format t "~%Next steps:~%")
(format t "  1. Review profiling output above~%")
(format t "  2. Identify top bottlenecks~%")
(format t "  3. Create .taskmaster/docs/PROFILING_RESULTS.md~%")
(format t "~%")

;; Exit
(quit)
