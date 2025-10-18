;;;; profile-physics-loop.lisp - Performance Profiling for Physics Loop
;;;;
;;;; Uses SBCL's statistical profiler (sb-sprof) to analyze physics loop performance.
;;;; Identifies bottlenecks in collision detection, force application, and broadcasting.
;;;;
;;;; Usage:
;;;;   (ql:quickload :collabcanvas)
;;;;   (load "backend/tests/profile-physics-loop.lisp")
;;;;   (profile-physics-scenarios)
;;;;
;;;; PRD Section: Task 5.4 - Performance Profiling

(in-package #:collabcanvas)

(require :sb-sprof)

;;; ============================================================================
;;; Profiling Configuration
;;; ============================================================================

(defparameter *profile-duration* 60
  "Duration to run physics simulation for profiling (seconds)")

(defparameter *profile-max-samples* 10000
  "Maximum number of profiler samples to collect")

(defparameter *profile-canvas-id* "profile-test-canvas"
  "Canvas ID used for profiling tests")

;;; ============================================================================
;;; Test Scenario Setup
;;; ============================================================================

(defun setup-profiling-scenario (canvas-id num-balls num-force-fields &optional emitter-active)
  "Set up a profiling scenario with specified number of entities.

   Arguments:
     canvas-id        - Canvas identifier
     num-balls        - Number of ball entities to create
     num-force-fields - Number of force field entities
     emitter-active   - T to activate emitter (not implemented yet)

   Returns:
     ECS storage instance"

  (format t "~%[PROFILE] Setting up scenario: ~D balls, ~D force fields~%"
          num-balls num-force-fields)

  ;; Clean up any existing state
  (when (physics-loop-running-p canvas-id)
    (stop-physics-loop canvas-id))

  ;; Remove existing storage
  (bt:with-lock-held (*canvas-ecs-storage-lock*)
    (remhash canvas-id *canvas-ecs-storage*))

  ;; Create fresh storage
  (let ((storage (get-canvas-ecs-storage canvas-id)))

    ;; Create balls in grid layout
    (let ((grid-size (ceiling (sqrt num-balls)))
          (spacing 60.0))

      (cl-fast-ecs:with-storage (storage)
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
                                        :restitution 0.8)))))

    ;; Create force fields
    (when (> num-force-fields 0)
      (cl-fast-ecs:with-storage (storage)
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

            ;; Add force field components
            (cl-fast-ecs:make-component entity 'position
                                        :x (coerce x 'single-float)
                                        :y (coerce y 'single-float))

            (cl-fast-ecs:make-component entity 'force-field
                                        :field-type field-type
                                        :strength 50.0
                                        :radius 200.0
                                        :direction (coerce angle 'single-float))))))

    (format t "[PROFILE] Scenario setup complete~%")
    storage))

;;; ============================================================================
;;; Profiling Runner
;;; ============================================================================

(defun run-physics-for-duration (canvas-id duration)
  "Run physics loop for specified duration (seconds).

   Arguments:
     canvas-id - Canvas identifier
     duration  - Duration in seconds

   Side effects:
     - Starts physics loop
     - Sleeps for duration
     - Stops physics loop"

  (format t "[PROFILE] Running physics for ~D seconds...~%" duration)

  ;; Start physics loop
  (start-physics-loop canvas-id)

  ;; Let it run
  (sleep duration)

  ;; Stop physics loop
  (stop-physics-loop canvas-id)

  (format t "[PROFILE] Physics run complete~%"))

(defun profile-physics-scenario (scenario-name num-balls num-force-fields
                                 &optional emitter-active)
  "Profile a specific physics scenario.

   Arguments:
     scenario-name    - Descriptive name for the scenario
     num-balls        - Number of ball entities
     num-force-fields - Number of force field entities
     emitter-active   - T to activate emitter (optional)

   Returns:
     Profiling report (printed to stdout)"

  (format t "~%~%========================================~%")
  (format t "PROFILING: ~A~%" scenario-name)
  (format t "========================================~%")

  ;; Set up scenario
  (setup-profiling-scenario *profile-canvas-id* num-balls num-force-fields emitter-active)

  ;; Configure gravity
  (let ((config (get-canvas-physics-config *profile-canvas-id*)))
    (when config
      (setf (physics-canvas-config-gravity-x config) 0.0)
      (setf (physics-canvas-config-gravity-y config) 98.0)))

  ;; Run profiling
  (format t "~%[PROFILE] Starting profiler...~%")

  (sb-sprof:with-profiling (:max-samples *profile-max-samples*
                            :report :graph
                            :loop nil
                            :show-progress t)
    (run-physics-for-duration *profile-canvas-id* *profile-duration*))

  (format t "~%[PROFILE] Scenario complete~%~%"))

;;; ============================================================================
;;; Main Profiling Entry Point
;;; ============================================================================

(defun profile-physics-scenarios ()
  "Run all profiling scenarios and generate reports.

   Scenarios:
     1. Baseline: 100 balls, no force fields
     2. Target: 500 balls, no force fields
     3. Complex: 500 balls + 5 force fields
     4. Future: 500 balls + 5 force fields + emitter (not implemented)

   Output:
     - Prints profiling reports to stdout
     - Shows top functions by time
     - Identifies bottlenecks

   Usage:
     (profile-physics-scenarios)"

  (format t "~%~%")
  (format t "╔══════════════════════════════════════════════════════════════╗~%")
  (format t "║       PHYSICS LOOP PERFORMANCE PROFILING                    ║~%")
  (format t "║       Task 5.4 - Performance Analysis                       ║~%")
  (format t "╚══════════════════════════════════════════════════════════════╝~%")

  (format t "~%Configuration:~%")
  (format t "  - Duration: ~D seconds per scenario~%" *profile-duration*)
  (format t "  - Max samples: ~D~%" *profile-max-samples*)
  (format t "  - Physics frequency: 60 Hz~%")
  (format t "  - Broadcast frequency: 20 Hz~%")
  (format t "  - Expected ticks: ~D~%" (* *profile-duration* 60))

  ;; Scenario 1: Baseline (100 balls)
  (profile-physics-scenario "Scenario 1: Baseline (100 balls)" 100 0)

  ;; Scenario 2: Target (500 balls)
  (profile-physics-scenario "Scenario 2: Target (500 balls)" 500 0)

  ;; Scenario 3: Complex (500 balls + 5 force fields)
  (profile-physics-scenario "Scenario 3: Complex (500 balls + 5 force fields)" 500 5)

  ;; Scenario 4: Future (500 balls + 5 force fields + emitter)
  ;; (profile-physics-scenario "Scenario 4: Future (500 balls + emitter)" 500 5 t)

  (format t "~%~%")
  (format t "╔══════════════════════════════════════════════════════════════╗~%")
  (format t "║       PROFILING COMPLETE                                     ║~%")
  (format t "╚══════════════════════════════════════════════════════════════╝~%")
  (format t "~%Next steps:~%")
  (format t "  1. Review flamegraphs and call trees above~%")
  (format t "  2. Identify top 3 bottlenecks~%")
  (format t "  3. Document findings in .taskmaster/docs/PROFILING_RESULTS.md~%")
  (format t "  4. Prioritize optimization opportunities~%")
  (format t "~%"))

;;; ============================================================================
;;; Quick Single-Scenario Profiling
;;; ============================================================================

(defun quick-profile (&optional (num-balls 500))
  "Quick profiling for development/debugging.

   Arguments:
     num-balls - Number of balls to profile (default: 500)

   Usage:
     (quick-profile)      ; Profile 500 balls
     (quick-profile 100)  ; Profile 100 balls"

  (profile-physics-scenario
   (format nil "Quick Profile (~D balls)" num-balls)
   num-balls
   0))

;;; ============================================================================
;;; Profiling Utilities
;;; ============================================================================

(defun profile-single-system (system-name num-balls)
  "Profile a single ECS system in isolation.

   Arguments:
     system-name - Symbol naming the system (e.g., 'collision-system)
     num-balls   - Number of balls to test with

   Usage:
     (profile-single-system 'collision-system 500)"

  (format t "~%[PROFILE] Profiling system: ~A (~D balls)~%" system-name num-balls)

  ;; Set up scenario
  (let ((storage (setup-profiling-scenario *profile-canvas-id* num-balls 0)))

    (cl-fast-ecs:with-storage (storage)

      ;; Profile system execution
      (sb-sprof:with-profiling (:max-samples 1000
                                :report :flat
                                :loop nil)

        ;; Run system 1000 times
        (dotimes (i 1000)
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
             (format t "[PROFILE ERROR] Unknown system: ~A~%" system-name)
             (return-from profile-single-system nil))))))))

(defun compare-scenarios ()
  "Compare performance across different scenarios (shorter duration).

   Useful for quick performance comparisons during optimization work."

  (let ((*profile-duration* 10))  ; Shorter duration for comparison
    (format t "~%QUICK SCENARIO COMPARISON (10 seconds each)~%~%")

    (profile-physics-scenario "100 balls" 100 0)
    (profile-physics-scenario "500 balls" 500 0)
    (profile-physics-scenario "500 balls + 5 force fields" 500 5)))

;;; ============================================================================
;;; Module Initialization
;;; ============================================================================

(format t "~%Physics Loop Profiler loaded~%")
(format t "  Main function: (profile-physics-scenarios)~%")
(format t "  Quick profile: (quick-profile)~%")
(format t "  Single system: (profile-single-system 'collision-system 500)~%")
(format t "  Comparison:    (compare-scenarios)~%~%")
