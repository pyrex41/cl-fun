;;;; ECS Physics Performance Benchmark
;;;; Tests cl-fast-ecs with 500 balls and custom physics systems
;;;; Target: <16ms per frame at 60 Hz

;;; Load cl-fast-ecs first
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-fast-ecs :silent t))

(defpackage :collabcanvas/tests/ecs-benchmark
  (:use :cl)
  (:import-from :cl-fast-ecs
                #:define-component
                #:define-system
                #:make-storage
                #:with-storage
                #:make-object
                #:run-systems)
  (:export #:main
           #:run-benchmark))

(in-package :collabcanvas/tests/ecs-benchmark)

;;; ============================================================================
;;; Component Definitions (from PRD Section 4.1.2)
;;; ============================================================================

(define-component position
  "World space position in pixels."
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))

(define-component velocity
  "Velocity vector in pixels/second."
  (vx 0.0 :type single-float)
  (vy 0.0 :type single-float))

(define-component acceleration
  "Acceleration vector in pixels/second²."
  (ax 0.0 :type single-float)
  (ay 0.0 :type single-float))

(define-component ball
  "Dynamic circular physics body."
  (radius 10.0 :type single-float)
  (mass 1.0 :type single-float)
  (restitution 0.8 :type single-float))

;;; ============================================================================
;;; Physics Systems (from PRD Section 4.1.3)
;;; ============================================================================

;; Updates velocity based on acceleration: v(t+dt) = v(t) + a(t) * dt
(define-system apply-acceleration-system
  (:components-ro (acceleration)
   :components-rw (velocity)
   :arguments ((:dt single-float)))
  ;; v += a * dt
  (incf velocity-vx (* dt acceleration-ax))
  (incf velocity-vy (* dt acceleration-ay)))

;; Updates position based on velocity: p(t+dt) = p(t) + v(t) * dt
(define-system apply-velocity-system
  (:components-ro (velocity)
   :components-rw (position)
   :arguments ((:dt single-float)))
  ;; p += v * dt
  (incf position-x (* dt velocity-vx))
  (incf position-y (* dt velocity-vy)))

;; Bounces balls at world edges (800x600 canvas)
(define-system boundary-bounce-system
  (:components-rw (position velocity)
   :components-ro (ball))
  (let ((min-x 0.0)
        (max-x 800.0)
        (min-y 0.0)
        (max-y 600.0)
        (radius ball-radius)
        (restitution ball-restitution))

    ;; Left/Right walls
    (when (< position-x (+ min-x radius))
      (setf position-x (+ min-x radius))
      (setf velocity-vx (* (- velocity-vx) restitution)))
    (when (> position-x (- max-x radius))
      (setf position-x (- max-x radius))
      (setf velocity-vx (* (- velocity-vx) restitution)))

    ;; Top/Bottom walls
    (when (< position-y (+ min-y radius))
      (setf position-y (+ min-y radius))
      (setf velocity-vy (* (- velocity-vy) restitution)))
    (when (> position-y (- max-y radius))
      (setf position-y (- max-y radius))
      (setf velocity-vy (* (- velocity-vy) restitution)))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun random-float (min max)
  "Generate random float between min and max."
  (+ min (* (random 1.0) (- max min))))

(defun spawn-ball (x y vx vy)
  "Create a ball entity with position, velocity, acceleration components.
   Must be called within a with-storage context."
  (make-object
   `((:position :x ,x :y ,y)
     (:velocity :vx ,vx :vy ,vy)
     (:acceleration :ax 0.0 :ay 9.8)  ; Gravity: 9.8 pixels/s²
     (:ball :radius 10.0 :mass 1.0 :restitution 0.8))))

(defun spawn-random-balls (count)
  "Spawn COUNT balls with random positions and velocities.
   Must be called within a with-storage context."
  (format t "~%Spawning ~D balls with random positions/velocities...~%" count)
  (dotimes (i count)
    (spawn-ball (random-float 50.0 750.0)   ; x: 50-750
                (random-float 50.0 550.0)   ; y: 50-550
                (random-float -50.0 50.0)   ; vx: -50 to 50
                (random-float -50.0 50.0))) ; vy: -50 to 50
  (format t "✓ Spawned ~D balls~%" count))

(defun get-timestamp-ms ()
  "Get current timestamp in milliseconds."
  (* (get-internal-real-time)
     (/ 1000.0 internal-time-units-per-second)))

;;; ============================================================================
;;; Benchmark Runner
;;; ============================================================================

(defun run-benchmark (&key (num-balls 500) (duration-seconds 5))
  "Run physics benchmark with NUM-BALLS for DURATION-SECONDS.
   Returns average frame time in milliseconds."
  (format t "~%=== ECS Physics Benchmark ===~%")
  (format t "Target: <16ms per frame at 60 Hz~%")
  (format t "Balls: ~D~%" num-balls)
  (format t "Duration: ~D seconds~%" duration-seconds)
  (format t "Expected frames: ~D~%~%" (* duration-seconds 60))

  ;; Create ECS storage and spawn balls
  (let ((storage (make-storage)))
    (with-storage (storage)
      (spawn-random-balls num-balls))

    ;; Run simulation
    (format t "~%Running simulation...~%")
    (let ((dt (/ 1.0 60.0))           ; 16.67ms fixed timestep
          (frame-count 0)
          (total-frame-time 0.0)
          (max-frame-time 0.0)
          (min-frame-time 999999.0)
          (start-time (get-timestamp-ms))
          (target-duration-ms (* duration-seconds 1000.0)))

      ;; Simulation loop
      (loop while (< (- (get-timestamp-ms) start-time) target-duration-ms)
            do
            (let ((frame-start (get-timestamp-ms)))

              ;; Run physics systems
              (with-storage (storage)
                (run-systems :dt dt))

              ;; Measure frame time
              (let ((frame-time (- (get-timestamp-ms) frame-start)))
                (incf total-frame-time frame-time)
                (setf max-frame-time (max max-frame-time frame-time))
                (setf min-frame-time (min min-frame-time frame-time))
                (incf frame-count))

              ;; Sleep to maintain 60 Hz (approximately)
              (sleep dt)))

      ;; Print results
      (let ((avg-frame-time (/ total-frame-time frame-count))
            (actual-duration (/ (- (get-timestamp-ms) start-time) 1000.0)))
        (format t "~%=== Benchmark Results ===~%")
        (format t "Frames simulated: ~D~%" frame-count)
        (format t "Actual duration: ~,2F seconds~%" actual-duration)
        (format t "Average FPS: ~,2F~%" (/ frame-count actual-duration))
        (format t "~%Frame Times:~%")
        (format t "  Average: ~,3F ms~%" avg-frame-time)
        (format t "  Minimum: ~,3F ms~%" min-frame-time)
        (format t "  Maximum: ~,3F ms~%" max-frame-time)
        (format t "~%Target: <16ms per frame~%")
        (if (< avg-frame-time 16.0)
            (format t "✓ PASS - Average frame time is ~,3F ms~%" avg-frame-time)
            (format t "✗ FAIL - Average frame time is ~,3F ms (target: <16ms)~%" avg-frame-time))
        (format t "~%")

        ;; Return average frame time
        avg-frame-time))))

;;; ============================================================================
;;; Main Entry Point
;;; ============================================================================

(defun main ()
  "Main entry point for benchmark."
  (handler-case
      (progn
        (format t "~%╔════════════════════════════════════════════╗~%")
        (format t "║  cl-fast-ecs Physics Benchmark (Task 1.2) ║~%")
        (format t "╚════════════════════════════════════════════╝~%")

        ;; Check if cl-fast-ecs is loaded
        (unless (find-package :cl-fast-ecs)
          (error "cl-fast-ecs is not loaded. Please install it first."))

        (format t "~%✓ cl-fast-ecs loaded successfully~%")

        ;; Run benchmark
        (let ((avg-time (run-benchmark :num-balls 500 :duration-seconds 5)))
          (if (< avg-time 16.0)
              (progn
                (format t "~%╔════════════════════════════════════════════╗~%")
                (format t "║         ✓ BENCHMARK PASSED                 ║~%")
                (format t "║  Performance target achieved: ~,3F ms      ║~%" avg-time)
                (format t "╚════════════════════════════════════════════╝~%")
                0) ; Success exit code
              (progn
                (format t "~%╔════════════════════════════════════════════╗~%")
                (format t "║         ✗ BENCHMARK FAILED                 ║~%")
                (format t "║  Frame time too high: ~,3F ms (>16ms)     ║~%" avg-time)
                (format t "╚════════════════════════════════════════════╝~%")
                1)))) ; Failure exit code
    (error (e)
      (format t "~%ERROR: ~A~%" e)
      (format t "~%Make sure cl-fast-ecs is installed:~%")
      (format t "  1. Download from: https://github.com/ddl/cl-fast-ecs~%")
      (format t "  2. Extract to: ~~//quicklisp/local-projects/cl-fast-ecs/~~%")
      (format t "  3. Load with: (ql:quickload :cl-fast-ecs)~%")
      1)))

;;; ============================================================================
;;; Usage Instructions
;;; ============================================================================

#|

USAGE:
------

1. Install cl-fast-ecs:
   cd ~/quicklisp/local-projects/
   # Download and extract cl-fast-ecs

2. Load this file:
   ros -l backend/tests/ecs-physics-benchmark.lisp

3. Run benchmark:
   ros -l backend/tests/ecs-physics-benchmark.lisp \
       -e "(collabcanvas/tests/ecs-benchmark:main)" \
       -e "(sb-ext:exit)"

4. Or from REPL:
   (ql:quickload :cl-fast-ecs)
   (load "backend/tests/ecs-physics-benchmark.lisp")
   (collabcanvas/tests/ecs-benchmark:main)

EXPECTED OUTPUT:
----------------

╔════════════════════════════════════════════╗
║  cl-fast-ecs Physics Benchmark (Task 1.2) ║
╚════════════════════════════════════════════╝

✓ cl-fast-ecs loaded successfully

=== ECS Physics Benchmark ===
Target: <16ms per frame at 60 Hz
Balls: 500
Duration: 5 seconds
Expected frames: 300

Spawning 500 balls with random positions/velocities...
✓ Spawned 500 balls

Running simulation...

=== Benchmark Results ===
Frames simulated: 300
Actual duration: 5.00 seconds
Average FPS: 60.00

Frame Times:
  Average: 2.345 ms
  Minimum: 1.234 ms
  Maximum: 5.678 ms

Target: <16ms per frame
✓ PASS - Average frame time is 2.345 ms

╔════════════════════════════════════════════╗
║         ✓ BENCHMARK PASSED                 ║
║  Performance target achieved: 2.345 ms     ║
╚════════════════════════════════════════════╝

WHAT THIS TESTS:
----------------

1. cl-fast-ecs component definitions (position, velocity, acceleration, ball)
2. cl-fast-ecs system definitions (apply-acceleration, apply-velocity, boundary-bounce)
3. ECS storage creation and entity spawning
4. System execution with run-systems and dt parameter
5. Performance at 500 balls with gravity and boundary collisions
6. Frame time measurement to verify <16ms target

SUCCESS CRITERIA:
-----------------

✓ All components defined successfully
✓ All systems defined successfully
✓ 500 balls spawned with random initial conditions
✓ Simulation runs for 5 seconds
✓ Average frame time < 16ms

|#
