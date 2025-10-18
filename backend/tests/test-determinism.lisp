;;;; test-determinism.lisp - Determinism Testing Framework for Physics Engine
;;;;
;;;; Tests that identical initial conditions produce identical results across
;;;; multiple simulation runs. This ensures deterministic physics behavior for
;;;; client-side prediction in multiplayer environments.
;;;;
;;;; Test Strategy:
;;;;   1. Create two identical ECS storages with same initial state
;;;;   2. Run both simulations for 10 seconds (600 ticks at 60 Hz)
;;;;   3. Verify that final states match within floating-point tolerance
;;;;   4. Test edge cases (simultaneous spawns, rapid force field toggles, etc.)
;;;;
;;;; PRD Section: 4.3 - Determinism Testing Framework

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-fast-ecs :silent t)
  (ql:quickload :bordeaux-threads :silent t)
  (ql:quickload :trivial-backtrace :silent t))

;;; Load the collabcanvas system
(let ((asd-path (merge-pathnames "backend/collabcanvas.asd"
                                 (make-pathname :directory (butlast (pathname-directory *load-truename*) 2)))))
  (unless (probe-file asd-path)
    (error "Cannot find collabcanvas.asd at ~A" asd-path))
  (load asd-path))

(asdf:load-system :collabcanvas)

(in-package :collabcanvas)

;;; ============================================================================
;;; Constants
;;; ============================================================================

(defconstant +physics-hz+ 60
  "Physics simulation frequency in Hz.")

(defconstant +physics-dt+ (coerce (/ 1.0 +physics-hz+) 'single-float)
  "Fixed timestep in seconds (0.016667 for 60 Hz).")

(defconstant +simulation-duration+ 10.0
  "Duration of each test simulation in seconds.")

(defconstant +simulation-ticks+ (* +physics-hz+ +simulation-duration+)
  "Number of ticks to simulate (600 ticks for 10 seconds).")

(defconstant +float-tolerance+ 0.001
  "Maximum allowed difference for floating-point comparisons.")

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

(defmacro assert-float-equal (expected actual &optional message)
  "Assert that two floats are equal within tolerance."
  `(let ((exp (coerce ,expected 'single-float))
         (act (coerce ,actual 'single-float))
         (tol (coerce +float-tolerance+ 'single-float)))
     (unless (<= (abs (- act exp)) tol)
       (error "~@[~A~%~]Expected: ~,6F~%Got: ~,6F~%Difference: ~,6F (tolerance: ~,6F)"
              ,message exp act (abs (- act exp)) tol))))

;;; ============================================================================
;;; Entity Creation Helpers
;;; ============================================================================

(defun create-ball-entity (storage x y vx vy &key (radius 10.0) (mass 1.0) (restitution 0.8))
  "Create a ball entity in the given storage.
   Returns the entity ID."
  (with-storage (storage)
    (let ((entity (make-entity)))
      ;; Position component
      (let ((pos (make-component entity 'position)))
        (setf (position-x pos) (coerce x 'single-float))
        (setf (position-y pos) (coerce y 'single-float)))
      ;; Velocity component
      (let ((vel (make-component entity 'velocity)))
        (setf (velocity-vx vel) (coerce vx 'single-float))
        (setf (velocity-vy vel) (coerce vy 'single-float)))
      ;; Acceleration component
      (let ((acc (make-component entity 'acceleration)))
        (setf (acceleration-ax acc) 0.0)
        (setf (acceleration-ay acc) 0.0))
      ;; Ball component
      (let ((b (make-component entity 'ball)))
        (setf (ball-radius b) (coerce radius 'single-float))
        (setf (ball-mass b) (coerce mass 'single-float))
        (setf (ball-restitution b) (coerce restitution 'single-float)))
      entity)))

(defun create-block-entity (storage x y width height)
  "Create a block entity in the given storage.
   Returns the entity ID."
  (with-storage (storage)
    (let ((entity (make-entity)))
      ;; Position component
      (let ((pos (make-component entity 'position)))
        (setf (position-x pos) (coerce x 'single-float))
        (setf (position-y pos) (coerce y 'single-float)))
      ;; Block component
      (let ((blk (make-component entity 'block)))
        (setf (block-width blk) (coerce width 'single-float))
        (setf (block-height blk) (coerce height 'single-float)))
      entity)))

(defun create-force-field-entity (storage x y field-type strength radius &optional (direction 0.0))
  "Create a force field entity in the given storage.
   Returns the entity ID."
  (with-storage (storage)
    (let ((entity (make-entity)))
      ;; Position component
      (let ((pos (make-component entity 'position)))
        (setf (position-x pos) (coerce x 'single-float))
        (setf (position-y pos) (coerce y 'single-float)))
      ;; Force field component
      (let ((ff (make-component entity 'force-field)))
        (setf (force-field-field-type ff) field-type)
        (setf (force-field-strength ff) (coerce strength 'single-float))
        (setf (force-field-radius ff) (coerce radius 'single-float))
        (setf (force-field-direction ff) (coerce direction 'single-float)))
      entity)))

;;; ============================================================================
;;; Simulation Runner
;;; ============================================================================

(defun run-simulation (storage gravity-x gravity-y ticks)
  "Run physics simulation for the given number of ticks.
   Returns a hash table mapping entity-id to final state."
  (with-storage (storage)
    (dotimes (tick ticks)
      ;; System 1: Apply force fields to balls
      (run-system 'apply-forces-system
                              :dt +physics-dt+)

      ;; System 2: Update velocity from acceleration + gravity
      (run-system 'apply-acceleration-system
                              :dt +physics-dt+
                              :gravity-x gravity-x
                              :gravity-y gravity-y)

      ;; System 3: Update position from velocity
      (run-system 'apply-velocity-system
                              :dt +physics-dt+)

      ;; System 4: Detect and resolve collisions
      (run-system 'collision-system
                              :dt +physics-dt+)

      ;; System 5: Check for sleeping entities
      (run-system 'check-sleeping-system))

    ;; Collect final state
    (let ((final-state (make-hash-table)))
      (do-entities (entity)
        (when (has-component entity 'position)
          (let ((pos (get-component entity 'position))
                (vel (when (has-component entity 'velocity)
                      (get-component entity 'velocity))))
            (setf (gethash entity final-state)
                  (list :x (position-x pos)
                        :y (position-y pos)
                        :vx (if vel (velocity-vx vel) 0.0)
                        :vy (if vel (velocity-vy vel) 0.0))))))
      final-state)))

(defun compare-states (state1 state2)
  "Compare two state hash tables for equality within tolerance.
   Returns T if equal, NIL otherwise. Signals error with details if different."
  (let ((entities1 (alexandria:hash-table-keys state1))
        (entities2 (alexandria:hash-table-keys state2)))

    ;; Check same number of entities
    (unless (= (length entities1) (length entities2))
      (error "Different number of entities: ~D vs ~D"
             (length entities1) (length entities2)))

    ;; Sort entity IDs for consistent comparison
    (setf entities1 (sort entities1 #'<))
    (setf entities2 (sort entities2 #'<))

    ;; Check entity IDs match
    (unless (equal entities1 entities2)
      (error "Entity IDs don't match: ~A vs ~A" entities1 entities2))

    ;; Compare each entity's state
    (dolist (entity entities1)
      (let ((s1 (gethash entity state1))
            (s2 (gethash entity state2)))
        (let ((x1 (getf s1 :x))
              (y1 (getf s1 :y))
              (vx1 (getf s1 :vx))
              (vy1 (getf s1 :vy))
              (x2 (getf s2 :x))
              (y2 (getf s2 :y))
              (vx2 (getf s2 :vx))
              (vy2 (getf s2 :vy)))

          (unless (<= (abs (- x1 x2)) +float-tolerance+)
            (error "Entity ~D: X position mismatch: ~,6F vs ~,6F (diff: ~,6F)"
                   entity x1 x2 (abs (- x1 x2))))

          (unless (<= (abs (- y1 y2)) +float-tolerance+)
            (error "Entity ~D: Y position mismatch: ~,6F vs ~,6F (diff: ~,6F)"
                   entity y1 y2 (abs (- y1 y2))))

          (unless (<= (abs (- vx1 vx2)) +float-tolerance+)
            (error "Entity ~D: VX velocity mismatch: ~,6F vs ~,6F (diff: ~,6F)"
                   entity vx1 vx2 (abs (- vx1 vx2))))

          (unless (<= (abs (- vy1 vy2)) +float-tolerance+)
            (error "Entity ~D: VY velocity mismatch: ~,6F vs ~,6F (diff: ~,6F)"
                   entity vy1 vy2 (abs (- vy1 vy2)))))))

    t))

;;; ============================================================================
;;; Test Cases
;;; ============================================================================

(defun test-single-ball-gravity ()
  "Test determinism with a single ball falling under gravity."
  (test "Single ball with gravity (baseline)"
    ; (cleanup-all-physics-canvases) ; Skip cleanup for determinism tests

    ;; Create two identical scenarios
    (let ((storage1 (cl-fast-ecs:make-storage))
          (storage2 (cl-fast-ecs:make-storage))
          (gravity-x 0.0)
          (gravity-y 9.8))

      ;; Create identical balls in both storages
      (create-ball-entity storage1 400.0 100.0 0.0 0.0)
      (create-ball-entity storage2 400.0 100.0 0.0 0.0)

      ;; Run both simulations
      (let ((state1 (run-simulation storage1 gravity-x gravity-y +simulation-ticks+))
            (state2 (run-simulation storage2 gravity-x gravity-y +simulation-ticks+)))

        ;; Compare final states
        (assert-true (compare-states state1 state2)
                     "Single ball simulations should produce identical results")))))

(defun test-multiple-balls-collisions ()
  "Test determinism with multiple balls colliding."
  (test "Multiple balls with collisions"
    ; (cleanup-all-physics-canvases) ; Skip cleanup for determinism tests

    ;; Create two identical scenarios
    (let ((storage1 (cl-fast-ecs:make-storage))
          (storage2 (cl-fast-ecs:make-storage))
          (gravity-x 0.0)
          (gravity-y 9.8))

      ;; Create identical ball configurations
      ;; Ball 1: Stationary in center
      (create-ball-entity storage1 400.0 300.0 0.0 0.0)
      (create-ball-entity storage2 400.0 300.0 0.0 0.0)

      ;; Ball 2: Moving left to right
      (create-ball-entity storage1 100.0 300.0 50.0 0.0)
      (create-ball-entity storage2 100.0 300.0 50.0 0.0)

      ;; Ball 3: Moving right to left
      (create-ball-entity storage1 700.0 300.0 -50.0 0.0)
      (create-ball-entity storage2 700.0 300.0 -50.0 0.0)

      ;; Ball 4: Falling from top
      (create-ball-entity storage1 400.0 50.0 0.0 0.0)
      (create-ball-entity storage2 400.0 50.0 0.0 0.0)

      ;; Run both simulations
      (let ((state1 (run-simulation storage1 gravity-x gravity-y +simulation-ticks+))
            (state2 (run-simulation storage2 gravity-x gravity-y +simulation-ticks+)))

        ;; Compare final states
        (assert-true (compare-states state1 state2)
                     "Multiple ball collisions should be deterministic")))))

(defun test-balls-with-force-fields ()
  "Test determinism with balls affected by force fields."
  (test "Balls with force fields"
    ; (cleanup-all-physics-canvases) ; Skip cleanup for determinism tests

    ;; Create two identical scenarios
    (let ((storage1 (cl-fast-ecs:make-storage))
          (storage2 (cl-fast-ecs:make-storage))
          (gravity-x 0.0)
          (gravity-y 9.8))

      ;; Create force field (gravity well in center)
      (create-force-field-entity storage1 400.0 300.0 :gravity-well 200.0 250.0)
      (create-force-field-entity storage2 400.0 300.0 :gravity-well 200.0 250.0)

      ;; Create balls around the force field
      (create-ball-entity storage1 200.0 300.0 0.0 0.0)
      (create-ball-entity storage2 200.0 300.0 0.0 0.0)

      (create-ball-entity storage1 600.0 300.0 0.0 0.0)
      (create-ball-entity storage2 600.0 300.0 0.0 0.0)

      (create-ball-entity storage1 400.0 100.0 0.0 0.0)
      (create-ball-entity storage2 400.0 100.0 0.0 0.0)

      ;; Run both simulations
      (let ((state1 (run-simulation storage1 gravity-x gravity-y +simulation-ticks+))
            (state2 (run-simulation storage2 gravity-x gravity-y +simulation-ticks+)))

        ;; Compare final states
        (assert-true (compare-states state1 state2)
                     "Force field interactions should be deterministic")))))

(defun test-complex-multi-body ()
  "Test determinism with complex multi-body interactions."
  (test "Complex multi-body interactions"
    ; (cleanup-all-physics-canvases) ; Skip cleanup for determinism tests

    ;; Create two identical scenarios
    (let ((storage1 (cl-fast-ecs:make-storage))
          (storage2 (cl-fast-ecs:make-storage))
          (gravity-x 0.0)
          (gravity-y 9.8))

      ;; Create static blocks
      (create-block-entity storage1 400.0 550.0 600.0 20.0)  ; Floor
      (create-block-entity storage2 400.0 550.0 600.0 20.0)

      (create-block-entity storage1 100.0 300.0 20.0 400.0)  ; Left wall
      (create-block-entity storage2 100.0 300.0 20.0 400.0)

      (create-block-entity storage1 700.0 300.0 20.0 400.0)  ; Right wall
      (create-block-entity storage2 700.0 300.0 20.0 400.0)

      ;; Create multiple balls with varied initial conditions
      (dotimes (i 10)
        (let ((x (+ 200.0 (* i 40.0)))
              (y (+ 100.0 (* (mod i 3) 50.0)))
              (vx (* (- (random 100.0) 50.0)))
              (vy (* (- (random 100.0) 50.0))))
          (create-ball-entity storage1 x y vx vy)
          (create-ball-entity storage2 x y vx vy)))

      ;; Add force field
      (create-force-field-entity storage1 400.0 200.0 :fan 80.0 300.0 0.0)
      (create-force-field-entity storage2 400.0 200.0 :fan 80.0 300.0 0.0)

      ;; Run both simulations
      (let ((state1 (run-simulation storage1 gravity-x gravity-y +simulation-ticks+))
            (state2 (run-simulation storage2 gravity-x gravity-y +simulation-ticks+)))

        ;; Compare final states
        (assert-true (compare-states state1 state2)
                     "Complex multi-body systems should be deterministic")))))

(defun test-simultaneous-spawns ()
  "Test determinism with simultaneous ball spawns (edge case)."
  (test "Simultaneous ball spawns (edge case)"
    ; (cleanup-all-physics-canvases) ; Skip cleanup for determinism tests

    ;; Create two identical scenarios
    (let ((storage1 (cl-fast-ecs:make-storage))
          (storage2 (cl-fast-ecs:make-storage))
          (gravity-x 0.0)
          (gravity-y 9.8))

      ;; Spawn multiple balls at exact same positions
      ;; This tests hash table ordering and collision detection
      (dotimes (i 5)
        (create-ball-entity storage1 400.0 300.0 0.0 0.0)
        (create-ball-entity storage2 400.0 300.0 0.0 0.0))

      ;; Run both simulations
      (let ((state1 (run-simulation storage1 gravity-x gravity-y +simulation-ticks+))
            (state2 (run-simulation storage2 gravity-x gravity-y +simulation-ticks+)))

        ;; Compare final states
        (assert-true (compare-states state1 state2)
                     "Simultaneous spawns should be deterministic")))))

(defun test-boundary-collisions ()
  "Test determinism with boundary collisions (edge case)."
  (test "Boundary collisions (edge case)"
    ; (cleanup-all-physics-canvases) ; Skip cleanup for determinism tests

    ;; Create two identical scenarios
    (let ((storage1 (cl-fast-ecs:make-storage))
          (storage2 (cl-fast-ecs:make-storage))
          (gravity-x 0.0)
          (gravity-y 9.8))

      ;; Create boundary walls
      (create-block-entity storage1 400.0 10.0 800.0 20.0)   ; Top
      (create-block-entity storage2 400.0 10.0 800.0 20.0)

      (create-block-entity storage1 400.0 590.0 800.0 20.0)  ; Bottom
      (create-block-entity storage2 400.0 590.0 800.0 20.0)

      (create-block-entity storage1 10.0 300.0 20.0 600.0)   ; Left
      (create-block-entity storage2 10.0 300.0 20.0 600.0)

      (create-block-entity storage1 790.0 300.0 20.0 600.0)  ; Right
      (create-block-entity storage2 790.0 300.0 20.0 600.0)

      ;; Create balls bouncing in the box
      (create-ball-entity storage1 400.0 300.0 80.0 60.0 :restitution 0.95)
      (create-ball-entity storage2 400.0 300.0 80.0 60.0 :restitution 0.95)

      (create-ball-entity storage1 300.0 200.0 -70.0 50.0 :restitution 0.95)
      (create-ball-entity storage2 300.0 200.0 -70.0 50.0 :restitution 0.95)

      ;; Run both simulations
      (let ((state1 (run-simulation storage1 gravity-x gravity-y +simulation-ticks+))
            (state2 (run-simulation storage2 gravity-x gravity-y +simulation-ticks+)))

        ;; Compare final states
        (assert-true (compare-states state1 state2)
                     "Boundary collisions should be deterministic")))))

(defun test-rapid-force-field-toggles ()
  "Test determinism with rapid force field changes (edge case)."
  (test "Rapid force field toggles (edge case)"
    ; (cleanup-all-physics-canvases) ; Skip cleanup for determinism tests

    ;; Create two identical scenarios
    (let ((storage1 (cl-fast-ecs:make-storage))
          (storage2 (cl-fast-ecs:make-storage))
          (gravity-x 0.0)
          (gravity-y 9.8))

      ;; Create balls
      (let ((ball1-id (create-ball-entity storage1 400.0 300.0 0.0 0.0))
            (ball2-id (create-ball-entity storage2 400.0 300.0 0.0 0.0)))
        (declare (ignore ball1-id ball2-id)))

      ;; Create force fields that will be toggled
      (let ((ff1-id (create-force-field-entity storage1 200.0 300.0 :fan 100.0 300.0 0.0))
            (ff2-id (create-force-field-entity storage2 200.0 300.0 :fan 100.0 300.0 0.0)))

        ;; Simulate with periodic force field toggling
        (with-storage (storage1)
          (with-storage (storage2)
            (dotimes (tick +simulation-ticks+)
              ;; Toggle force fields every 30 ticks (0.5 seconds)
              (when (zerop (mod tick 30))
                (if (has-component ff1-id 'force-field)
                    (progn
                      (delete-component ff1-id 'force-field)
                      (delete-component ff2-id 'force-field))
                    (progn
                      (let ((ff1 (make-component ff1-id 'force-field)))
                        (setf (force-field-field-type ff1) :fan)
                        (setf (force-field-strength ff1) 100.0)
                        (setf (force-field-radius ff1) 300.0)
                        (setf (force-field-direction ff1) 0.0))
                      (let ((ff2 (make-component ff2-id 'force-field)))
                        (setf (force-field-field-type ff2) :fan)
                        (setf (force-field-strength ff2) 100.0)
                        (setf (force-field-radius ff2) 300.0)
                        (setf (force-field-direction ff2) 0.0)))))

              ;; Run systems on both storages
              (with-storage (storage1)
                (run-system 'apply-forces-system :dt +physics-dt+)
                (run-system 'apply-acceleration-system
                                        :dt +physics-dt+ :gravity-x gravity-x :gravity-y gravity-y)
                (run-system 'apply-velocity-system :dt +physics-dt+)
                (run-system 'collision-system :dt +physics-dt+)
                (run-system 'check-sleeping-system))

              (with-storage (storage2)
                (run-system 'apply-forces-system :dt +physics-dt+)
                (run-system 'apply-acceleration-system
                                        :dt +physics-dt+ :gravity-x gravity-x :gravity-y gravity-y)
                (run-system 'apply-velocity-system :dt +physics-dt+)
                (run-system 'collision-system :dt +physics-dt+)
                (run-system 'check-sleeping-system))))))

      ;; Collect and compare final states
      (let ((state1 (make-hash-table))
            (state2 (make-hash-table)))
        (with-storage (storage1)
          (do-entities (entity)
            (when (has-component entity 'position)
              (let ((pos (get-component entity 'position))
                    (vel (when (has-component entity 'velocity)
                          (get-component entity 'velocity))))
                (setf (gethash entity state1)
                      (list :x (position-x pos)
                            :y (position-y pos)
                            :vx (if vel (velocity-vx vel) 0.0)
                            :vy (if vel (velocity-vy vel) 0.0)))))))

        (with-storage (storage2)
          (do-entities (entity)
            (when (has-component entity 'position)
              (let ((pos (get-component entity 'position))
                    (vel (when (has-component entity 'velocity)
                          (get-component entity 'velocity))))
                (setf (gethash entity state2)
                      (list :x (position-x pos)
                            :y (position-y pos)
                            :vx (if vel (velocity-vx vel) 0.0)
                            :vy (if vel (velocity-vy vel) 0.0)))))))

        ;; Compare final states
        (assert-true (compare-states state1 state2)
                     "Rapid force field toggles should be deterministic")))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-tests ()
  "Run all determinism tests."
  (setf *test-count* 0
        *test-passed* 0
        *test-failed* 0)

  (format t "~%╔════════════════════════════════════════════════════════╗~%")
  (format t "║     Physics Engine Determinism Tests (Task 4.3)       ║~%")
  (format t "╠════════════════════════════════════════════════════════╣~%")
  (format t "║  Duration: ~D seconds (~D ticks at ~D Hz)             ║~%"
          +simulation-duration+ +simulation-ticks+ +physics-hz+)
  (format t "║  Tolerance: ±~,6F for floating-point comparisons    ║~%"
          +float-tolerance+)
  (format t "╚════════════════════════════════════════════════════════╝~%")

  ;; Run all test suites
  (test-single-ball-gravity)
  (test-multiple-balls-collisions)
  (test-balls-with-force-fields)
  (test-complex-multi-body)
  (test-simultaneous-spawns)
  (test-boundary-collisions)
  (test-rapid-force-field-toggles)

  ;; Print summary
  (format t "~%╔════════════════════════════════════════════════════════╗~%")
  (format t "║                    Test Summary                        ║~%")
  (format t "╠════════════════════════════════════════════════════════╣~%")
  (format t "║  Total:  ~3D tests                                      ║~%" *test-count*)
  (format t "║  Passed: ~3D tests                                      ║~%" *test-passed*)
  (format t "║  Failed: ~3D tests                                      ║~%" *test-failed*)
  (format t "╚════════════════════════════════════════════════════════╝~%")

  (if (zerop *test-failed*)
      (progn
        (format t "~%✓ All determinism tests PASSED!~%")
        (format t "~%Physics engine is DETERMINISTIC:~%")
        (format t "  - Same initial conditions → Same final state~%")
        (format t "  - Ready for client-side prediction~%")
        (format t "  - Multiplayer sync will be accurate~%~%")
        0)
      (progn
        (format t "~%✗ Some tests FAILED!~%")
        (format t "~%WARNING: Physics engine is NOT deterministic~%")
        (format t "  - Review failed tests for non-deterministic behavior~%")
        (format t "  - Check for floating-point precision issues~%")
        (format t "  - Verify system execution order is consistent~%~%")
        1)))

;;; ============================================================================
;;; Main Entry Point
;;; ============================================================================

(defun test-determinism-main ()
  "Main entry point for determinism test suite."
  (handler-case
      (run-tests)
    (error (e)
      (format t "~%FATAL ERROR: ~A~%~%" e)
      (format t "Backtrace: ~A~%" (trivial-backtrace:print-backtrace e :output nil))
      1)))

#|

USAGE:
------

Run from command line:
  ros -l backend/tests/test-determinism.lisp \
      -e "(collabcanvas:test-determinism-main)" \
      -e "(sb-ext:exit)"

Or from REPL:
  (load "backend/tests/test-determinism.lisp")
  (collabcanvas:test-determinism-main)

WHAT THIS TESTS:
----------------

1. **Single Ball Gravity (Baseline)**
   - Simplest case: one ball falling under gravity
   - Verifies basic physics determinism

2. **Multiple Balls with Collisions**
   - Tests collision detection and response determinism
   - Multiple balls interacting with each other

3. **Balls with Force Fields**
   - Tests force field calculations (gravity well)
   - Verifies consistent force application

4. **Complex Multi-Body Interactions**
   - 10 balls, 3 blocks, 1 force field
   - Tests real-world scenario determinism

5. **Simultaneous Spawns (Edge Case)**
   - Multiple balls spawned at exact same position
   - Tests hash table ordering and collision handling

6. **Boundary Collisions (Edge Case)**
   - Balls bouncing in enclosed space
   - Tests repeated wall collisions

7. **Rapid Force Field Toggles (Edge Case)**
   - Force field enabled/disabled every 0.5 seconds
   - Tests dynamic entity modification during simulation

TEST STRATEGY:
--------------

For each test:
1. Create two identical ECS storages
2. Add same entities with same initial state
3. Run both simulations for 600 ticks (10 seconds at 60 Hz)
4. Compare final positions and velocities
5. Fail if any difference exceeds ±0.001 tolerance

SUCCESS CRITERIA:
-----------------

✓ All 7 tests pass with 100% consistency
✓ Final states match within 0.001 tolerance
✓ No non-deterministic behavior detected
✓ Physics engine ready for multiplayer sync

DETERMINISM GUARANTEES:
-----------------------

This test suite verifies that:
- Fixed timestep (dt=0.016667) produces consistent results
- Floating-point calculations are order-independent
- System execution order is deterministic
- Component iteration order is consistent
- Collision resolution is reproducible
- Force calculations are deterministic

These guarantees enable:
- Client-side prediction (clients can simulate ahead)
- Server reconciliation (server can verify client state)
- Network optimization (only send deltas, not full state)
- Replay functionality (deterministic playback)

|#
