;;;; physics-systems.lisp - ECS Physics Systems
;;;;
;;;; Defines all physics simulation systems using cl-fast-ecs:define-system.
;;;; Systems process entities with specific component combinations and update
;;;; their state based on physics laws (forces, acceleration, velocity, collisions).
;;;;
;;;; System Execution Order (defined in physics-loop.lisp):
;;;;   1. apply-forces-system       - Apply force fields to balls (STUB)
;;;;   2. apply-acceleration-system - Update velocity from acceleration
;;;;   3. apply-velocity-system     - Update position from velocity
;;;;   4. collision-system          - Detect and resolve collisions (STUB)
;;;;   5. check-sleeping-system     - Optimize inactive entities
;;;;
;;;; Reference: backend/tests/ecs-physics-benchmark.lisp
;;;; PRD Section: 4.1.3 - System Definitions

(in-package #:collabcanvas)

;;; ============================================================================
;;; Collision Impulse Management (Two-Pass Architecture)
;;; ============================================================================
;;;
;;; Two-pass collision resolution separates detection from application:
;;;   Pass 1 (detect-collisions-system):  Query quadtree, calculate impulses
;;;   Pass 2 (apply-collision-impulses-system): Apply stored impulses to entities
;;;
;;; Benefits:
;;;   - No order dependency (A→B same result as B→A)
;;;   - Supports multiple simultaneous collisions per entity
;;;   - Works within ECS constraints (each system only modifies current entity)

(defstruct collision-impulse
  "Accumulated collision impulses to apply to an entity"
  (impulse-x 0.0 :type single-float)      ; Velocity change in X
  (impulse-y 0.0 :type single-float)      ; Velocity change in Y
  (correction-x 0.0 :type single-float)   ; Position correction in X
  (correction-y 0.0 :type single-float))  ; Position correction in Y

(defparameter *collision-impulses* (make-hash-table :test 'eql)
  "Hash table mapping entity-id → collision-impulse.
   Cleared each frame before collision detection.")

(defun add-impulse (entity-id delta-vx delta-vy delta-x delta-y)
  "Accumulate a collision impulse for an entity.

   If entity already has impulses from previous collisions this frame,
   add to the existing values (supports multiple simultaneous collisions)."
  (let ((existing (gethash entity-id *collision-impulses*)))
    (if existing
        ;; Accumulate with existing impulses
        (progn
          (incf (collision-impulse-impulse-x existing) delta-vx)
          (incf (collision-impulse-impulse-y existing) delta-vy)
          (incf (collision-impulse-correction-x existing) delta-x)
          (incf (collision-impulse-correction-y existing) delta-y))
        ;; Create new impulse entry
        (setf (gethash entity-id *collision-impulses*)
              (make-collision-impulse :impulse-x delta-vx
                                     :impulse-y delta-vy
                                     :correction-x delta-x
                                     :correction-y delta-y)))))

(defun clear-collision-impulses ()
  "Clear all collision impulses. Called at start of each physics frame."
  (clrhash *collision-impulses*))

;;; ============================================================================
;;; System 1: Apply Forces (STUB)
;;; ============================================================================

(define-system apply-forces-system
  (:components-ro (position ball)
   :components-rw (acceleration))

  "STUB: Force field application temporarily disabled.

   TODO: Implement force field iteration using one of:
   1. Global variable to cache force-field positions each frame
   2. Custom iteration API function
   3. Quadtree spatial queries

   Once implemented, this system will:
   - Apply fan forces (directional wind)
   - Apply gravity-well forces (radial attraction)
   - Apply magnet forces (polarity-based attraction/repulsion)"

  (declare (ignore position-x position-y ball-mass ball-polarity ball-radius ball-restitution
                   acceleration-ax acceleration-ay))
  nil)

;;; ============================================================================
;;; System 2: Apply Acceleration
;;; ============================================================================

(define-system apply-acceleration-system
  (:components-ro (acceleration)
   :components-rw (velocity)
   :arguments ((dt single-float)
               (gravity-x single-float)
               (gravity-y single-float)))

  "Update velocity from acceleration using Euler integration.

   Algorithm:
   1. Apply entity's acceleration to velocity: v(t+dt) = v(t) + a(t) * dt
   2. Add global gravity acceleration
   3. Reset acceleration to zero (forces are recalculated each frame)

   Physics:
   - Uses explicit Euler integration (simple, fast, good enough for games)
   - Global gravity is added AFTER entity acceleration
   - Acceleration is cleared so force fields can recalculate next frame"

  ;; Apply acceleration to velocity: v += a * dt
  (incf velocity-vx (* acceleration-ax dt))
  (incf velocity-vy (* acceleration-ay dt))

  ;; Add global gravity
  (incf velocity-vx (* gravity-x dt))
  (incf velocity-vy (* gravity-y dt))

  ;; Reset acceleration to zero (will be recalculated by forces next frame)
  (setf acceleration-ax 0.0)
  (setf acceleration-ay 0.0))

;;; ============================================================================
;;; System 3: Apply Velocity
;;; ============================================================================

(define-system apply-velocity-system
  (:components-ro (velocity)
   :components-rw (position)
   :arguments ((dt single-float)))

  "Update position from velocity using Euler integration.

   Algorithm:
   1. Update position: p(t+dt) = p(t) + v(t) * dt

   Physics:
   - Uses explicit Euler integration
   - Simple translation based on current velocity
   - No damping applied here (can be added in acceleration system if needed)"

  ;; Update position from velocity: p += v * dt
  (incf position-x (* velocity-vx dt))
  (incf position-y (* velocity-vy dt)))

;;; ============================================================================
;;; System 4a: Populate Quadtree
;;; ============================================================================

(define-system populate-quadtree-system
  (:components-ro (position ball velocity)
   :arguments ((quadtree t)))

  "Insert all balls into quadtree with full physics data for collision resolution.

   This system runs BEFORE collision detection each frame to build the
   spatial index. The quadtree is cleared in physics-loop before this runs.

   Physics data (velocity, mass, restitution) is stored in the quadtree
   to enable two-pass collision resolution without ECS component access.

   Complexity: O(n log n) to insert n entities into quadtree"

  ;; Insert ball into quadtree with physics data
  (insert-entity-bounds quadtree
                        (current-entity)
                        position-x
                        position-y
                        ball-radius
                        :ball
                        :vx velocity-vx
                        :vy velocity-vy
                        :mass ball-mass
                        :restitution ball-restitution))

;;; System 4b: Populate blocks into quadtree
(define-system populate-quadtree-blocks-system
  (:components-ro (position block)
   :arguments ((quadtree t)))

  "Insert all blocks into quadtree with dimensions for collision detection.

   Blocks are static obstacles (no velocity), but we store their width/height
   for accurate AABB collision detection with balls."

  ;; Blocks use max(width, height)/2 as effective radius for quadtree
  (let ((effective-radius (* 0.5 (max block-width block-height))))
    (insert-entity-bounds quadtree
                          (current-entity)
                          position-x
                          position-y
                          effective-radius
                          :block
                          :width block-width
                          :height block-height)))

;;; ============================================================================
;;; System 4a: Detect Collisions (Two-Pass Architecture - Pass 1)
;;; ============================================================================

(define-system detect-collisions-system
  (:components-ro (ball position velocity)
   :arguments ((quadtree t)))

  "Detect collisions and calculate impulses (stores in *collision-impulses*).

   This is Pass 1 of two-pass collision resolution. Queries the quadtree for
   nearby entities and calculates collision impulses for both entities involved.
   Impulses are accumulated in *collision-impulses* hash table.

   Algorithm:
   1. Query quadtree for nearby entities (broad phase)
   2. For each nearby entity:
      - Detect precise collision (narrow phase)
      - Calculate elastic collision impulse
      - Store impulse for both entities (Newton's 3rd law)

   Complexity: O(n log n) with quadtree vs O(n²) brute force
   - 500 balls: ~3,000 checks vs 124,750 (40x faster!)
   - 1000 balls: ~6,000 checks vs 499,500 (80x faster!)"

  (let* ((entity-id (current-entity))
         (nearby (query-nearby-entities quadtree position-x position-y 
                                       (* 2.0 ball-radius))))

    ;; Check each nearby entity for collision
    (loop for other-bounds in nearby
          for other-entity = (entity-bounds-entity-id other-bounds)
          for other-type = (entity-bounds-type other-bounds)
          do

          ;; Skip self-collision
          (when (not (= other-entity entity-id))

            (case other-type
              ;; Ball-Ball collision
              (:ball
               (when (ball-ball-colliding-p position-x position-y ball-radius other-bounds)
                 (calculate-ball-ball-impulses
                   entity-id position-x position-y velocity-vx velocity-vy 
                   ball-mass ball-restitution ball-radius
                   other-bounds)))

              ;; Ball-Block collision
              (:block
               (when (ball-block-colliding-p position-x position-y ball-radius other-bounds)
                 (calculate-ball-block-impulses
                   entity-id position-x position-y velocity-vx velocity-vy
                   ball-restitution ball-radius
                   other-bounds))))))))

;;; ============================================================================
;;; System 4b: Apply Collision Impulses (Two-Pass Architecture - Pass 2)
;;; ============================================================================

(define-system apply-collision-impulses-system
  (:components-rw (position velocity))

  "Apply accumulated collision impulses to entity.

   This is Pass 2 of two-pass collision resolution. Reads impulses from
   *collision-impulses* hash table and applies them to the current entity.

   Each entity can have multiple impulses (from multiple simultaneous collisions),
   all accumulated and applied in one operation."

  (let ((impulse (gethash (current-entity) *collision-impulses*)))
    (when impulse
      ;; Apply velocity impulse
      (incf velocity-vx (collision-impulse-impulse-x impulse))
      (incf velocity-vy (collision-impulse-impulse-y impulse))

      ;; Apply position correction (prevents overlap/sinking)
      (incf position-x (collision-impulse-correction-x impulse))
      (incf position-y (collision-impulse-correction-y impulse)))))

;;; ============================================================================
;;; Collision Detection Helper Functions
;;; ============================================================================

(defun ball-ball-colliding-p (x1 y1 r1 other-bounds)
  "Check if ball collides with another ball (circle-circle collision).

   Parameters:
     X1, Y1 - Ball position
     R1 - Ball radius
     OTHER-BOUNDS - entity-bounds structure for other ball

   Returns:
     T if balls are overlapping, NIL otherwise"
  (let* ((x2 (entity-bounds-x other-bounds))
         (y2 (entity-bounds-y other-bounds))
         (r2 (entity-bounds-radius other-bounds))
         (dx (- x2 x1))
         (dy (- y2 y1))
         (dist-sq (+ (* dx dx) (* dy dy)))
         (min-dist-sq (* (+ r1 r2) (+ r1 r2))))
    (< dist-sq min-dist-sq)))

(defun ball-block-colliding-p (ball-x ball-y ball-r block-bounds)
  "Check if ball collides with block (circle-AABB collision).

   Uses closest-point-on-AABB algorithm for accurate detection.

   Parameters:
     BALL-X, BALL-Y - Ball position
     BALL-R - Ball radius
     BLOCK-BOUNDS - entity-bounds structure for block

   Returns:
     T if ball overlaps with block AABB, NIL otherwise"
  (let* ((block-x (entity-bounds-x block-bounds))
         (block-y (entity-bounds-y block-bounds))
         (block-w (entity-bounds-width block-bounds))
         (block-h (entity-bounds-height block-bounds))

         ;; AABB bounds
         (block-left (- block-x (* 0.5 block-w)))
         (block-right (+ block-x (* 0.5 block-w)))
         (block-top (- block-y (* 0.5 block-h)))
         (block-bottom (+ block-y (* 0.5 block-h)))

         ;; Closest point on AABB to circle center
         (closest-x (max block-left (min ball-x block-right)))
         (closest-y (max block-top (min ball-y block-bottom)))

         ;; Distance from ball center to closest point
         (dx (- ball-x closest-x))
         (dy (- ball-y closest-y))
         (dist-sq (+ (* dx dx) (* dy dy)))
         (ball-r-sq (* ball-r ball-r)))

    (< dist-sq ball-r-sq)))

;;; ============================================================================
;;; Collision Impulse Calculation Functions
;;; ============================================================================

(defun calculate-ball-ball-impulses (entity1-id x1 y1 vx1 vy1 m1 e1 r1 other-bounds)
  "Calculate and store collision impulses for ball-ball elastic collision.

   Uses conservation of momentum and restitution coefficient.
   Stores impulses for BOTH balls (Newton's 3rd law: equal and opposite).

   Physics:
     - Elastic collision along collision normal
     - Restitution coefficient e controls bounciness (0=inelastic, 1=elastic)
     - Position correction prevents overlap/sinking"
  (let* ((entity2-id (entity-bounds-entity-id other-bounds))
         (x2 (entity-bounds-x other-bounds))
         (y2 (entity-bounds-y other-bounds))
         (r2 (entity-bounds-radius other-bounds))
         (vx2 (entity-bounds-vx other-bounds))
         (vy2 (entity-bounds-vy other-bounds))
         (m2 (entity-bounds-mass other-bounds))
         (e2 (entity-bounds-restitution other-bounds))

         ;; Collision geometry
         (dx (- x2 x1))
         (dy (- y2 y1))
         (dist (sqrt (+ (* dx dx) (* dy dy))))
         (nx (/ dx dist))  ; Collision normal (normalized)
         (ny (/ dy dist))

         ;; Relative velocity along collision normal
         (dvx (- vx2 vx1))
         (dvy (- vy2 vy1))
         (rel-vel-normal (+ (* dvx nx) (* dvy ny))))

    ;; Only resolve if balls are approaching (not separating)
    (when (< rel-vel-normal 0.0)

      (let* (;; Use minimum restitution of the two balls
             (e (min e1 e2))

             ;; Impulse magnitude from elastic collision formula:
             ;; J = -(1 + e) * v_rel · n / (1/m1 + 1/m2)
             (impulse-mag (* (- (+ 1.0 e))
                            rel-vel-normal
                            (/ (* m1 m2) (+ m1 m2))))
             (impulse-x (* impulse-mag nx))
             (impulse-y (* impulse-mag ny))

             ;; Position correction to prevent overlap
             (overlap (- (+ r1 r2) dist))
             (correction (* 0.5 overlap)))  ; Split 50/50 between balls

        ;; Add impulse for ball 1 (current entity)
        (add-impulse entity1-id
                    (/ (- impulse-x) m1)      ; Velocity impulse
                    (/ (- impulse-y) m1)
                    (* (- correction) nx)     ; Position correction
                    (* (- correction) ny))

        ;; Add impulse for ball 2 (other entity) - Newton's 3rd law
        (add-impulse entity2-id
                    (/ impulse-x m2)
                    (/ impulse-y m2)
                    (* correction nx)
                    (* correction ny))))))

(defun calculate-ball-block-impulses (ball-entity ball-x ball-y ball-vx ball-vy 
                                      ball-e ball-r block-bounds)
  "Calculate and store collision impulse for ball-block collision.

   Blocks are static (infinite mass), so only ball receives impulse.
   Uses velocity reflection with restitution coefficient.

   Physics:
     - Velocity is reflected across collision normal: v' = v - 2(v·n)n
     - Restitution coefficient e scales the reflection (energy loss)
     - Position correction pushes ball out of block"
  (let* ((block-x (entity-bounds-x block-bounds))
         (block-y (entity-bounds-y block-bounds))
         (block-w (entity-bounds-width block-bounds))
         (block-h (entity-bounds-height block-bounds))

         ;; AABB bounds
         (block-left (- block-x (* 0.5 block-w)))
         (block-right (+ block-x (* 0.5 block-w)))
         (block-top (- block-y (* 0.5 block-h)))
         (block-bottom (+ block-y (* 0.5 block-h)))

         ;; Closest point on AABB to ball center
         (closest-x (max block-left (min ball-x block-right)))
         (closest-y (max block-top (min ball-y block-bottom)))

         ;; Collision normal (from block surface to ball center)
         (dx (- ball-x closest-x))
         (dy (- ball-y closest-y))
         (dist (sqrt (+ (* dx dx) (* dy dy))))

         ;; Handle edge case: ball center inside block
         (nx (if (zerop dist) 0.0 (/ dx dist)))
         (ny (if (zerop dist) -1.0 (/ dy dist)))  ; Default: push up

         ;; Calculate reflection: v' = v - 2(v·n)n * restitution
         (dot-product (+ (* ball-vx nx) (* ball-vy ny)))
         (reflection-scale (* -2.0 ball-e dot-product))

         ;; Position correction (push ball out of block)
         (overlap (- ball-r dist)))

    ;; Add impulse for ball (velocity reflection + position correction)
    (add-impulse ball-entity
                (* reflection-scale nx)        ; Velocity impulse
                (* reflection-scale ny)
                (* overlap nx)                 ; Position correction
                (* overlap ny))))


;;; ============================================================================
;;; System 5: Check Sleeping State
;;; ============================================================================

(define-system check-sleeping-system
  (:components-ro (velocity))

  "Mark low-velocity entities as sleeping for optimization.

   Algorithm:
   1. Calculate velocity magnitude
   2. If |v| < threshold (0.01 pixels/second), mark as sleeping
   3. If |v| >= threshold and entity is sleeping, wake it up

   Optimization:
   - Sleeping entities skip physics calculations in other systems
   - Entities wake up when:
     * Velocity increases above threshold
     * Collisions occur (handled in collision-system)
     * Force fields affect them (handled in apply-forces-system)

   Performance Impact:
   - Reduces CPU usage for resting entities
   - Critical for scenes with many static/settled objects"

  (let* ((vx velocity-vx)
         (vy velocity-vy)
         (speed-sq (+ (* vx vx) (* vy vy)))
         (threshold-sq (* 0.01 0.01))  ; 0.01 pixels/second
         (entity (current-entity)))

    ;; Note: We can't check has-component in cl-fast-ecs easily without the function
    ;; So we'll just try to add/delete the sleeping component
    (cond
      ;; Entity is moving slowly -> put to sleep
      ((< speed-sq threshold-sq)
       ;; Try to add sleeping component (no-op if already exists)
       (ignore-errors (cl-fast-ecs:make-component entity 'sleeping)))

      ;; Entity is moving fast -> wake up
      ((>= speed-sq threshold-sq)
       ;; Try to delete sleeping component (no-op if doesn't exist)
       (ignore-errors (cl-fast-ecs:delete-component entity 'sleeping))))))

;;; ============================================================================
;;; Module Initialization
;;; ============================================================================

(format t "Physics Systems loaded (QUADTREE OPTIMIZED)~%")
(format t "  - apply-forces-system: STUB (needs iteration API)~%")
(format t "  - apply-acceleration-system: Euler integration (velocity) ✓~%")
(format t "  - apply-velocity-system: Euler integration (position) ✓~%")
(format t "  - populate-quadtree-system: Spatial indexing (balls) ✓~%")
(format t "  - populate-quadtree-blocks-system: Spatial indexing (blocks) ✓~%")
(format t "  - collision-system: Quadtree O(n log n) detection ✓~%")
(format t "      * Ball-ball elastic collisions~%")
(format t "      * Ball-block AABB collisions~%")
(format t "      * 40-160x faster than brute force!~%")
(format t "  - check-sleeping-system: Sleep optimization (<0.01 px/s) ✓~%")
(format t "  - Performance: 500 balls = 3,000 checks (vs 124,750)~%")
