;;;; physics-components.lisp - ECS Component Definitions for Physics Engine
;;;;
;;;; Defines all physics-related components using cl-fast-ecs:define-component
;;;; Components follow the Entity-Component-System (ECS) architecture pattern.
;;;;
;;;; Reference: backend/tests/ecs-physics-benchmark.lisp
;;;; PRD Section: 4.1.2 - Component Definitions

(in-package #:collabcanvas)

;;; ============================================================================
;;; Core Physics Components
;;; ============================================================================

(define-component position
  "World space position in pixels.
   Represents the center point of an entity in 2D canvas space."
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))

(define-component velocity
  "Velocity vector in pixels/second.
   Linear motion rate of change for position."
  (vx 0.0 :type single-float)
  (vy 0.0 :type single-float))

(define-component acceleration
  "Acceleration vector in pixels/second².
   Rate of change for velocity (e.g., gravity, drag)."
  (ax 0.0 :type single-float)
  (ay 0.0 :type single-float))

;;; ============================================================================
;;; Physics Body Components
;;; ============================================================================

(define-component ball
  "Dynamic circular physics body.

   Slots:
   - radius: Circle radius in pixels
   - mass: Body mass in arbitrary units (affects force response)
   - restitution: Bounciness coefficient (0.0 = no bounce, 1.0 = perfect bounce)
   - polarity: Magnetic polarity (+1, -1, or 0 for neutral)"
  (radius 10.0 :type single-float)
  (mass 1.0 :type single-float)
  (restitution 0.8 :type single-float)
  (polarity 0.0 :type single-float))

(define-component block
  "Static rectangular obstacle.

   Slots:
   - width: Rectangle width in pixels
   - height: Rectangle height in pixels

   Note: Position component defines the center point of the block."
  (width 50.0 :type single-float)
  (height 50.0 :type single-float))

;;; ============================================================================
;;; Force Field Component
;;; ============================================================================

(define-component force-field
  "Area-of-effect force that influences nearby balls.

   Slots:
   - field-type: Type of force field (:fan, :gravity-well, :magnet)
   - strength: Force magnitude (positive or negative)
   - radius: Area of influence in pixels (from position component)
   - direction: Direction angle in radians (for :fan type)
   - polarity: Magnetic polarity (+1 or -1, for :magnet type only)

   Field Types:
   - :fan         - Directional wind force (uses direction slot)
   - :gravity-well - Radial attraction force (pulls toward center)
   - :magnet      - Magnetic force (attracts or repels based on polarity)"
  (field-type :fan :type keyword)
  (strength 100.0 :type single-float)
  (radius 200.0 :type single-float)
  (direction 0.0 :type single-float)
  (polarity 1.0 :type single-float))

;;; ============================================================================
;;; Emitter Component (Post-MVP)
;;; ============================================================================

(define-component emitter
  "Ball emitter that spawns balls at regular intervals.

   Slots:
   - rate: Emission rate in balls per second (0.1 to 10.0)
   - direction: Emission direction angle in radians (0 = right, π/2 = down)
   - initial-velocity: Speed of emitted balls in pixels/second
   - last-emit-time: Last emission timestamp (internal, managed by system)
   - enabled: Whether emitter is currently active
   - ball-radius: Radius of spawned balls in pixels
   - ball-mass: Mass of spawned balls

   Example:
   - rate 2.0 = emit 2 balls per second (one every 0.5 seconds)
   - direction 0.0 = emit balls to the right
   - initial-velocity 200.0 = balls start with 200 px/s velocity"
  (rate 1.0 :type single-float)
  (direction 0.0 :type single-float)
  (initial-velocity 200.0 :type single-float)
  (last-emit-time 0.0 :type single-float)
  (enabled t :type boolean)
  (ball-radius 10.0 :type single-float)
  (ball-mass 1.0 :type single-float))

;;; ============================================================================
;;; Tag Components (Zero-Slot Components)
;;; ============================================================================

(define-component sleeping
  "Tag component indicating entity is at rest (optimization).

   When a ball has minimal velocity and acceleration, it's marked as sleeping
   to skip physics calculations. The sleep system will wake entities when:
   - External forces are applied
   - Nearby entities collide with it
   - Force fields affect it")

;;; ============================================================================
;;; Component Usage Notes
;;; ============================================================================

#|

CREATING ENTITIES WITH COMPONENTS:
-----------------------------------

;; Create a ball entity
(make-object
 '((:position :x 100.0 :y 100.0)
   (:velocity :vx 50.0 :vy -30.0)
   (:acceleration :ax 0.0 :ay 9.8)
   (:ball :radius 15.0 :mass 1.0 :restitution 0.85)))

;; Create a block entity
(make-object
 '((:position :x 400.0 :y 300.0)
   (:block :width 100.0 :height 50.0)))

;; Create a fan force field
(make-object
 '((:position :x 200.0 :y 200.0)
   (:force-field :field-type :fan
                 :strength 150.0
                 :radius 300.0
                 :direction 0.0)))  ; 0 radians = pointing right

;; Create a gravity well
(make-object
 '((:position :x 400.0 :y 300.0)
   (:force-field :field-type :gravity-well
                 :strength 500.0
                 :radius 250.0
                 :direction 0.0)))  ; direction unused for gravity wells

;; Create a sleeping ball
(make-object
 '((:position :x 50.0 :y 50.0)
   (:velocity :vx 0.0 :vy 0.0)
   (:acceleration :ax 0.0 :ay 0.0)
   (:ball :radius 10.0 :mass 1.0 :restitution 0.8)
   (:sleeping)))


COMPONENT COMBINATIONS:
------------------------

Valid combinations:
- Ball:        position + velocity + acceleration + ball [+ sleeping]
- Block:       position + block
- Force Field: position + force-field

Required components:
- All entities MUST have position component
- Dynamic entities (balls) MUST have velocity + acceleration + ball
- Static entities (blocks, force fields) have only position


TYPE SAFETY:
------------

All numeric slots use :type single-float for:
- Memory efficiency (4 bytes vs 8 bytes for double-float)
- SIMD optimization potential
- Sufficient precision for pixel-based physics (< 0.01 pixel error)


PERFORMANCE CONSIDERATIONS:
----------------------------

1. Component data is stored in contiguous arrays by cl-fast-ecs
2. Systems iterate efficiently over entities with specific component sets
3. Tag components (sleeping) have zero memory overhead per entity
4. Type declarations enable compiler optimizations (no boxing/unboxing)

|#
