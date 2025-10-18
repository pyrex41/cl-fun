;;;; physics-quadtree.lisp - Spatial Partitioning for Collision Detection
;;;;
;;;; Wraps takagi/quadtree library for efficient broad-phase collision detection.
;;;; Reduces collision checks from O(n²) to O(n log n) using spatial partitioning.

(in-package :collabcanvas)

;;; Ensure quadtree package is available at compile time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :quadtree))

;;; Entity Bounds Structure
;;; Stores spatial information for entities in the quadtree

(defstruct entity-bounds
  "Bounding box and physics data for an entity in the physics simulation.

   For two-pass collision resolution, stores all data needed to calculate
   collision impulses without accessing ECS components during detection."
  (entity-id nil :type (or null entity))
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (radius 0.0 :type single-float)
  (type :ball :type keyword)
  ;; Physics data for balls
  (vx 0.0 :type single-float)         ; Velocity X
  (vy 0.0 :type single-float)         ; Velocity Y
  (mass 1.0 :type single-float)       ; Mass (for impulse calculation)
  (restitution 0.8 :type single-float) ; Bounciness coefficient
  ;; Dimensions for blocks
  (width 0.0 :type single-float)      ; Block width (AABB)
  (height 0.0 :type single-float))    ; Block height (AABB)

;;; Quadtree Performance Stats

(defvar *quadtree-stats* (make-hash-table)
  "Hash table tracking quadtree performance metrics")

(defun reset-quadtree-stats ()
  "Reset quadtree performance statistics"
  (setf *quadtree-stats* (make-hash-table))
  (setf (gethash :inserts *quadtree-stats*) 0)
  (setf (gethash :queries *quadtree-stats*) 0)
  (setf (gethash :clears *quadtree-stats*) 0)
  (setf (gethash :total-entities-queried *quadtree-stats*) 0))

(defun get-quadtree-stats ()
  "Return current quadtree performance statistics"
  (list :inserts (gethash :inserts *quadtree-stats* 0)
        :queries (gethash :queries *quadtree-stats* 0)
        :clears (gethash :clears *quadtree-stats* 0)
        :total-entities-queried (gethash :total-entities-queried *quadtree-stats* 0)))

;; Initialize stats on load
(reset-quadtree-stats)

;;; Quadtree Wrapper Functions

(defun make-physics-quadtree (width height &key (max-depth 6) (max-capacity 10))
  "Create a quadtree for physics collision detection.

  Parameters:
    WIDTH - Canvas width in pixels
    HEIGHT - Canvas height in pixels
    MAX-DEPTH - Maximum tree depth (default 6, optimal for 500-2000 entities)
    MAX-CAPACITY - Max entities per node before split (default 10)

  Returns:
    A quadtree instance covering the canvas bounds"
  (quadtree:make 0.0 0.0
                 (float width 0.0)
                 (float height 0.0)
                 :max-depth max-depth
                 :max-capacity max-capacity))

(defun clear-physics-quadtree (quadtree)
  "Clear all entities from the quadtree (called every frame)"
  (incf (gethash :clears *quadtree-stats* 0))
  (quadtree:clear quadtree))

(defun insert-entity-bounds (quadtree entity-id x y radius entity-type
                            &key (vx 0.0) (vy 0.0) (mass 1.0) (restitution 0.8)
                                 (width 0.0) (height 0.0))
  "Insert an entity into the quadtree with physics data for collision resolution.

  Parameters:
    QUADTREE - The quadtree instance
    ENTITY-ID - ECS entity reference
    X, Y - Entity position (center point)
    RADIUS - Entity radius (for circles) or half-width (for rectangles)
    ENTITY-TYPE - :ball or :block
    VX, VY - Velocity components (for balls)
    MASS - Entity mass (for impulse calculation)
    RESTITUTION - Bounciness coefficient (0.0-1.0)
    WIDTH, HEIGHT - Dimensions (for blocks)

  Returns:
    The created entity-bounds structure"
  (incf (gethash :inserts *quadtree-stats* 0))
  (let ((bounds (make-entity-bounds :entity-id entity-id
                                    :x (float x 0.0)
                                    :y (float y 0.0)
                                    :radius (float radius 0.0)
                                    :type entity-type
                                    :vx (float vx 0.0)
                                    :vy (float vy 0.0)
                                    :mass (float mass 0.0)
                                    :restitution (float restitution 0.0)
                                    :width (float width 0.0)
                                    :height (float height 0.0))))
    ;; Insert the entity-bounds object directly into the quadtree
    (quadtree:insert quadtree bounds)
    bounds))

(defun query-nearby-entities (quadtree x y radius)
  "Query entities near a given position using point-based sampling.

  Queries the quadtree at the center point and uses neighbor-p flag to get
  entities from adjacent quadrants.

  Parameters:
    QUADTREE - The quadtree instance
    X, Y - Query center position
    RADIUS - Query radius (currently unused, for future distance filtering)

  Returns:
    List of entity-bounds structures for nearby entities"
  (declare (ignore radius)) ; For now, we just use neighbor-p
  (incf (gethash :queries *quadtree-stats* 0))

  ;; Query with neighbor-p=t to get entities from current and adjacent quadrants
  (let ((results (quadtree:query quadtree (float x 0.0) (float y 0.0) t)))
    (incf (gethash :total-entities-queried *quadtree-stats* 0) (length results))
    results))

(defun entities-potentially-collide-p (bounds1 bounds2)
  "Fast AABB (axis-aligned bounding box) prefilter for collision detection.

  Returns T if the AABBs of two entities overlap, NIL otherwise.
  This is a cheap broad-phase check before expensive circle-circle collision math.

  Parameters:
    BOUNDS1, BOUNDS2 - entity-bounds structures

  Returns:
    T if AABBs overlap (potential collision), NIL otherwise"
  (let ((x1 (entity-bounds-x bounds1))
        (y1 (entity-bounds-y bounds1))
        (r1 (entity-bounds-radius bounds1))
        (x2 (entity-bounds-x bounds2))
        (y2 (entity-bounds-y bounds2))
        (r2 (entity-bounds-radius bounds2)))
    ;; AABB overlap test: check if rectangles intersect
    ;; rect1: [x1-r1, x1+r1] × [y1-r1, y1+r1]
    ;; rect2: [x2-r2, x2+r2] × [y2-r2, y2+r2]
    (and (< (abs (- x1 x2)) (+ r1 r2))
         (< (abs (- y1 y2)) (+ r1 r2)))))

;;; Intersection Test for Entity Bounds
;;; Required by quadtree library to determine if object fits in quadtree boundary

(defmethod quadtree:intersect-p (quadtree (entity entity-bounds))
  "Determine if an entity-bounds object intersects with the quadtree boundary.

  This method is called by quadtree:insert to check if the object fits within
  the quadtree bounds.

  Parameters:
    QUADTREE - The quadtree instance
    ENTITY - entity-bounds structure (treated as circle)

  Returns:
    T if entity intersects quadtree boundary, NIL otherwise"
  (destructuring-bind (x0 y0 x1 y1) (quadtree:boundary quadtree)
    (let ((cx (entity-bounds-x entity))
          (cy (entity-bounds-y entity))
          (radius (entity-bounds-radius entity)))
      ;; Check if circle center is within expanded boundary
      ;; (allow entities partially outside to be stored)
      (and (<= (- x0 radius) cx (+ x1 radius))
           (<= (- y0 radius) cy (+ y1 radius))))))
