;;;; physics.lisp - 2D Physics Engine for CollabCanvas Living Canvas
;;;;
;;;; Implements a simple Verlet integration physics engine with:
;;;; - Circles as dynamic objects (affected by physics)
;;;; - Rectangles as static walls/platforms
;;;; - Mass proportional to radius (area-based)
;;;; - Global gravity
;;;; - Boundary constraints (bounce)
;;;; - Circle-circle and circle-rectangle collision detection

(in-package #:collabcanvas)

;;; Physics constants
(defparameter *physics-timestep* 0.02 "Fixed timestep: 50 steps/second")
(defparameter *default-gravity* 9.8 "Default gravity acceleration")
(defparameter *default-friction* 0.02 "Air friction coefficient")
(defparameter *default-restitution* 0.7 "Bounciness (0=dead, 1=perfectly elastic)")
(defparameter *canvas-width* 4000 "Physics simulation width")
(defparameter *canvas-height* 3000 "Physics simulation height")

;;; Physics object structure
(defstruct physics-object
  id              ; Object ID (string)
  type            ; 'circle or 'rectangle
  x               ; Current X position
  y               ; Current Y position
  old-x           ; Previous X (for Verlet integration)
  old-y           ; Previous Y
  vx              ; Velocity X (derived from position delta)
  vy              ; Velocity Y
  ax              ; Acceleration X
  ay              ; Acceleration Y
  radius          ; For circles
  width           ; For rectangles
  height          ; For rectangles
  mass            ; Mass (derived from size)
  is-dynamic      ; T if affected by physics, NIL if static
  restitution     ; Bounciness
  friction        ; Friction coefficient
  color)          ; Color (hex string)

;;; Global physics state
(defparameter *physics-objects* (make-hash-table :test 'equal)
  "Hash table of physics objects by ID")

(defparameter *global-gravity* *default-gravity*
  "Current global gravity value")

(defparameter *physics-paused* t
  "Whether physics simulation is paused")

(defparameter *boundary-mode* :contain
  "Boundary behavior: :contain (bounce) or :wrap (wrap-around)")

;;; Object creation and management

(defun create-physics-circle (id x y radius &key (is-dynamic t) (color "#3498db"))
  "Create a dynamic circle physics object"
  (let* ((mass (calculate-mass-from-radius radius))
         (obj (make-physics-object
               :id id
               :type 'circle
               :x x
               :y y
               :old-x x
               :old-y y
               :vx 0.0
               :vy 0.0
               :ax 0.0
               :ay 0.0
               :radius radius
               :mass mass
               :is-dynamic is-dynamic
               :restitution *default-restitution*
               :friction *default-friction*
               :color color)))
    (setf (gethash id *physics-objects*) obj)
    obj))

(defun create-physics-rectangle (id x y width height &key (color "#95a5a6"))
  "Create a static rectangle (wall/platform)"
  (let ((obj (make-physics-object
              :id id
              :type 'rectangle
              :x x
              :y y
              :old-x x
              :old-y y
              :vx 0.0
              :vy 0.0
              :ax 0.0
              :ay 0.0
              :width width
              :height height
              :mass 0.0 ; Static objects have infinite mass
              :is-dynamic nil ; Never moves
              :restitution 0.8
              :friction 0.1
              :color color)))
    (setf (gethash id *physics-objects*) obj)
    obj))

(defun remove-physics-object (id)
  "Remove an object from the physics simulation"
  (remhash id *physics-objects*))

(defun get-physics-object (id)
  "Get a physics object by ID"
  (gethash id *physics-objects*))

(defun clear-all-physics-objects ()
  "Remove all physics objects"
  (clrhash *physics-objects*))

;;; Mass calculation

(defun calculate-mass-from-radius (radius)
  "Calculate mass based on circle area (π * r²)"
  (* pi radius radius))

;;; Verlet integration

(defun integrate-verlet (obj dt)
  "Update position using Verlet integration"
  (when (physics-object-is-dynamic obj)
    (let* ((dt2 (* dt dt))
           ;; Calculate velocity from position delta (Verlet)
           (vx (- (physics-object-x obj) (physics-object-old-x obj)))
           (vy (- (physics-object-y obj) (physics-object-old-y obj)))

           ;; Apply friction (air resistance)
           (friction-factor (- 1.0 (physics-object-friction obj)))
           (vx (* vx friction-factor))
           (vy (* vy friction-factor))

           ;; Calculate new position
           (new-x (+ (physics-object-x obj) vx (* (physics-object-ax obj) dt2)))
           (new-y (+ (physics-object-y obj) vy (* (physics-object-ay obj) dt2))))

      ;; Store old position
      (setf (physics-object-old-x obj) (physics-object-x obj))
      (setf (physics-object-old-y obj) (physics-object-y obj))

      ;; Update position
      (setf (physics-object-x obj) new-x)
      (setf (physics-object-y obj) new-y)

      ;; Store velocity for external use
      (setf (physics-object-vx obj) (/ vx dt))
      (setf (physics-object-vy obj) (/ vy dt))

      ;; Reset acceleration
      (setf (physics-object-ax obj) 0.0)
      (setf (physics-object-ay obj) 0.0))))

;;; Forces

(defun apply-gravity (obj)
  "Apply global gravity force to object"
  (when (physics-object-is-dynamic obj)
    (incf (physics-object-ay obj) *global-gravity*)))

(defun apply-force (obj fx fy)
  "Apply a force to an object (F = ma, so a = F/m)"
  (when (and (physics-object-is-dynamic obj)
             (> (physics-object-mass obj) 0))
    (let ((mass (physics-object-mass obj)))
      (incf (physics-object-ax obj) (/ fx mass))
      (incf (physics-object-ay obj) (/ fy mass)))))

;;; Boundary constraints

(defun apply-boundary-constraints (obj)
  "Apply canvas boundary constraints"
  (when (physics-object-is-dynamic obj)
    (case (physics-object-type obj)
      (circle
       (let ((r (physics-object-radius obj))
             (restitution (physics-object-restitution obj)))
         ;; Left/Right walls
         (when (< (physics-object-x obj) r)
           (setf (physics-object-x obj) r)
           ;; Reverse velocity (bounce)
           (let ((vx (- (physics-object-x obj) (physics-object-old-x obj))))
             (setf (physics-object-old-x obj) (+ (physics-object-x obj) (* vx restitution)))))

         (when (> (physics-object-x obj) (- *canvas-width* r))
           (setf (physics-object-x obj) (- *canvas-width* r))
           (let ((vx (- (physics-object-x obj) (physics-object-old-x obj))))
             (setf (physics-object-old-x obj) (+ (physics-object-x obj) (* vx restitution)))))

         ;; Top/Bottom walls
         (when (< (physics-object-y obj) r)
           (setf (physics-object-y obj) r)
           (let ((vy (- (physics-object-y obj) (physics-object-old-y obj))))
             (setf (physics-object-old-y obj) (+ (physics-object-y obj) (* vy restitution)))))

         (when (> (physics-object-y obj) (- *canvas-height* r))
           (setf (physics-object-y obj) (- *canvas-height* r))
           (let ((vy (- (physics-object-y obj) (physics-object-old-y obj))))
             (setf (physics-object-old-y obj) (+ (physics-object-y obj) (* vy restitution))))))))))

;;; Collision detection

(defun check-circle-circle-collision (c1 c2)
  "Check and resolve collision between two circles"
  (let* ((dx (- (physics-object-x c2) (physics-object-x c1)))
         (dy (- (physics-object-y c2) (physics-object-y c1)))
         (dist-sq (+ (* dx dx) (* dy dy)))
         (r-sum (+ (physics-object-radius c1) (physics-object-radius c2)))
         (r-sum-sq (* r-sum r-sum)))

    (when (< dist-sq r-sum-sq)
      ;; Collision detected
      (let* ((dist (sqrt dist-sq))
             (overlap (- r-sum dist))
             (nx (/ dx dist))
             (ny (/ dy dist))

             ;; Move circles apart proportional to their masses
             (total-mass (+ (physics-object-mass c1) (physics-object-mass c2)))
             (ratio1 (if (> total-mass 0) (/ (physics-object-mass c2) total-mass) 0.5))
             (ratio2 (if (> total-mass 0) (/ (physics-object-mass c1) total-mass) 0.5)))

        ;; Separate circles
        (when (physics-object-is-dynamic c1)
          (decf (physics-object-x c1) (* overlap nx ratio1))
          (decf (physics-object-y c1) (* overlap ny ratio1)))

        (when (physics-object-is-dynamic c2)
          (incf (physics-object-x c2) (* overlap nx ratio2))
          (incf (physics-object-y c2) (* overlap ny ratio2)))

        ;; Apply elastic collision response (simplified)
        (let ((restitution (min (physics-object-restitution c1)
                               (physics-object-restitution c2))))
          (when (and (physics-object-is-dynamic c1) (physics-object-is-dynamic c2))
            ;; Exchange velocities along collision normal
            (let ((v1x (- (physics-object-x c1) (physics-object-old-x c1)))
                  (v1y (- (physics-object-y c1) (physics-object-old-y c1)))
                  (v2x (- (physics-object-x c2) (physics-object-old-x c2)))
                  (v2y (- (physics-object-y c2) (physics-object-old-y c2))))

              ;; Relative velocity along normal
              (let ((dvn (+ (* (- v1x v2x) nx) (* (- v1y v2y) ny))))
                (when (< dvn 0) ; Objects moving towards each other
                  ;; Impulse magnitude
                  (let ((impulse (* (- 1.0 restitution) dvn)))
                    ;; Apply impulse
                    (decf (physics-object-old-x c1) (* impulse nx ratio1))
                    (decf (physics-object-old-y c1) (* impulse ny ratio1))
                    (incf (physics-object-old-x c2) (* impulse nx ratio2))
                    (incf (physics-object-old-y c2) (* impulse ny ratio2))))))))

        t)))) ; Return T if collision occurred

(defun check-circle-rectangle-collision (circle rect)
  "Check and resolve collision between circle and axis-aligned rectangle"
  (let* ((cx (physics-object-x circle))
         (cy (physics-object-y circle))
         (r (physics-object-radius circle))
         (rx (physics-object-x rect))
         (ry (physics-object-y rect))
         (rw (physics-object-width rect))
         (rh (physics-object-height rect))

         ;; Find closest point on rectangle to circle center
         (closest-x (max rx (min cx (+ rx rw))))
         (closest-y (max ry (min cy (+ ry rh))))

         ;; Distance from circle center to closest point
         (dx (- cx closest-x))
         (dy (- cy closest-y))
         (dist-sq (+ (* dx dx) (* dy dy))))

    (when (< dist-sq (* r r))
      ;; Collision detected
      (let* ((dist (sqrt dist-sq))
             (overlap (- r dist))
             (nx (if (> dist 0) (/ dx dist) 0))
             (ny (if (> dist 0) (/ dy dist) -1)) ; Default to pushing up
             (restitution (physics-object-restitution circle)))

        ;; Push circle out of rectangle
        (when (physics-object-is-dynamic circle)
          (incf (physics-object-x circle) (* overlap nx))
          (incf (physics-object-y circle) (* overlap ny))

          ;; Bounce (reflect velocity)
          (let ((vx (- (physics-object-x circle) (physics-object-old-x circle)))
                (vy (- (physics-object-y circle) (physics-object-old-y circle))))
            (let ((dot (* 2 (+ (* vx nx) (* vy ny)))))
              (setf (physics-object-old-x circle)
                    (- (physics-object-x circle) (* (- vx (* dot nx)) restitution)))
              (setf (physics-object-old-y circle)
                    (- (physics-object-y circle) (* (- vy (* dot ny)) restitution))))))

        t)))) ; Return T if collision occurred

(defun resolve-all-collisions ()
  "Resolve all collisions between physics objects"
  (let ((objects (hash-table-values *physics-objects*))
        (collision-count 0))

    ;; Circle-circle collisions
    (loop for i from 0 below (length objects) do
      (loop for j from (1+ i) below (length objects) do
        (let ((obj1 (nth i objects))
              (obj2 (nth j objects)))
          (when (and (eq (physics-object-type obj1) 'circle)
                     (eq (physics-object-type obj2) 'circle)
                     (or (physics-object-is-dynamic obj1)
                         (physics-object-is-dynamic obj2)))
            (when (check-circle-circle-collision obj1 obj2)
              (incf collision-count))))))

    ;; Circle-rectangle collisions
    (loop for obj in objects do
      (when (and (eq (physics-object-type obj) 'circle)
                 (physics-object-is-dynamic obj))
        (loop for other in objects do
          (when (eq (physics-object-type other) 'rectangle)
            (when (check-circle-rectangle-collision obj other)
              (incf collision-count))))))

    collision-count))

;;; Main simulation step

(defun physics-step ()
  "Execute one physics simulation step"
  (unless *physics-paused*
    (let ((dt *physics-timestep*))
      ;; 1. Apply forces
      (maphash (lambda (id obj)
                 (declare (ignore id))
                 (apply-gravity obj))
               *physics-objects*)

      ;; 2. Integrate (update positions)
      (maphash (lambda (id obj)
                 (declare (ignore id))
                 (integrate-verlet obj dt))
               *physics-objects*)

      ;; 3. Apply constraints
      (maphash (lambda (id obj)
                 (declare (ignore id))
                 (apply-boundary-constraints obj))
               *physics-objects*)

      ;; 4. Resolve collisions (multiple iterations for stability)
      (dotimes (i 3)
        (resolve-all-collisions)))))

;;; Utility functions

(defun hash-table-values (ht)
  "Get all values from a hash table as a list"
  (loop for v being the hash-values of ht collect v))

(defun get-physics-state-snapshot ()
  "Get a snapshot of all dynamic object positions for broadcasting"
  (let ((snapshot '()))
    (maphash (lambda (id obj)
               (when (physics-object-is-dynamic obj)
                 (push (list :id id
                            :x (physics-object-x obj)
                            :y (physics-object-y obj)
                            :rotation 0.0) ; For future use
                       snapshot)))
             *physics-objects*)
    snapshot))

(defun set-global-gravity (gravity)
  "Set the global gravity value"
  (setf *global-gravity* gravity))

(defun pause-physics ()
  "Pause the physics simulation"
  (setf *physics-paused* t))

(defun resume-physics ()
  "Resume the physics simulation"
  (setf *physics-paused* nil))

(defun reset-physics ()
  "Reset all physics objects to their initial state"
  (pause-physics)
  (clear-all-physics-objects))

(defun is-physics-paused ()
  "Check if physics is paused"
  *physics-paused*)

;;; Sync with canvas state

(defun sync-canvas-object-to-physics (obj-data)
  "Sync a canvas object to the physics engine"
  (let ((id (cdr (assoc :id obj-data)))
        (type-str (cdr (assoc :type obj-data)))
        (x (cdr (assoc :x obj-data)))
        (y (cdr (assoc :y obj-data)))
        (color (cdr (assoc :color obj-data)))
        (is-dynamic (cdr (assoc :is-dynamic obj-data))))

    (cond
      ((string= type-str "circle")
       (let ((radius (cdr (assoc :radius obj-data))))
         (unless (get-physics-object id)
           (create-physics-circle id x y radius
                                 :is-dynamic (if is-dynamic t nil)
                                 :color (or color "#3498db")))))

      ((string= type-str "rectangle")
       (let ((width (cdr (assoc :width obj-data)))
             (height (cdr (assoc :height obj-data))))
         (unless (get-physics-object id)
           (create-physics-rectangle id x y width height
                                    :color (or color "#95a5a6"))))))))

(defun update-physics-object-position (id x y)
  "Manually update a physics object's position (e.g., user drag)"
  (let ((obj (get-physics-object id)))
    (when obj
      (setf (physics-object-x obj) x)
      (setf (physics-object-y obj) y)
      (setf (physics-object-old-x obj) x)
      (setf (physics-object-old-y obj) y)
      (setf (physics-object-vx obj) 0.0)
      (setf (physics-object-vy obj) 0.0))))
