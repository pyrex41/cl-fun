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

;;; Particle lifecycle constants
(defparameter *max-particles* 500 "Maximum number of particles allowed simultaneously")
(defparameter *default-particle-lifespan* 5.0 "Default particle lifespan in seconds")

;;; Emitter tracking
(defparameter *emitters* (make-hash-table :test 'equal)
  "Hash table of active emitters by ID")

;;; Emitter structure
(defstruct emitter
  id                    ; Emitter ID (string)
  x                     ; X position
  y                     ; Y position
  rate                  ; Particles per second
  lifespan              ; Particle lifespan in milliseconds
  particle-size         ; Particle radius
  initial-velocity      ; Alist with :x and :y velocity components
  color                 ; Particle color
  accumulator           ; Time accumulator for emission (seconds)
  particle-count)       ; Total particles emitted (for unique IDs)

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
  color           ; Color (hex string)
  particle-p      ; T if this is a particle (for lifecycle management)
  birth-time      ; Universal time when particle was created
  lifespan)       ; Lifespan in seconds (NIL = infinite)

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
               :color color
               :particle-p nil
               :birth-time nil
               :lifespan nil)))
    (setf (gethash id *physics-objects*) obj)
    obj))

(defun create-particle (id x y radius &key
                        (color "#ffaa00")
                        (lifespan *default-particle-lifespan*)
                        (initial-vx 0.0)
                        (initial-vy 0.0))
  "Create a particle with lifecycle management"
  (let* ((mass (calculate-mass-from-radius radius))
         (current-time (get-universal-time))
         (obj (make-physics-object
               :id id
               :type 'circle
               :x x
               :y y
               :old-x (- x initial-vx)
               :old-y (- y initial-vy)
               :vx initial-vx
               :vy initial-vy
               :ax 0.0
               :ay 0.0
               :radius radius
               :mass mass
               :is-dynamic t
               :restitution *default-restitution*
               :friction *default-friction*
               :color color
               :particle-p t
               :birth-time current-time
               :lifespan lifespan)))
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
              :color color
              :particle-p nil
              :birth-time nil
              :lifespan nil)))
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

;;; Emitter management

(defun create-emitter (id x y rate lifespan particle-size initial-velocity color)
  "Create a particle emitter at the specified position"
  (let ((em (make-emitter
             :id id
             :x x
             :y y
             :rate rate
             :lifespan (/ lifespan 1000.0) ; Convert milliseconds to seconds
             :particle-size particle-size
             :initial-velocity initial-velocity
             :color color
             :accumulator 0.0
             :particle-count 0)))
    (setf (gethash id *emitters*) em)
    (format t "~&[Physics] Created emitter ~A at (~A, ~A) with rate ~A p/s~%" id x y rate)
    em))

(defun remove-emitter (id)
  "Remove an emitter from the simulation"
  (remhash id *emitters*)
  (format t "~&[Physics] Removed emitter ~A~%" id))

(defun get-emitter (id)
  "Get an emitter by ID"
  (gethash id *emitters*))

(defun update-emitter-position (id x y)
  "Update an emitter's position (when dragged by user)"
  (let ((em (get-emitter id)))
    (when em
      (setf (emitter-x em) x)
      (setf (emitter-y em) y))))

(defun clear-all-emitters ()
  "Remove all emitters"
  (clrhash *emitters*))

(defun process-emitters (dt)
  "Process all emitters and emit particles based on their rate"
  (let ((total-emitted 0))
    (maphash (lambda (id em)
               (declare (ignore id))
               ;; Accumulate time
               (incf (emitter-accumulator em) dt)

               ;; Calculate emission interval (time between particles)
               (let ((emission-interval (if (> (emitter-rate em) 0)
                                           (/ 1.0 (emitter-rate em))
                                           most-positive-double-float)))

                 ;; Emit particles based on accumulated time
                 (loop while (>= (emitter-accumulator em) emission-interval) do
                   ;; Check particle limit before emitting
                   (when (< (count-particles) *max-particles*)
                     ;; Generate unique particle ID
                     (incf (emitter-particle-count em))
                     (let* ((particle-id (format nil "~A-p~D"
                                               (emitter-id em)
                                               (emitter-particle-count em)))
                            (vx (cdr (assoc :x (emitter-initial-velocity em))))
                            (vy (cdr (assoc :y (emitter-initial-velocity em)))))

                       ;; Create particle with initial velocity
                       (create-particle particle-id
                                      (emitter-x em)
                                      (emitter-y em)
                                      (emitter-particle-size em)
                                      :color (emitter-color em)
                                      :lifespan (emitter-lifespan em)
                                      :initial-vx (or vx 0.0)
                                      :initial-vy (or vy 0.0))

                       (incf total-emitted)))

                   ;; Decrement accumulator
                   (decf (emitter-accumulator em) emission-interval))))
             *emitters*)

    total-emitted))

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
      ;; 0. Process emitters (emit new particles)
      (process-emitters dt)

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
        (resolve-all-collisions))

      ;; 5. Particle lifecycle management
      (cleanup-expired-particles)
      (enforce-particle-limit))))

;;; Utility functions

(defun hash-table-values (ht)
  "Get all values from a hash table as a list"
  (loop for v being the hash-values of ht collect v))

;;; Particle lifecycle management

(defun get-particle-age (particle)
  "Calculate the age of a particle in seconds"
  (if (and (physics-object-particle-p particle)
           (physics-object-birth-time particle))
      (- (get-universal-time) (physics-object-birth-time particle))
      0))

(defun is-particle-expired (particle)
  "Check if a particle has exceeded its lifespan"
  (and (physics-object-particle-p particle)
       (physics-object-lifespan particle)
       (physics-object-birth-time particle)
       (> (get-particle-age particle) (physics-object-lifespan particle))))

(defun cleanup-expired-particles ()
  "Remove all particles that have exceeded their lifespan"
  (let ((expired-ids '())
        (removed-count 0))
    ;; Collect expired particle IDs
    (maphash (lambda (id obj)
               (when (is-particle-expired obj)
                 (push id expired-ids)))
             *physics-objects*)

    ;; Remove expired particles
    (dolist (id expired-ids)
      (remove-physics-object id)
      (incf removed-count))

    (when (> removed-count 0)
      (format t "~&[Physics] Cleaned up ~D expired particle~:P~%" removed-count))

    removed-count))

(defun count-particles ()
  "Count the total number of particles in the simulation"
  (let ((count 0))
    (maphash (lambda (id obj)
               (declare (ignore id))
               (when (physics-object-particle-p obj)
                 (incf count)))
             *physics-objects*)
    count))

(defun get-oldest-particles (n)
  "Get the N oldest particles sorted by birth-time"
  (let ((particles '()))
    ;; Collect all particles
    (maphash (lambda (id obj)
               (when (physics-object-particle-p obj)
                 (push (cons id obj) particles)))
             *physics-objects*)

    ;; Sort by birth-time (oldest first)
    (setf particles (sort particles #'<
                         :key (lambda (pair)
                                (or (physics-object-birth-time (cdr pair))
                                    most-positive-fixnum))))

    ;; Return first N
    (subseq particles 0 (min n (length particles)))))

(defun enforce-particle-limit ()
  "Enforce the maximum particle limit by removing oldest particles"
  (let ((particle-count (count-particles))
        (removed-count 0))
    (when (> particle-count *max-particles*)
      (let* ((excess (- particle-count *max-particles*))
             (oldest-particles (get-oldest-particles excess)))

        (format t "~&[Physics] WARNING: Particle limit reached (~D/~D). Removing ~D oldest particle~:P~%"
                particle-count *max-particles* excess)

        ;; Remove oldest particles
        (dolist (pair oldest-particles)
          (remove-physics-object (car pair))
          (incf removed-count))))

    removed-count))

(defun get-particle-statistics ()
  "Get statistics about particles in the simulation"
  (let ((total-particles 0)
        (total-objects 0)
        (oldest-age 0))
    (maphash (lambda (id obj)
               (declare (ignore id))
               (incf total-objects)
               (when (physics-object-particle-p obj)
                 (incf total-particles)
                 (let ((age (get-particle-age obj)))
                   (when (> age oldest-age)
                     (setf oldest-age age)))))
             *physics-objects*)

    (list :total-objects total-objects
          :total-particles total-particles
          :regular-objects (- total-objects total-particles)
          :oldest-particle-age oldest-age
          :particle-capacity *max-particles*
          :particle-usage-percent (if (> *max-particles* 0)
                                      (* 100.0 (/ total-particles *max-particles*))
                                      0))))

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

(defun set-max-particles (max)
  "Set the maximum number of particles allowed"
  (setf *max-particles* max)
  (format t "~&[Physics] Max particles set to ~D~%" max))

(defun get-max-particles ()
  "Get the current maximum particle limit"
  *max-particles*)

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
                                    :color (or color "#95a5a6")))))

      ((string= type-str "emitter")
       (let ((rate (cdr (assoc :rate obj-data)))
             (lifespan (cdr (assoc :lifespan obj-data)))
             (particle-size (cdr (assoc :particle-size obj-data)))
             (initial-velocity (cdr (assoc :initial-velocity obj-data))))
         (unless (get-emitter id)
           (create-emitter id x y rate lifespan particle-size initial-velocity
                          (or color "#ffaa00"))))))))

(defun update-physics-object-position (id x y)
  "Manually update a physics object's position (e.g., user drag)"
  (let ((obj (get-physics-object id)))
    (when obj
      (setf (physics-object-x obj) x)
      (setf (physics-object-y obj) y)
      (setf (physics-object-old-x obj) x)
      (setf (physics-object-old-y obj) y)
      (setf (physics-object-vx obj) 0.0)
      (setf (physics-object-vy obj) 0.0)))
  ;; Also check if it's an emitter
  (let ((em (get-emitter id)))
    (when em
      (update-emitter-position id x y))))

(defun reset-all-velocities ()
  "Reset velocities of all dynamic physics objects to zero"
  (maphash (lambda (id obj)
             (declare (ignore id))
             (when (physics-object-is-dynamic obj)
               (setf (physics-object-vx obj) 0.0)
               (setf (physics-object-vy obj) 0.0)
               (setf (physics-object-ax obj) 0.0)
               (setf (physics-object-ay obj) 0.0)
               ;; Reset old position to current position to zero velocity in Verlet
               (setf (physics-object-old-x obj) (physics-object-x obj))
               (setf (physics-object-old-y obj) (physics-object-y obj))))
           *physics-objects*))
