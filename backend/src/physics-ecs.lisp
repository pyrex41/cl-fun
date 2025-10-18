;;;; physics-ecs.lisp - Physics ECS Storage Management
;;;;
;;;; Manages per-canvas ECS storage instances for physics simulations.
;;;; Each canvas has its own isolated physics world with configurable
;;;; gravity and simulation rate.

(in-package #:collabcanvas)

;;; ============================================================================
;;; Global Storage Management
;;; ============================================================================

(defparameter *physics-canvases* (make-hash-table :test 'equal)
  "Hash table mapping canvas-id (string) to ECS storage instances.
   Each canvas has its own isolated physics world.")

(defparameter *physics-canvases-lock* (bt:make-lock "physics-canvases-lock")
  "Thread-safe lock for accessing *physics-canvases* hash table.")

;;; ============================================================================
;;; ECS Storage Configuration Structure
;;; ============================================================================

(defstruct physics-canvas-config
  "Configuration and storage for a canvas physics world."
  (storage nil :type (or null storage))
  (quadtree nil :type t)  ; Quadtree for spatial partitioning (collision optimization)
  (canvas-width 800.0 :type single-float)
  (canvas-height 600.0 :type single-float)
  (gravity-x 0.0 :type single-float)
  (gravity-y 9.8 :type single-float)
  (simulation-rate 60 :type fixnum)
  (created-at (get-universal-time) :type integer))

;;; ============================================================================
;;; Core Functions
;;; ============================================================================

(defun init-canvas-physics (canvas-id &key (gravity-x nil) (gravity-y nil)
                                           (simulation-rate nil))
  "Initialize physics ECS storage for a canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)
     gravity-x - Horizontal gravity (defaults to database or 0.0)
     gravity-y - Vertical gravity (defaults to database or 9.8)
     simulation-rate - Simulation frequency in Hz (defaults to database or 60)

   Returns:
     The newly created ECS storage instance.

   Side effects:
     - Creates new ECS storage with (ecs:make-storage)
     - Loads settings from database if not provided
     - Stores configuration in *physics-canvases* hash table
     - Thread-safe operation

   Example:
     (init-canvas-physics \"canvas-123\" :gravity-y 19.6 :simulation-rate 120)"

  (bt:with-lock-held (*physics-canvases-lock*)
    ;; Load settings from database if not provided
    (let* ((db-settings (load-physics-settings canvas-id))
           (final-gravity-x (or gravity-x
                               (cdr (assoc :gravity-x db-settings))
                               0.0))
           (final-gravity-y (or gravity-y
                               (cdr (assoc :gravity-y db-settings))
                               9.8))
           (final-sim-rate (or simulation-rate
                              (cdr (assoc :simulation-rate db-settings))
                              60))
           ;; Get canvas dimensions (default to 800x600 if not in DB)
           (canvas-width (or (cdr (assoc :canvas-width db-settings)) 800.0))
           (canvas-height (or (cdr (assoc :canvas-height db-settings)) 600.0))
           ;; Create new ECS storage
           (storage (cl-fast-ecs:make-storage))
           ;; Create quadtree for spatial partitioning
           (quadtree (make-physics-quadtree (float canvas-width 0.0)
                                            (float canvas-height 0.0)))
           ;; Create configuration structure
           (config (make-physics-canvas-config
                    :storage storage
                    :quadtree quadtree
                    :canvas-width (coerce canvas-width 'single-float)
                    :canvas-height (coerce canvas-height 'single-float)
                    :gravity-x (coerce final-gravity-x 'single-float)
                    :gravity-y (coerce final-gravity-y 'single-float)
                    :simulation-rate final-sim-rate)))

      ;; Store in hash table
      (setf (gethash canvas-id *physics-canvases*) config)

      ;; Log initialization
      (format t "Initialized physics for canvas ~A: gravity=(~,2F, ~,2F) rate=~D Hz~%"
              canvas-id final-gravity-x final-gravity-y final-sim-rate)

      ;; Return the storage
      storage)))

(defun get-canvas-ecs-storage (canvas-id)
  "Retrieve ECS storage for a canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Returns:
     The ECS storage instance if found, NIL otherwise.

   Thread-safe: Yes

   Example:
     (let ((storage (get-canvas-ecs-storage \"canvas-123\")))
       (when storage
         (with-storage (storage)
           ...)))"

  (bt:with-lock-held (*physics-canvases-lock*)
    (when-let ((config (gethash canvas-id *physics-canvases*)))
      (physics-canvas-config-storage config))))

(defun get-canvas-physics-config (canvas-id)
  "Retrieve full physics configuration for a canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Returns:
     physics-canvas-config structure if found, NIL otherwise.

   Thread-safe: Yes

   Example:
     (let ((config (get-canvas-physics-config \"canvas-123\")))
       (when config
         (format t \"Gravity: ~,2F~%\" (physics-canvas-config-gravity-y config))))"

  (bt:with-lock-held (*physics-canvases-lock*)
    (gethash canvas-id *physics-canvases*)))

(defun destroy-canvas-physics (canvas-id)
  "Clean up and remove physics ECS storage for a canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Returns:
     T if canvas was found and destroyed, NIL if not found.

   Side effects:
     - Removes canvas from *physics-canvases* hash table
     - ECS storage is cleaned up by garbage collector
     - Thread-safe operation

   Example:
     (destroy-canvas-physics \"canvas-123\")"

  (bt:with-lock-held (*physics-canvases-lock*)
    (let ((config (gethash canvas-id *physics-canvases*)))
      (when config
        ;; Remove from hash table (storage will be GC'd)
        (remhash canvas-id *physics-canvases*)
        (format t "Destroyed physics for canvas ~A~%" canvas-id)
        t))))

(defun list-active-physics-canvases ()
  "List all canvas IDs with active physics simulations.

   Returns:
     List of canvas-id strings.

   Thread-safe: Yes"

  (bt:with-lock-held (*physics-canvases-lock*)
    (alexandria:hash-table-keys *physics-canvases*)))

(defun canvas-physics-active-p (canvas-id)
  "Check if physics is initialized for a canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Returns:
     T if physics is active, NIL otherwise.

   Thread-safe: Yes"

  (bt:with-lock-held (*physics-canvases-lock*)
    (not (null (gethash canvas-id *physics-canvases*)))))

(defun update-canvas-physics-settings (canvas-id &key gravity-x gravity-y simulation-rate)
  "Update physics settings for an active canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)
     gravity-x - New horizontal gravity (optional)
     gravity-y - New vertical gravity (optional)
     simulation-rate - New simulation frequency in Hz (optional)

   Returns:
     T if updated successfully, NIL if canvas not found.

   Note: Does not recreate ECS storage, only updates configuration.

   Example:
     (update-canvas-physics-settings \"canvas-123\" :gravity-y 19.6)"

  (bt:with-lock-held (*physics-canvases-lock*)
    (when-let ((config (gethash canvas-id *physics-canvases*)))
      (when gravity-x
        (setf (physics-canvas-config-gravity-x config)
              (coerce gravity-x 'single-float)))
      (when gravity-y
        (setf (physics-canvas-config-gravity-y config)
              (coerce gravity-y 'single-float)))
      (when simulation-rate
        (setf (physics-canvas-config-simulation-rate config)
              simulation-rate))
      (format t "Updated physics settings for canvas ~A~%" canvas-id)
      t)))

(defun cleanup-all-physics-canvases ()
  "Destroy all physics canvases. Used during server shutdown.

   Side effects:
     - Clears *physics-canvases* hash table
     - All ECS storage instances are cleaned up

   Thread-safe: Yes"

  (bt:with-lock-held (*physics-canvases-lock*)
    (let ((count (hash-table-count *physics-canvases*)))
      (clrhash *physics-canvases*)
      (format t "Cleaned up ~D physics canvas(es)~%" count)
      count)))

;;; ============================================================================
;;; Legacy Compatibility (deprecated, will be removed)
;;; ============================================================================

(defparameter *physics-world* nil
  "Deprecated: Use per-canvas ECS storage instead.
   This is kept for backwards compatibility only.")

(defun init-physics-world ()
  "Deprecated: Use init-canvas-physics instead.
   This function is a no-op for backwards compatibility."
  (format t "Warning: init-physics-world is deprecated. Use init-canvas-physics instead.~%")
  nil)

;;; ============================================================================
;;; Physics Entity Spawning
;;; ============================================================================

(defun spawn-physics-ball (canvas-id x y vx vy
                           &key (radius 10.0) (mass 1.0) (restitution 0.8))
  "Spawn a physics ball entity in the specified canvas.

   Arguments:
     canvas-id - Canvas identifier (string)
     x, y - Initial position (single-float)
     vx, vy - Initial velocity in pixels/second (single-float)
     radius - Ball radius in pixels (default: 10.0)
     mass - Ball mass for physics (default: 1.0)
     restitution - Bounciness coefficient 0.0-1.0 (default: 0.8)

   Returns:
     The created entity ID, or NIL if canvas physics not initialized.

   Example:
     (spawn-physics-ball \"canvas-123\" 400.0 300.0 50.0 -20.0
                         :radius 15.0 :mass 2.0)"

  (let ((storage (get-canvas-ecs-storage canvas-id)))
    (unless storage
      (format t "[PHYS WARN] Cannot spawn ball: canvas ~A physics not initialized~%"
              canvas-id)
      (return-from spawn-physics-ball nil))

    ;; Create entity within storage context
    (setf cl-fast-ecs:*storage* storage)
    (let ((entity (cl-fast-ecs:make-entity)))

      ;; Add position component
      (cl-fast-ecs:make-component entity 'position
                                  :x (coerce x 'single-float)
                                  :y (coerce y 'single-float))

      ;; Add velocity component
      (cl-fast-ecs:make-component entity 'velocity
                                  :vx (coerce vx 'single-float)
                                  :vy (coerce vy 'single-float))

      ;; Add acceleration component (starts at zero, gravity added each frame)
      (cl-fast-ecs:make-component entity 'acceleration
                                  :ax 0.0
                                  :ay 0.0)

      ;; Add ball component with physics properties
      (cl-fast-ecs:make-component entity 'ball
                                  :radius (coerce radius 'single-float)
                                  :mass (coerce mass 'single-float)
                                  :restitution (coerce restitution 'single-float)
                                  :sleeping nil)

      (format t "[PHYS] Spawned ball entity ~A at (~,1F, ~,1F) vel=(~,1F, ~,1F) on canvas ~A~%"
              entity x y vx vy canvas-id)

      entity)))

(defun spawn-physics-block (canvas-id x y width height)
  "Spawn a static block entity in the specified canvas.

   Arguments:
     canvas-id - Canvas identifier (string)
     x, y - Block center position (single-float)
     width, height - Block dimensions (single-float)

   Returns:
     The created entity ID, or NIL if canvas physics not initialized.

   Example:
     (spawn-physics-block \"canvas-123\" 400.0 550.0 200.0 20.0)"

  (let ((storage (get-canvas-ecs-storage canvas-id)))
    (unless storage
      (format t "[PHYS WARN] Cannot spawn block: canvas ~A physics not initialized~%"
              canvas-id)
      (return-from spawn-physics-block nil))

    ;; Create entity within storage context
    (setf cl-fast-ecs:*storage* storage)
    (let ((entity (cl-fast-ecs:make-entity)))

      ;; Add position component
      (cl-fast-ecs:make-component entity 'position
                                  :x (coerce x 'single-float)
                                  :y (coerce y 'single-float))

      ;; Add block component (static obstacle, no velocity)
      (cl-fast-ecs:make-component entity 'block
                                  :width (coerce width 'single-float)
                                  :height (coerce height 'single-float))

      (format t "[PHYS] Spawned block entity ~A at (~,1F, ~,1F) size=(~,1Fx~,1F) on canvas ~A~%"
              entity x y width height canvas-id)

      entity)))

;;; ============================================================================
;;; Module Initialization
;;; ============================================================================

(format t "Physics ECS Storage Management loaded~%")
(format t "  - Per-canvas ECS storage with *physics-canvases* hash table~%")
(format t "  - Default gravity: (0.0, 9.8) pixels/s²~%")
(format t "  - Default simulation rate: 60 Hz~%")
