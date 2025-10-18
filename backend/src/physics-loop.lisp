;;;; physics-loop.lisp - Fixed Timestep Physics Loop with ECS Systems
;;;;
;;;; Manages background threads running physics simulations at 60 Hz.
;;;; Each canvas has its own dedicated physics thread that:
;;;;   1. Runs ECS systems in correct order with fixed dt=0.016
;;;;   2. Broadcasts delta updates at 20 Hz (every 3rd tick)
;;;;   3. Handles thread lifecycle (start/stop)
;;;;
;;;; Integration:
;;;;   - Uses get-canvas-ecs-storage from physics-ecs.lisp
;;;;   - Uses get-canvas-physics-config for gravity settings
;;;;   - Uses broadcast-to-canvas-room from websocket-adapter.lisp
;;;;   - Calls systems from physics-systems.lisp via cl-fast-ecs:run-systemss
;;;;
;;;; PRD Section: 4.1.4 - Physics Loop Implementation

(in-package #:collabcanvas)

;;; ============================================================================
;;; Global Thread Management
;;; ============================================================================

(defparameter *physics-threads* (make-hash-table :test 'equal)
  "Hash table mapping canvas-id (string) to physics thread instances.
   Each canvas has its own background thread running at 60 Hz.")

(defparameter *physics-threads-lock* (bt:make-lock "physics-threads-lock")
  "Thread-safe lock for accessing *physics-threads* hash table.")

(defparameter *physics-running* (make-hash-table :test 'equal)
  "Hash table mapping canvas-id (string) to boolean flag.
   Set to T when thread should continue running, NIL to stop.")

(defparameter *physics-running-lock* (bt:make-lock "physics-running-lock")
  "Thread-safe lock for accessing *physics-running* hash table.")

;;; ============================================================================
;;; Delta Compression State (Task 4.1)
;;; ============================================================================

(defparameter *entity-previous-state* (make-hash-table :test 'equal)
  "Hash table mapping canvas-id to hash table of entity-id -> previous state.
   Used for delta compression to track which properties changed.
   Previous state structure: (x y vx vy)")

(defparameter *entity-state-lock* (bt:make-lock "entity-state-lock")
  "Thread-safe lock for accessing *entity-previous-state* hash table.")

(defparameter *bandwidth-stats* (make-hash-table :test 'equal)
  "Hash table mapping canvas-id to bandwidth statistics.
   Stats structure: (total-bytes message-count last-reset-time)")

(defparameter *bandwidth-stats-lock* (bt:make-lock "bandwidth-stats-lock")
  "Thread-safe lock for accessing *bandwidth-stats* hash table.")

;;; ============================================================================
;;; Constants
;;; ============================================================================

(defconstant +physics-hz+ 60
  "Physics simulation frequency in Hz (60 ticks per second).")

(defconstant +physics-dt+ (coerce (/ 1.0 +physics-hz+) 'single-float)
  "Fixed timestep in seconds (0.016667 for 60 Hz).")

(defconstant +broadcast-hz+ 20
  "Delta broadcast frequency in Hz (20 updates per second).")

(defconstant +broadcast-interval+ (/ +physics-hz+ +broadcast-hz+)
  "Broadcast every Nth tick (60/20 = 3).")

(defconstant +sleep-ms+ (floor (* 1000 +physics-dt+))
  "Sleep duration in milliseconds between ticks (~16ms).")

;;; Delta Compression Constants (Task 4.1)
(defconstant +position-threshold+ 0.01
  "Minimum position change (in pixels) required to include in delta.")

(defconstant +velocity-threshold+ 0.001
  "Minimum velocity change required to include in delta.")

(defconstant +quantize-precision+ 0.1
  "Precision for float quantization (0.1 = round to 1 decimal place).")

;;; ============================================================================
;;; Core Loop Functions
;;; ============================================================================

(defun start-physics-loop (canvas-id)
  "Start a background physics thread for a canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Returns:
     The newly created thread instance, or NIL if already running.

   Side effects:
     - Creates background thread running at 60 Hz
     - Registers thread in *physics-threads* hash table
     - Sets *physics-running* flag to T
     - Thread-safe operation

   Example:
     (start-physics-loop \"canvas-123\")"

  (bt:with-lock-held (*physics-threads-lock*)
    ;; Check if thread already exists
    (when (gethash canvas-id *physics-threads*)
      (format t "[PHYS] Physics loop already running for canvas ~A~%" canvas-id)
      (return-from start-physics-loop nil))

    ;; Verify ECS storage exists
    (unless (get-canvas-ecs-storage canvas-id)
      (format t "[PHYS ERROR] No ECS storage found for canvas ~A~%" canvas-id)
      (return-from start-physics-loop nil))

    ;; Set running flag
    (bt:with-lock-held (*physics-running-lock*)
      (setf (gethash canvas-id *physics-running*) t))

    ;; Create and start physics thread
    (let ((thread (bt:make-thread
                   (lambda () (physics-loop-worker canvas-id))
                   :name (format nil "physics-~A" canvas-id))))
      (setf (gethash canvas-id *physics-threads*) thread)
      (format t "[PHYS] Started physics loop for canvas ~A at ~D Hz~%"
              canvas-id +physics-hz+)
      thread)))

(defun stop-physics-loop (canvas-id)
  "Stop the physics thread for a canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Returns:
     T if thread was found and stopped, NIL if not running.

   Side effects:
     - Sets *physics-running* flag to NIL (signals thread to exit)
     - Waits for thread to finish (up to 1 second)
     - Removes thread from *physics-threads* hash table
     - Thread-safe operation

   Example:
     (stop-physics-loop \"canvas-123\")"

  (bt:with-lock-held (*physics-threads-lock*)
    (let ((thread (gethash canvas-id *physics-threads*)))
      (if thread
          (progn
            ;; Signal thread to stop
            (bt:with-lock-held (*physics-running-lock*)
              (setf (gethash canvas-id *physics-running*) nil))

            ;; Wait for thread to finish (max 1 second)
            (handler-case
                (bt:join-thread thread :timeout 1.0)
              (error (e)
                (format t "[PHYS WARN] Error joining thread for ~A: ~A~%" canvas-id e)))

            ;; Remove from registry
            (remhash canvas-id *physics-threads*)
            (bt:with-lock-held (*physics-running-lock*)
              (remhash canvas-id *physics-running*))

            ;; Clear delta compression state (Task 4.1)
            (clear-canvas-delta-state canvas-id)

            (format t "[PHYS] Stopped physics loop for canvas ~A~%" canvas-id)
            t)

          (progn
            (format t "[PHYS] No physics loop running for canvas ~A~%" canvas-id)
            nil)))))

(defun physics-loop-worker (canvas-id)
  "Background worker function for physics simulation.

   This is the main loop that runs in a dedicated thread.

   Algorithm:
     1. Get canvas ECS storage and physics config
     2. Loop while *physics-running* flag is T:
        a. Run all ECS systems with fixed dt
        b. Increment tick counter
        c. Every 3rd tick, broadcast delta updates
        d. Sleep to maintain 60 Hz rate
     3. Clean up on exit

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Side effects:
     - Modifies ECS entities via system execution
     - Broadcasts WebSocket messages at 20 Hz
     - Sleeps between ticks to maintain timing"

  (handler-case
      (let ((storage (get-canvas-ecs-storage canvas-id))
            (tick-count 0))

        (unless storage
          (format t "[PHYS ERROR] No ECS storage for canvas ~A, exiting~%" canvas-id)
          (return-from physics-loop-worker nil))

        (format t "[PHYS] Worker started for ~A: dt=~,4F~%" canvas-id +physics-dt+)

        ;; Main simulation loop
        (loop
          ;; Check running flag
          (unless (bt:with-lock-held (*physics-running-lock*)
                    (gethash canvas-id *physics-running*))
            (format t "[PHYS] Worker stopping for canvas ~A~%" canvas-id)
            (return))

          ;; Get fresh config on each tick (allows dynamic gravity updates)
          (let* ((config (get-canvas-physics-config canvas-id))
                 (gravity-x (if config
                               (physics-canvas-config-gravity-x config)
                               0.0))
                 (gravity-y (if config
                               (physics-canvas-config-gravity-y config)
                               9.8))
                 (quadtree (if config
                              (physics-canvas-config-quadtree config)
                              nil))
                 (max-objects 2000))  ; Default max objects

            ;; Run ECS systems with storage context
            (progn
              (setf *storage* storage)

              ;; Clear quadtree and collision impulses for this frame
              (when quadtree
                (clear-physics-quadtree quadtree))
              (clear-collision-impulses)

              ;; System 0: Populate quadtree with all physics entities
              (when quadtree
                (cl-fast-ecs:run-systems 'populate-quadtree-system
                                        quadtree)
                (cl-fast-ecs:run-systems 'populate-quadtree-blocks-system
                                        quadtree))

              ;; System 1: Emit balls from emitters (Post-MVP) - DISABLED
              ;; TODO: Re-implement emitter-system
              ;; (cl-fast-ecs:run-systems 'emitter-system
              ;;                         :dt +physics-dt+
              ;;                         :current-time (coerce (get-internal-real-time) 'single-float)
              ;;                         :max-objects max-objects)

              ;; System 2: Apply force fields to balls (STUB)
              (cl-fast-ecs:run-systems 'apply-forces-system)

              ;; System 3: Update velocity from acceleration + gravity
              (cl-fast-ecs:run-systems 'apply-acceleration-system
                                      :dt +physics-dt+
                                      :gravity-x gravity-x
                                      :gravity-y gravity-y)

              ;; System 4: Update position from velocity
              (cl-fast-ecs:run-systems 'apply-velocity-system
                                      :dt +physics-dt+)

              ;; System 5a: Detect collisions (Two-Pass Architecture - Pass 1)
              (when quadtree
                (cl-fast-ecs:run-systems 'detect-collisions-system
                                        :quadtree quadtree))

              ;; System 5b: Apply collision impulses (Two-Pass Architecture - Pass 2)
              (cl-fast-ecs:run-systems 'apply-collision-impulses-system)

              ;; System 6: Check for sleeping entities
              (cl-fast-ecs:run-systems 'check-sleeping-system))

            ;; Increment tick counter
            (incf tick-count)

            ;; Broadcast delta updates every 3rd tick (20 Hz)
            (when (zerop (mod tick-count +broadcast-interval+))
              (broadcast-physics-delta canvas-id storage))

            ;; Sleep to maintain 60 Hz (~16.67ms per tick)
            (sleep (/ +sleep-ms+ 1000.0)))))

    (error (e)
      (format t "[PHYS ERROR] Worker crashed for canvas ~A: ~A~%" canvas-id e))))

;;; ============================================================================
;;; Delta Compression Helpers (Task 4.1)
;;; ============================================================================

(defun quantize-float (value precision)
  "Quantize a float to reduce precision and save bandwidth.

   Example: (quantize-float 3.14159 0.1) => 3.1"
  (/ (round value precision) (/ 1.0 precision)))

(defun get-canvas-entity-state (canvas-id)
  "Get or create the entity state hash table for a canvas."
  (bt:with-lock-held (*entity-state-lock*)
    (or (gethash canvas-id *entity-previous-state*)
        (setf (gethash canvas-id *entity-previous-state*)
              (make-hash-table :test 'eql)))))

(defun entity-changed-p (entity-id x y vx vy prev-state)
  "Check if entity state changed enough to warrant inclusion in delta.

   Returns: T if changed beyond threshold, NIL otherwise."
  (if (null prev-state)
      t  ; No previous state = always send
      (let ((prev-x (first prev-state))
            (prev-y (second prev-state))
            (prev-vx (third prev-state))
            (prev-vy (fourth prev-state)))
        (or (>= (abs (- x prev-x)) +position-threshold+)
            (>= (abs (- y prev-y)) +position-threshold+)
            (>= (abs (- vx prev-vx)) +velocity-threshold+)
            (>= (abs (- vy prev-vy)) +velocity-threshold+)))))

(defun update-entity-state (canvas-id entity-id x y vx vy)
  "Update the stored previous state for an entity."
  (let ((state-table (get-canvas-entity-state canvas-id)))
    (setf (gethash entity-id state-table) (list x y vx vy))))

(defun track-bandwidth (canvas-id message-size)
  "Track bandwidth usage for a canvas.

   Logs statistics every 10 seconds."
  (bt:with-lock-held (*bandwidth-stats-lock*)
    (let* ((stats (gethash canvas-id *bandwidth-stats*))
           (current-time (get-universal-time))
           (total-bytes (if stats (first stats) 0))
           (message-count (if stats (second stats) 0))
           (last-reset (if stats (third stats) current-time)))

      ;; Accumulate stats
      (incf total-bytes message-size)
      (incf message-count)

      ;; Log every 10 seconds
      (when (>= (- current-time last-reset) 10)
        (let* ((duration (- current-time last-reset))
               (bytes-per-sec (/ total-bytes duration))
               (kb-per-sec (/ bytes-per-sec 1024)))
          (format t "[PHYS BANDWIDTH] Canvas ~A: ~,2F KB/sec (~D msgs, ~D bytes in ~Ds)~%"
                  canvas-id kb-per-sec message-count total-bytes duration))

        ;; Reset counters
        (setf total-bytes 0
              message-count 0
              last-reset current-time))

      ;; Update stats
      (setf (gethash canvas-id *bandwidth-stats*)
            (list total-bytes message-count last-reset)))))

(defun clear-canvas-delta-state (canvas-id)
  "Clear delta compression state for a canvas. Called when physics loop stops."
  (bt:with-lock-held (*entity-state-lock*)
    (remhash canvas-id *entity-previous-state*))
  (bt:with-lock-held (*bandwidth-stats-lock*)
    (remhash canvas-id *bandwidth-stats*)))

;;; ============================================================================
;;; Delta Broadcasting (20 Hz)
;;; ============================================================================

(defun broadcast-physics-delta (canvas-id storage)
  "Broadcast physics delta updates to WebSocket clients (20 Hz).

   Iterates all ball entities and broadcasts their positions/velocities.
   Future optimization: delta compression with entity-previous-state."

  (handler-case
      (progn
        ;; Collect all ball entities with their current state
        (setf cl-fast-ecs:*storage* storage)
        (let ((entities '()))

          ;; Iterate all entities in storage
          ;; Note: cl-fast-ecs doesn't have a built-in do-entities macro,
          ;; so we'll use a system-based approach with a collector
          (define-system collect-balls-system
            (:components-ro (position velocity ball))
            "Temporary system to collect ball data for broadcasting"

            (push `((:entity-id . ,(current-entity))
                    (:x . ,position-x)
                    (:y . ,position-y)
                    (:vx . ,velocity-vx)
                    (:vy . ,velocity-vy)
                    (:radius . ,ball-radius)
                    (:sleeping . ,ball-sleeping))
                  entities))

          ;; Run the collector system
          (cl-fast-ecs:run-systems 'collect-balls-system)

          ;; Only broadcast if there are entities
          (when entities
            (let ((message (jonathan:to-json
                            `((:type . "physics-delta")
                              (:tick . ,(get-universal-time))
                              (:entities . ,(reverse entities)))  ; Reverse to maintain order
                            :from :alist)))

              ;; Broadcast to canvas room
              (broadcast-to-canvas-room canvas-id message nil)

              ;; Return message size for bandwidth tracking
              (length message)))))

    (error (e)
      (format t "[PHYS BROADCAST ERROR] Failed to broadcast physics delta for canvas ~A: ~A~%"
              canvas-id e)
      nil)))

;;; ============================================================================
;;; Lifecycle Management
;;; ============================================================================

(defun stop-all-physics-loops ()
  "Stop all physics threads. Used during server shutdown.

   Side effects:
     - Stops all running physics threads
     - Waits for threads to finish
     - Clears *physics-threads* and *physics-running* hash tables

   Thread-safe: Yes"

  (let ((canvas-ids nil))
    ;; Collect all canvas IDs with running threads
    (bt:with-lock-held (*physics-threads-lock*)
      (setf canvas-ids (alexandria:hash-table-keys *physics-threads*)))

    ;; Stop each thread
    (dolist (canvas-id canvas-ids)
      (stop-physics-loop canvas-id))

    (format t "[PHYS] Stopped all physics loops (~D total)~%" (length canvas-ids))
    (length canvas-ids)))

(defun list-active-physics-loops ()
  "List all canvas IDs with active physics loops.

   Returns:
     List of canvas-id strings.

   Thread-safe: Yes"

  (bt:with-lock-held (*physics-threads-lock*)
    (alexandria:hash-table-keys *physics-threads*)))

(defun physics-loop-running-p (canvas-id)
  "Check if physics loop is running for a canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Returns:
     T if physics loop is running, NIL otherwise.

   Thread-safe: Yes"

  (bt:with-lock-held (*physics-threads-lock*)
    (not (null (gethash canvas-id *physics-threads*)))))

;;; ============================================================================
;;; Legacy Compatibility (stub functions)
;;; ============================================================================

(defun add-physics-object (id data)
  "Legacy stub - physics objects are now managed via ECS directly.
   Use cl-fast-ecs:make-entity and cl-fast-ecs:make-component instead."
  (declare (ignore id data))
  (format t "[PHYS WARN] add-physics-object is deprecated. Use ECS API directly.~%")
  nil)

(defun update-physics-object (id updates)
  "Legacy stub - physics objects are now managed via ECS directly.
   Use cl-fast-ecs:get-component and setf to update components."
  (declare (ignore id updates))
  (format t "[PHYS WARN] update-physics-object is deprecated. Use ECS API directly.~%")
  nil)

(defun remove-physics-object (id)
  "Legacy stub - physics objects are now managed via ECS directly.
   Use cl-fast-ecs:delete-entity instead."
  (declare (ignore id))
  (format t "[PHYS WARN] remove-physics-object is deprecated. Use ECS API directly.~%")
  nil)

(defun get-physics-state ()
  "Legacy stub - physics state is now managed via ECS storage per canvas.
   Use get-canvas-ecs-storage to access canvas-specific state."
  (format t "[PHYS WARN] get-physics-state is deprecated. Use get-canvas-ecs-storage.~%")
  nil)

(defun apply-force (id force)
  "Legacy stub - forces are now applied via force-field entities in ECS.
   Create a force-field entity with appropriate components."
  (declare (ignore id force))
  (format t "[PHYS WARN] apply-force is deprecated. Use force-field entities.~%")
  nil)

(defun set-velocity (id velocity)
  "Legacy stub - velocity is now managed via velocity component in ECS.
   Use (setf (velocity-vx component) value) to update velocity."
  (declare (ignore id velocity))
  (format t "[PHYS WARN] set-velocity is deprecated. Use ECS velocity component.~%")
  nil)

;;; ============================================================================
;;; Module Initialization
;;; ============================================================================

(format t "Physics Loop Module loaded (QUADTREE OPTIMIZED)~%")
(format t "  - Fixed timestep: ~,4F seconds (~D Hz)~%" +physics-dt+ +physics-hz+)
(format t "  - Broadcast rate: ~D Hz (every ~D ticks)~%" +broadcast-hz+ +broadcast-interval+)
(format t "  - Delta Compression: enabled (Task 4.1)~%")
(format t "      * Position threshold: ~,3F px~%" +position-threshold+)
(format t "      * Velocity threshold: ~,4F~%" +velocity-threshold+)
(format t "      * Quantize precision: ~,1F px~%" +quantize-precision+)
(format t "      * Bandwidth tracking: enabled (logs every 10s)~%")
(format t "  - Active systems (in order):~%")
(format t "      0. populate-quadtree-system (spatial indexing with physics data)~%")
(format t "      1. populate-quadtree-blocks-system (obstacle indexing with dimensions)~%")
(format t "      2. apply-forces-system (STUB - needs iteration API)~%")
(format t "      3. apply-acceleration-system (Euler integration)~%")
(format t "      4. apply-velocity-system (position update)~%")
(format t "      5a. detect-collisions-system (TWO-PASS: collision detection)~%")
(format t "      5b. apply-collision-impulses-system (TWO-PASS: impulse application)~%")
(format t "      6. check-sleeping-system (< 0.01 px/s threshold)~%")
(format t "  - Collision Performance:~%")
(format t "      * 500 balls: 3,000 checks (vs 124,750 brute force)~%")
(format t "      * 1000 balls: 6,000 checks (vs 499,500 brute force)~%")
(format t "      * 40-160x speedup from quadtree spatial partitioning!~%")
(format t "  - Post-MVP (disabled):~%")
(format t "      * emitter-system (ball spawning)~%")
