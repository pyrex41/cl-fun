;;;; physics-loop.lisp - Physics Simulation Loop and State Broadcasting
;;;;
;;;; Implements a threaded simulation loop that:
;;;; - Runs at 50Hz (every 20ms)
;;;; - Steps the physics engine each tick
;;;; - Generates state snapshots every 2-3 ticks (~20Hz broadcast rate)
;;;; - Broadcasts snapshots via WebSocket to all connected clients

(in-package #:collabcanvas)

;;; Simulation Loop State

(defparameter *physics-loop-thread* nil
  "The background thread running the physics simulation loop")

(defparameter *physics-loop-running* nil
  "Flag indicating if the physics loop is active")

(defparameter *physics-loop-lock* (bt:make-lock "physics-loop-lock")
  "Lock for thread-safe access to physics loop state")

(defparameter *simulation-tick-count* 0
  "Total number of simulation ticks executed")

(defparameter *snapshot-interval* 3
  "Generate snapshot every N ticks (3 ticks = ~16.67Hz with 50Hz simulation)")

(defparameter *target-frame-time* 0.020
  "Target time per frame in seconds (50Hz = 0.02s = 20ms)")

;;; Performance Metrics

(defstruct physics-metrics
  "Performance metrics for the physics simulation loop"
  (total-ticks 0 :type integer)
  (total-snapshots 0 :type integer)
  (average-tick-time 0.0 :type float)
  (last-tick-time 0.0 :type float)
  (max-tick-time 0.0 :type float)
  (last-snapshot-time 0 :type integer))

(defparameter *physics-metrics* (make-physics-metrics)
  "Current physics metrics")

;;; Utility Functions

(defun get-current-time-ms ()
  "Get current time in milliseconds"
  (* (get-internal-real-time) 1000.0 internal-time-units-per-second))

(defun sleep-precise (duration)
  "Sleep for a precise duration in seconds, accounting for system overhead"
  (let ((target-time (+ (get-internal-real-time)
                        (* duration internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) target-time)
          do (sleep 0.001)))) ; Sleep in small increments for precision

;;; Snapshot Generation

(defun generate-physics-snapshot ()
  "Generate a snapshot of all dynamic physics objects for broadcasting.
   Returns an alist suitable for JSON serialization."
  (let ((snapshot (get-physics-state-snapshot)))
    ;; Convert to JSON-compatible format
    `((:type . "physics-snapshot")
      (:timestamp . ,(get-universal-time))
      (:tick . ,*simulation-tick-count*)
      (:objects . ,snapshot))))

;;; Broadcasting

(defun broadcast-physics-snapshot (snapshot)
  "Broadcast physics snapshot to all connected clients in all active canvas rooms."
  (let ((snapshot-json (to-json-string snapshot))
        (total-sent 0))
    ;; Broadcast to all active canvas rooms
    (bt:with-lock-held (*canvas-rooms-lock*)
      (maphash (lambda (canvas-id conn-ids)
                 (declare (ignore conn-ids))
                 (let ((sent (broadcast-to-canvas-room canvas-id snapshot-json nil)))
                   (incf total-sent sent)))
               *canvas-rooms*))

    ;; Update metrics
    (setf (physics-metrics-total-snapshots *physics-metrics*)
          (1+ (physics-metrics-total-snapshots *physics-metrics*)))
    (setf (physics-metrics-last-snapshot-time *physics-metrics*)
          (get-universal-time))

    total-sent))

;;; Main Simulation Loop

(defun physics-simulation-loop ()
  "Main physics simulation loop running at 50Hz.
   This function is executed in a separate thread."
  (format t "[PHYSICS LOOP] Starting simulation loop at 50Hz~%")
  (setf *simulation-tick-count* 0)

  (loop
    ;; Check if loop should continue
    (unless (bt:with-lock-held (*physics-loop-lock*)
              *physics-loop-running*)
      (format t "[PHYSICS LOOP] Stopping simulation loop~%")
      (return))

    (let ((tick-start-time (get-internal-real-time)))

      ;; Execute physics step
      (handler-case
          (progn
            ;; Step 1: Execute physics simulation
            (physics-step)

            ;; Step 2: Increment tick counter
            (incf *simulation-tick-count*)

            ;; Step 3: Generate and broadcast snapshot every N ticks
            (when (zerop (mod *simulation-tick-count* *snapshot-interval*))
              (let ((snapshot (generate-physics-snapshot)))
                (broadcast-physics-snapshot snapshot)
                (when (zerop (mod *simulation-tick-count* 150)) ; Log every 3 seconds
                  (format t "[PHYSICS LOOP] Tick ~A: broadcasted snapshot~%"
                          *simulation-tick-count*)))))

        (error (e)
          (format t "[PHYSICS LOOP ERROR] Tick ~A failed: ~A~%"
                  *simulation-tick-count* e)))

      ;; Calculate tick time and update metrics
      (let* ((tick-end-time (get-internal-real-time))
             (tick-duration (/ (- tick-end-time tick-start-time)
                              internal-time-units-per-second)))

        (setf (physics-metrics-last-tick-time *physics-metrics*) tick-duration)
        (setf (physics-metrics-total-ticks *physics-metrics*)
              (1+ (physics-metrics-total-ticks *physics-metrics*)))

        ;; Update max tick time
        (when (> tick-duration (physics-metrics-max-tick-time *physics-metrics*))
          (setf (physics-metrics-max-tick-time *physics-metrics*) tick-duration))

        ;; Update average tick time (exponential moving average)
        (setf (physics-metrics-average-tick-time *physics-metrics*)
              (+ (* 0.9 (physics-metrics-average-tick-time *physics-metrics*))
                 (* 0.1 tick-duration)))

        ;; Sleep for remaining frame time
        (let ((sleep-time (- *target-frame-time* tick-duration)))
          (when (> sleep-time 0)
            (sleep sleep-time))

          ;; Warn if we're running slow
          (when (< sleep-time 0)
            (format t "[PHYSICS LOOP WARN] Tick ~A took ~,3fms (target: ~,3fms)~%"
                    *simulation-tick-count*
                    (* tick-duration 1000)
                    (* *target-frame-time* 1000))))))))

;;; Public API

(defun start-physics-loop ()
  "Start the physics simulation loop in a background thread."
  (bt:with-lock-held (*physics-loop-lock*)
    (when *physics-loop-running*
      (format t "[PHYSICS LOOP WARN] Physics loop already running~%")
      (return-from start-physics-loop nil))

    ;; Reset metrics
    (setf *physics-metrics* (make-physics-metrics))
    (setf *simulation-tick-count* 0)

    ;; Resume physics if it was paused
    (resume-physics)

    ;; Start the loop
    (setf *physics-loop-running* t)
    (setf *physics-loop-thread*
          (bt:make-thread #'physics-simulation-loop
                          :name "physics-simulation-loop"))

    (format t "[PHYSICS LOOP] Started physics simulation loop~%")
    t))

(defun stop-physics-loop ()
  "Stop the physics simulation loop gracefully."
  (bt:with-lock-held (*physics-loop-lock*)
    (unless *physics-loop-running*
      (format t "[PHYSICS LOOP WARN] Physics loop not running~%")
      (return-from stop-physics-loop nil))

    ;; Signal loop to stop
    (setf *physics-loop-running* nil))

  ;; Wait for thread to finish (with timeout)
  (when *physics-loop-thread*
    (handler-case
        (bt:join-thread *physics-loop-thread* :timeout 5)
      (error (e)
        (format t "[PHYSICS LOOP WARN] Failed to join thread cleanly: ~A~%" e)
        ;; Force destroy if join fails
        (bt:destroy-thread *physics-loop-thread*)))

    (setf *physics-loop-thread* nil)
    (format t "[PHYSICS LOOP] Stopped physics simulation loop~%")
    t))

(defun restart-physics-loop ()
  "Restart the physics simulation loop."
  (stop-physics-loop)
  (sleep 0.1) ; Brief pause to ensure clean shutdown
  (start-physics-loop))

(defun is-physics-loop-running ()
  "Check if the physics simulation loop is currently running."
  (bt:with-lock-held (*physics-loop-lock*)
    *physics-loop-running*))

(defun get-physics-loop-metrics ()
  "Get current physics loop performance metrics."
  `((:total-ticks . ,(physics-metrics-total-ticks *physics-metrics*))
    (:total-snapshots . ,(physics-metrics-total-snapshots *physics-metrics*))
    (:average-tick-time-ms . ,(* (physics-metrics-average-tick-time *physics-metrics*) 1000))
    (:last-tick-time-ms . ,(* (physics-metrics-last-tick-time *physics-metrics*) 1000))
    (:max-tick-time-ms . ,(* (physics-metrics-max-tick-time *physics-metrics*) 1000))
    (:target-tick-time-ms . ,(* *target-frame-time* 1000))
    (:simulation-frequency-hz . ,(/ 1.0 *target-frame-time*))
    (:snapshot-frequency-hz . ,(/ 1.0 (* *target-frame-time* *snapshot-interval*)))
    (:running . ,(is-physics-loop-running))))

(defun set-snapshot-interval (interval)
  "Set the snapshot broadcast interval in ticks.
   For example: 3 ticks at 50Hz = ~16.67Hz broadcast rate"
  (when (and (integerp interval) (> interval 0))
    (setf *snapshot-interval* interval)
    (format t "[PHYSICS LOOP] Snapshot interval set to ~A ticks (~,2fHz)~%"
            interval
            (/ 1.0 (* *target-frame-time* interval)))
    t))

;;; Canvas Object Loading

(defun load-canvas-objects-into-physics (canvas-id)
  "Load all objects from a canvas into the physics engine.
   This is called when the physics system starts or when a canvas is loaded."
  (handler-case
      (let* ((canvas-objects (get-canvas-objects canvas-id))
             (loaded-count 0))
        (format t "[PHYSICS] Loading ~A objects from canvas ~A into physics~%"
                (length canvas-objects) canvas-id)

        (dolist (obj canvas-objects)
          (handler-case
              (progn
                (sync-canvas-object-to-physics obj)
                (incf loaded-count))
            (error (e)
              (format t "[PHYSICS WARN] Failed to load object ~A: ~A~%"
                      (cdr (assoc :id obj)) e))))

        (format t "[PHYSICS] Loaded ~A/~A objects into physics engine~%"
                loaded-count (length canvas-objects))
        loaded-count)
    (error (e)
      (format t "[PHYSICS ERROR] Failed to load canvas objects: ~A~%" e)
      0)))

(defun load-all-canvas-objects-into-physics ()
  "Load objects from all active canvases into the physics engine.
   This is called during physics system initialization."
  (let ((total-loaded 0))
    ;; Get all canvas IDs from active rooms
    (bt:with-lock-held (*canvas-rooms-lock*)
      (maphash (lambda (canvas-id conn-ids)
                 (declare (ignore conn-ids))
                 (incf total-loaded (load-canvas-objects-into-physics canvas-id)))
               *canvas-rooms*))

    ;; If no active rooms, try loading from a default canvas
    (when (zerop total-loaded)
      (format t "[PHYSICS] No active canvas rooms, loading from default canvas~%")
      (setf total-loaded (load-canvas-objects-into-physics "default")))

    (format t "[PHYSICS] Total objects loaded: ~A~%" total-loaded)
    total-loaded))

;;; Initialization Hook

(defun initialize-physics-system ()
  "Initialize the physics system and start the simulation loop.
   This should be called when the server starts."
  (format t "[PHYSICS] Initializing physics system...~%")

  ;; Clear any existing physics objects
  (clear-all-physics-objects)

  ;; Set default physics parameters
  (set-global-gravity *default-gravity*)
  (resume-physics)

  ;; Load existing canvas objects into physics
  (load-all-canvas-objects-into-physics)

  ;; Start the simulation loop
  (start-physics-loop)

  (format t "[PHYSICS] Physics system initialized~%"))

(defun shutdown-physics-system ()
  "Shutdown the physics system gracefully.
   This should be called when the server stops."
  (format t "[PHYSICS] Shutting down physics system...~%")

  ;; Stop the simulation loop
  (stop-physics-loop)

  ;; Pause physics
  (pause-physics)

  (format t "[PHYSICS] Physics system shut down~%"))
