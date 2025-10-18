;;;; physics-metrics.lisp - Performance Monitoring and Metrics for Physics
;;;;
;;;; Provides comprehensive metrics tracking for physics simulations:
;;;;   - Physics stats (active/sleeping bodies, force fields, collisions)
;;;;   - Network metrics (bandwidth, message sizes, broadcast frequency)
;;;;   - Performance metrics (frame time, FPS, per-system profiling)
;;;;   - Warnings for threshold violations
;;;;
;;;; Integration:
;;;;   - Called from physics-loop.lisp during simulation loop
;;;;   - Accessed externally via get-physics-metrics
;;;;
;;;; PRD Section: 4.2 - Monitoring and Metrics

(in-package #:collabcanvas)

;;; ============================================================================
;;; Metrics Structure
;;; ============================================================================

(defstruct physics-metrics
  "Performance and diagnostic metrics for a canvas physics loop."
  ;; Physics-specific stats
  (active-bodies 0 :type fixnum)
  (sleeping-bodies 0 :type fixnum)
  (force-fields 0 :type fixnum)
  (collision-count 0 :type fixnum)

  ;; Network metrics
  (delta-message-bytes 0 :type fixnum)
  (broadcast-count 0 :type fixnum)
  (total-bytes-sent 0 :type fixnum)

  ;; Performance metrics
  (frame-time-ms 0.0 :type single-float)
  (avg-frame-time-ms 0.0 :type single-float)
  (system-times (make-hash-table :test 'equal) :type hash-table)
  (fps 0.0 :type single-float)

  ;; Warnings
  (high-latency-warnings 0 :type fixnum)
  (high-bandwidth-warnings 0 :type fixnum)
  (high-collision-warnings 0 :type fixnum)
  (last-warning-time 0 :type integer))

(defparameter *physics-metrics* (make-hash-table :test 'equal)
  "Hash table mapping canvas-id (string) to physics-metrics structures.")

(defparameter *physics-metrics-lock* (bt:make-lock "physics-metrics-lock")
  "Thread-safe lock for accessing *physics-metrics* hash table.")

;;; ============================================================================
;;; Performance Thresholds
;;; ============================================================================

(defconstant +frame-time-warning-ms+ 16.0
  "Warn if frame time exceeds 16ms (60 FPS threshold).")

(defconstant +bandwidth-warning-kb+ 280
  "Warn if bandwidth exceeds 280 KB/sec.")

(defconstant +latency-warning-ms+ 100
  "Warn if latency exceeds 100ms.")

(defconstant +collision-threshold+ 100
  "Warn if collision count exceeds this per tick.")

;;; ============================================================================
;;; Core Functions
;;; ============================================================================

(defun init-physics-metrics (canvas-id)
  "Initialize metrics tracking for a canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Side effects:
     - Creates new physics-metrics structure
     - Stores in *physics-metrics* hash table
     - Thread-safe operation

   Example:
     (init-physics-metrics \"canvas-123\")"

  (bt:with-lock-held (*physics-metrics-lock*)
    (setf (gethash canvas-id *physics-metrics*)
          (make-physics-metrics))))

(defun clear-physics-metrics (canvas-id)
  "Clear metrics tracking for a canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Side effects:
     - Removes metrics from *physics-metrics* hash table
     - Thread-safe operation

   Example:
     (clear-physics-metrics \"canvas-123\")"

  (bt:with-lock-held (*physics-metrics-lock*)
    (remhash canvas-id *physics-metrics*)))

(defun get-physics-metrics (canvas-id)
  "Retrieve current metrics for a canvas.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Returns:
     Association list with current metrics, or NIL if not found.

   Thread-safe: Yes

   Example:
     (get-physics-metrics \"canvas-123\")
     => ((:active-bodies . 42) (:sleeping-bodies . 8) ...)"

  (bt:with-lock-held (*physics-metrics-lock*)
    (let ((metrics (gethash canvas-id *physics-metrics*)))
      (when metrics
        `((:active-bodies . ,(physics-metrics-active-bodies metrics))
          (:sleeping-bodies . ,(physics-metrics-sleeping-bodies metrics))
          (:force-fields . ,(physics-metrics-force-fields metrics))
          (:collision-count . ,(physics-metrics-collision-count metrics))
          (:delta-message-bytes . ,(physics-metrics-delta-message-bytes metrics))
          (:broadcast-count . ,(physics-metrics-broadcast-count metrics))
          (:total-bytes-sent . ,(physics-metrics-total-bytes-sent metrics))
          (:frame-time-ms . ,(physics-metrics-frame-time-ms metrics))
          (:avg-frame-time-ms . ,(physics-metrics-avg-frame-time-ms metrics))
          (:fps . ,(physics-metrics-fps metrics))
          (:high-latency-warnings . ,(physics-metrics-high-latency-warnings metrics))
          (:high-bandwidth-warnings . ,(physics-metrics-high-bandwidth-warnings metrics))
          (:high-collision-warnings . ,(physics-metrics-high-collision-warnings metrics))
          (:system-times . ,(alexandria:hash-table-alist
                            (physics-metrics-system-times metrics))))))))

;;; ============================================================================
;;; Metrics Collection
;;; ============================================================================

(defun update-entity-counts (canvas-id storage metrics)
  "Count active bodies, sleeping bodies, and force fields.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)
     storage - ECS storage instance
     metrics - physics-metrics structure to update

   Side effects:
     - Updates active-bodies, sleeping-bodies, force-fields in metrics structure

   Example:
     (update-entity-counts \"canvas-123\" storage metrics)"

  (declare (ignore canvas-id))

  (let ((active 0)
        (sleeping 0)
        (fields 0))

    (cl-fast-ecs:with-storage (storage)
      ;; Count entities
      (cl-fast-ecs:do-entities (entity)
        (cond
          ;; Force field
          ((cl-fast-ecs:has-component entity 'force-field)
           (incf fields))

          ;; Ball entity
          ((cl-fast-ecs:has-component entity 'ball)
           (if (cl-fast-ecs:has-component entity 'sleeping)
               (incf sleeping)
               (incf active))))))

    ;; Update metrics
    (setf (physics-metrics-active-bodies metrics) active
          (physics-metrics-sleeping-bodies metrics) sleeping
          (physics-metrics-force-fields metrics) fields)))

(defun count-collisions (canvas-id storage)
  "Count number of collisions in current tick.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)
     storage - ECS storage instance

   Returns:
     Number of collisions detected

   Note: This is a simplified count. For production, track in collision-system."

  (declare (ignore canvas-id))

  (let ((collision-count 0))

    (cl-fast-ecs:with-storage (storage)
      ;; Count ball entities (approximate collision count)
      (cl-fast-ecs:do-entities (entity)
        (when (cl-fast-ecs:has-component entity 'ball)
          (incf collision-count))))

      ;; Approximate: N balls can have at most N*(N-1)/2 collisions
      ;; Return conservative estimate
      (floor (* collision-count 0.1)))

    collision-count))

(defun update-frame-time (metrics frame-time-ms)
  "Update frame time metrics with moving average.

   Arguments:
     metrics - physics-metrics structure
     frame-time-ms - Current frame time in milliseconds

   Side effects:
     - Updates frame-time-ms
     - Updates avg-frame-time-ms (exponential moving average)
     - Updates fps

   Example:
     (update-frame-time metrics 15.2)"

  (setf (physics-metrics-frame-time-ms metrics) frame-time-ms)

  ;; Calculate exponential moving average (alpha = 0.1)
  (let ((current-avg (physics-metrics-avg-frame-time-ms metrics)))
    (setf (physics-metrics-avg-frame-time-ms metrics)
          (+ (* 0.9 current-avg) (* 0.1 frame-time-ms))))

  ;; Update FPS (avoid division by zero)
  (when (> frame-time-ms 0.0)
    (setf (physics-metrics-fps metrics)
          (/ 1000.0 frame-time-ms))))

(defun update-system-time (metrics system-name time-ms)
  "Record execution time for a specific ECS system.

   Arguments:
     metrics - physics-metrics structure
     system-name - Name of the system (string or symbol)
     time-ms - Execution time in milliseconds

   Side effects:
     - Updates system-times hash table

   Example:
     (update-system-time metrics 'collision-system 2.5)"

  (setf (gethash (string system-name)
                 (physics-metrics-system-times metrics))
        time-ms))

(defun update-broadcast-metrics (metrics message-size)
  "Update network broadcast metrics.

   Arguments:
     metrics - physics-metrics structure
     message-size - Size of delta message in bytes

   Side effects:
     - Increments broadcast-count
     - Updates delta-message-bytes
     - Updates total-bytes-sent

   Example:
     (update-broadcast-metrics metrics 1024)"

  (incf (physics-metrics-broadcast-count metrics))
  (setf (physics-metrics-delta-message-bytes metrics) message-size)
  (incf (physics-metrics-total-bytes-sent metrics) message-size))

;;; ============================================================================
;;; Warning System
;;; ============================================================================

(defun log-performance-warning (canvas-id metrics warning-type value threshold)
  "Log a performance warning if threshold is exceeded.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)
     metrics - physics-metrics structure
     warning-type - Type of warning (:frame-time, :bandwidth, :latency, :collisions)
     value - Current value
     threshold - Threshold value

   Side effects:
     - Logs warning message to console (max once per second)
     - Increments appropriate warning counter

   Example:
     (log-performance-warning \"canvas-123\" metrics :frame-time 18.5 16.0)"

  (let ((now (get-universal-time))
        (last-warn (physics-metrics-last-warning-time metrics)))

    ;; Only log warning once per second to avoid spam
    (when (or (zerop last-warn) (> (- now last-warn) 1))
      (format t "[PHYS WARN] Canvas ~A: ~A exceeded threshold (~,2F > ~,2F)~%"
              canvas-id warning-type value threshold)
      (setf (physics-metrics-last-warning-time metrics) now)

      ;; Increment warning counter
      (case warning-type
        (:frame-time
         (incf (physics-metrics-high-latency-warnings metrics)))
        (:bandwidth
         (incf (physics-metrics-high-bandwidth-warnings metrics)))
        (:collisions
         (incf (physics-metrics-high-collision-warnings metrics)))))))

(defun check-performance-thresholds (canvas-id metrics)
  "Check metrics against performance thresholds and log warnings.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)
     metrics - physics-metrics structure

   Side effects:
     - Logs warnings if thresholds exceeded
     - Increments warning counters

   Called every tick to monitor performance."

  ;; Check frame time
  (let ((frame-time (physics-metrics-frame-time-ms metrics)))
    (when (> frame-time +frame-time-warning-ms+)
      (log-performance-warning canvas-id metrics :frame-time
                              frame-time +frame-time-warning-ms+)))

  ;; Check collision count
  (let ((collisions (physics-metrics-collision-count metrics)))
    (when (> collisions +collision-threshold+)
      (log-performance-warning canvas-id metrics :collisions
                              (float collisions) (float +collision-threshold+))))

  ;; Check bandwidth (calculate from total bytes)
  (let* ((total-bytes (physics-metrics-total-bytes-sent metrics))
         (broadcast-count (physics-metrics-broadcast-count metrics))
         ;; Estimate bandwidth: bytes / (broadcasts / 20 Hz) = bytes/sec
         (kb-per-sec (if (> broadcast-count 0)
                        (/ total-bytes 1024.0 (/ broadcast-count 20.0))
                        0.0)))
    (when (> kb-per-sec +bandwidth-warning-kb+)
      (log-performance-warning canvas-id metrics :bandwidth
                              kb-per-sec (float +bandwidth-warning-kb+)))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defmacro with-timing ((result-var) &body body)
  "Execute body and measure execution time in milliseconds.

   Arguments:
     result-var - Symbol to bind execution time to (in ms)
     body - Code to execute

   Returns:
     Result of body execution

   Example:
     (with-timing (time)
       (expensive-computation))
     => time contains execution time in ms"

  `(let ((start (get-internal-real-time)))
     (prog1
         (progn ,@body)
       (setf ,result-var
             (* (- (get-internal-real-time) start)
                (/ 1000.0 internal-time-units-per-second))))))

(defun get-metrics-summary (canvas-id)
  "Get a human-readable summary of physics metrics.

   Arguments:
     canvas-id - Unique identifier for the canvas (string)

   Returns:
     Formatted string with metrics summary

   Example:
     (get-metrics-summary \"canvas-123\")
     => \"Active: 42, Sleeping: 8, FPS: 59.8, ...\""

  (let ((metrics (get-physics-metrics canvas-id)))
    (if metrics
        (format nil "Active:~D Sleep:~D Fields:~D Colls:~D FPS:~,1F FrameTime:~,2Fms Bandwidth:~DKB/s"
                (cdr (assoc :active-bodies metrics))
                (cdr (assoc :sleeping-bodies metrics))
                (cdr (assoc :force-fields metrics))
                (cdr (assoc :collision-count metrics))
                (cdr (assoc :fps metrics))
                (cdr (assoc :frame-time-ms metrics))
                (floor (cdr (assoc :total-bytes-sent metrics)) 1024))
        "No metrics available")))

;;; ============================================================================
;;; Module Initialization
;;; ============================================================================

(format t "Physics Metrics Module loaded~%")
(format t "  - Performance thresholds:~%")
(format t "      Frame time: ~,1F ms~%" +frame-time-warning-ms+)
(format t "      Bandwidth: ~D KB/s~%" +bandwidth-warning-kb+)
(format t "      Latency: ~D ms~%" +latency-warning-ms+)
(format t "      Collisions: ~D per tick~%" +collision-threshold+)
