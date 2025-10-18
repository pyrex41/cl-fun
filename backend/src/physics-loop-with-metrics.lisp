;;;; physics-loop-with-metrics.lisp - Integration Guide for Metrics
;;;;
;;;; This file shows how to integrate physics-metrics.lisp into physics-loop.lisp
;;;;
;;;; INTEGRATION STEPS:
;;;;
;;;; 1. In start-physics-loop, after creating thread:
;;;;      (init-physics-metrics canvas-id)
;;;;
;;;; 2. In stop-physics-loop, before removing thread:
;;;;      (clear-physics-metrics canvas-id)
;;;;
;;;; 3. In physics-loop-worker, replace the main loop with:

(in-package #:collabcanvas)

;;; EXAMPLE MODIFIED PHYSICS-LOOP-WORKER (showing integration points):

(defun physics-loop-worker-with-metrics (canvas-id)
  "Background worker function for physics simulation with metrics tracking.

   This is an EXAMPLE showing where to integrate metrics.
   Copy these sections into the actual physics-loop-worker."

  (handler-case
      (let ((storage (get-canvas-ecs-storage canvas-id))
            (config (get-canvas-physics-config canvas-id))
            (tick-count 0))

        (unless storage
          (format t "[PHYS ERROR] No ECS storage for canvas ~A, exiting~%" canvas-id)
          (return-from physics-loop-worker-with-metrics nil))

        (unless config
          (format t "[PHYS ERROR] No physics config for canvas ~A, exiting~%" canvas-id)
          (return-from physics-loop-worker-with-metrics nil))

        ;; Extract gravity from config
        (let ((gravity-x (physics-canvas-config-gravity-x config))
              (gravity-y (physics-canvas-config-gravity-y config)))

          (format t "[PHYS] Worker started for ~A: gravity=(~,2F,~,2F) dt=~,4F~%"
                  canvas-id gravity-x gravity-y +physics-dt+)

          ;; Main simulation loop
          (loop
            ;; Check running flag
            (unless (bt:with-lock-held (*physics-running-lock*)
                      (gethash canvas-id *physics-running*))
              (format t "[PHYS] Worker stopping for canvas ~A~%" canvas-id)
              (return))

            ;;; ================== METRICS INTEGRATION START ==================
            (let ((frame-start (get-internal-real-time))
                  (system-time 0.0)
                  (metrics (bt:with-lock-held (*physics-metrics-lock*)
                            (gethash canvas-id *physics-metrics*))))

              (when metrics
                ;;; ================== RUN ECS SYSTEMS WITH TIMING ==================
                (cl-fast-ecs:with-storage (storage)
                  ;; System 1: Apply force fields to balls
                  (with-timing (system-time)
                    (cl-fast-ecs:run-system 'apply-forces-system
                                            :dt +physics-dt+))
                  (update-system-time metrics 'apply-forces-system system-time)

                  ;; System 2: Update velocity from acceleration + gravity
                  (with-timing (system-time)
                    (cl-fast-ecs:run-system 'apply-acceleration-system
                                            :dt +physics-dt+
                                            :gravity-x gravity-x
                                            :gravity-y gravity-y))
                  (update-system-time metrics 'apply-acceleration-system system-time)

                  ;; System 3: Update position from velocity
                  (with-timing (system-time)
                    (cl-fast-ecs:run-system 'apply-velocity-system
                                            :dt +physics-dt+))
                  (update-system-time metrics 'apply-velocity-system system-time)

                  ;; System 4: Detect and resolve collisions
                  (with-timing (system-time)
                    (cl-fast-ecs:run-system 'collision-system
                                            :dt +physics-dt+))
                  (update-system-time metrics 'collision-system system-time)

                  ;; System 5: Check for sleeping entities
                  (with-timing (system-time)
                    (cl-fast-ecs:run-system 'check-sleeping-system))
                  (update-system-time metrics 'check-sleeping-system system-time))

                ;;; ================== UPDATE ENTITY COUNTS ==================
                (update-entity-counts canvas-id storage metrics)

                ;;; ================== UPDATE FRAME TIME ==================
                (let ((frame-time-ms (* (- (get-internal-real-time) frame-start)
                                       (/ 1000.0 internal-time-units-per-second))))
                  (update-frame-time metrics frame-time-ms))

                ;;; ================== CHECK PERFORMANCE THRESHOLDS ==================
                (check-performance-thresholds canvas-id metrics)))
            ;;; ================== METRICS INTEGRATION END ==================

            ;; Increment tick counter
            (incf tick-count)

            ;; Broadcast delta updates every 3rd tick (20 Hz)
            (when (zerop (mod tick-count +broadcast-interval+))
              ;;; ================== TRACK BROADCAST METRICS ==================
              (let ((message-size (broadcast-physics-delta-with-metrics canvas-id storage)))
                (when message-size
                  (bt:with-lock-held (*physics-metrics-lock*)
                    (let ((metrics (gethash canvas-id *physics-metrics*)))
                      (when metrics
                        (update-broadcast-metrics metrics message-size)))))))

            ;; Sleep to maintain 60 Hz (~16.67ms per tick)
            (sleep (/ +sleep-ms+ 1000.0)))))

    (error (e)
      (format t "[PHYS ERROR] Worker crashed for canvas ~A: ~A~%" canvas-id e)
      (format t "[PHYS ERROR] Backtrace: ~A~%" (trivial-backtrace:print-backtrace e :output nil)))))

;;; EXAMPLE MODIFIED BROADCAST FUNCTION (returns message size):

(defun broadcast-physics-delta-with-metrics (canvas-id storage)
  "Modified version of broadcast-physics-delta that returns message size.

   Returns:
     Size of broadcast message in bytes, or NIL if no broadcast."

  (handler-case
      (let ((deltas nil)
            (entity-state (get-canvas-entity-state canvas-id)))

        ;; Collect entity deltas using with-storage context
        (cl-fast-ecs:with-storage (storage)
          ;; Iterate through all entities with position and velocity
          (cl-fast-ecs:do-entities (entity)
            ;; Only process entities that have both position and velocity
            (when (and (cl-fast-ecs:has-component entity 'position)
                      (cl-fast-ecs:has-component entity 'velocity))

              ;; Skip sleeping entities (they're not moving)
              (unless (cl-fast-ecs:has-component entity 'sleeping)

                ;; Get component data
                (let* ((pos (cl-fast-ecs:get-component entity 'position))
                       (vel (cl-fast-ecs:get-component entity 'velocity))
                       ;; Get raw values
                       (x (position-x pos))
                       (y (position-y pos))
                       (vx (velocity-vx vel))
                       (vy (velocity-vy vel))
                       ;; Get previous state
                       (prev-state (gethash entity entity-state)))

                  ;; Only include if changed beyond threshold
                  (when (entity-changed-p entity x y vx vy prev-state)
                    ;; Quantize values to reduce bandwidth
                    (let ((q-x (quantize-float x +quantize-precision+))
                          (q-y (quantize-float y +quantize-precision+))
                          (q-vx (quantize-float vx +quantize-precision+))
                          (q-vy (quantize-float vy +quantize-precision+)))

                      ;; Build delta entry
                      (push `((:entity-id . ,entity)
                             (:x . ,q-x)
                             (:y . ,q-y)
                             (:vx . ,q-vx)
                             (:vy . ,q-vy))
                            deltas)

                      ;; Update previous state (use quantized values)
                      (update-entity-state canvas-id entity q-x q-y q-vx q-vy))))))))

        ;; Broadcast if we have any deltas
        (when deltas
          (let ((message (to-json-string
                          `((:type . "physics-delta")
                            (:canvas-id . ,canvas-id)
                            (:deltas . ,(nreverse deltas))
                            (:timestamp . ,(get-universal-time))))))
            ;; Track bandwidth usage (existing)
            (track-bandwidth canvas-id (length message))
            ;; Broadcast to all clients (existing)
            (broadcast-to-canvas-room canvas-id message nil)
            ;; Return message size for metrics
            (length message))))

    (error (e)
      (format t "[PHYS ERROR] Failed to broadcast delta for ~A: ~A~%" canvas-id e)
      nil)))

;;; SUMMARY OF CHANGES TO MAKE IN physics-loop.lisp:
;;;
;;; 1. ADD after line 54 (after *bandwidth-stats-lock*):
;;;    ;; Load metrics module
;;;    (load "physics-metrics.lisp")
;;;
;;; 2. MODIFY start-physics-loop (around line 128):
;;;    Add after setf (gethash canvas-id *physics-threads*) thread):
;;;      (init-physics-metrics canvas-id)
;;;
;;; 3. MODIFY stop-physics-loop (around line 169):
;;;    Add before (format t "[PHYS] Stopped..."):
;;;      (clear-physics-metrics canvas-id)
;;;
;;; 4. MODIFY physics-loop-worker:
;;;    Wrap the system execution in timing macros (see above)
;;;    Update entity counts after systems run
;;;    Calculate frame time and update metrics
;;;    Check performance thresholds
;;;
;;; 5. MODIFY broadcast-physics-delta:
;;;    Return message size so it can be tracked in metrics
;;;
;;; This preserves all existing functionality while adding comprehensive metrics.
