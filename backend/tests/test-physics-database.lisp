;;;; test-physics-database.lisp - Tests for physics database operations

(in-package #:cl-user)

(defpackage #:collabcanvas/test-physics-database
  (:use #:cl)
  (:import-from #:collabcanvas
                #:init-physics-schema
                #:save-physics-settings
                #:load-physics-settings
                #:save-physics-component
                #:load-physics-component
                #:load-physics-components
                #:delete-physics-component
                #:count-physics-components
                #:save-physics-body
                #:load-physics-body
                #:batch-save-physics-components
                #:validate-component-type
                #:get-physics-stats
                #:init-database
                #:init-database-pool
                #:close-database-pool))

(in-package #:collabcanvas/test-physics-database)

(defun run-physics-database-tests ()
  "Run all physics database tests"
  (format t "~%=== Physics Database Integration Tests ===~%~%")

  ;; Initialize database for testing
  (format t "1. Initializing database...~%")
  (handler-case
      (progn
        (collabcanvas::init-database)
        (init-physics-schema)
        (init-database-pool 5)
        (format t "   ✓ Database initialized successfully~%"))
    (error (e)
      (format t "   ✗ Database initialization failed: ~A~%" e)
      (return-from run-physics-database-tests nil)))

  ;; Test 1: Save and load physics settings
  (format t "~%2. Testing physics settings save/load...~%")
  (handler-case
      (let* ((canvas-id "test-canvas-001")
             (gravity-x 2.5)
             (gravity-y 15.0)
             (sim-rate 120)
             (max-objs 5000))
        ;; Save settings
        (save-physics-settings canvas-id
                              :gravity-x gravity-x
                              :gravity-y gravity-y
                              :simulation-rate sim-rate
                              :max-objects max-objs)
        (format t "   ✓ Settings saved~%")

        ;; Load settings
        (let ((loaded (load-physics-settings canvas-id)))
          (assert (= (cdr (assoc :gravity-x loaded)) gravity-x))
          (assert (= (cdr (assoc :gravity-y loaded)) gravity-y))
          (assert (= (cdr (assoc :simulation-rate loaded)) sim-rate))
          (assert (= (cdr (assoc :max-objects loaded)) max-objs))
          (format t "   ✓ Settings loaded and verified~%")))
    (error (e)
      (format t "   ✗ Physics settings test failed: ~A~%" e)))

  ;; Test 2: Load default settings for non-existent canvas
  (format t "~%3. Testing default settings...~%")
  (handler-case
      (let ((defaults (load-physics-settings "nonexistent-canvas")))
        (assert (= (cdr (assoc :gravity-x defaults)) 0.0))
        (assert (= (cdr (assoc :gravity-y defaults)) 9.8))
        (assert (= (cdr (assoc :simulation-rate defaults)) 60))
        (assert (= (cdr (assoc :max-objects defaults)) 2000))
        (format t "   ✓ Default settings returned correctly~%"))
    (error (e)
      (format t "   ✗ Default settings test failed: ~A~%" e)))

  ;; Test 3: Save and load physics components
  (format t "~%4. Testing physics component save/load...~%")
  (handler-case
      (let* ((canvas-id "test-canvas-001")
             (component-id "ball-001")
             (component-type "ball")
             (properties '((:radius . 10)
                          (:mass . 5.0)
                          (:restitution . 0.8)
                          (:color . "#FF0000")))
             (user-id 1))
        ;; Save component
        (save-physics-component component-id canvas-id component-type properties user-id)
        (format t "   ✓ Component saved~%")

        ;; Load component
        (let ((loaded (load-physics-component component-id)))
          (assert (string= (cdr (assoc :id loaded)) component-id))
          (assert (string= (cdr (assoc :canvas-id loaded)) canvas-id))
          (assert (string= (cdr (assoc :component-type loaded)) component-type))
          (let ((loaded-props (cdr (assoc :properties loaded))))
            (assert (= (cdr (assoc :radius loaded-props)) 10))
            (assert (= (cdr (assoc :mass loaded-props)) 5.0))
            (format t "   ✓ Component loaded and verified~%"))))
    (error (e)
      (format t "   ✗ Component save/load test failed: ~A~%" e)))

  ;; Test 4: Load all components for a canvas
  (format t "~%5. Testing load all components...~%")
  (handler-case
      (let* ((canvas-id "test-canvas-001")
             (user-id 1))
        ;; Save multiple components
        (save-physics-component "fan-001" canvas-id "fan"
                               '((:force . 50) (:angle . 45)) user-id)
        (save-physics-component "block-001" canvas-id "block"
                               '((:width . 100) (:height . 20)) user-id)

        ;; Load all components
        (let ((components (load-physics-components canvas-id)))
          (assert (>= (length components) 3)) ; ball + fan + block
          (format t "   ✓ Loaded ~A components~%" (length components)))

        ;; Load filtered by type
        (let ((balls (load-physics-components canvas-id :component-type "ball")))
          (assert (= (length balls) 1))
          (assert (string= (cdr (assoc :component-type (first balls))) "ball"))
          (format t "   ✓ Filtered load verified~%")))
    (error (e)
      (format t "   ✗ Load components test failed: ~A~%" e)))

  ;; Test 5: Component count
  (format t "~%6. Testing component count...~%")
  (handler-case
      (let* ((canvas-id "test-canvas-001")
             (count (count-physics-components canvas-id)))
        (assert (>= count 3))
        (format t "   ✓ Component count: ~A~%" count))
    (error (e)
      (format t "   ✗ Component count test failed: ~A~%" e)))

  ;; Test 6: Physics body save/load
  (format t "~%7. Testing physics body save/load...~%")
  (handler-case
      (let* ((body-id "body-001")
             (component-id "ball-001")
             (pos-x 100.0)
             (pos-y 200.0)
             (vel-x 5.0)
             (vel-y -3.0)
             (rotation 0.5))
        ;; Save body
        (save-physics-body body-id component-id pos-x pos-y
                          :velocity-x vel-x
                          :velocity-y vel-y
                          :rotation rotation
                          :is-sleeping 0)
        (format t "   ✓ Body saved~%")

        ;; Load body
        (let ((loaded (load-physics-body body-id)))
          (assert (string= (cdr (assoc :id loaded)) body-id))
          (assert (= (cdr (assoc :position-x loaded)) pos-x))
          (assert (= (cdr (assoc :position-y loaded)) pos-y))
          (assert (= (cdr (assoc :velocity-x loaded)) vel-x))
          (assert (= (cdr (assoc :velocity-y loaded)) vel-y))
          (assert (= (cdr (assoc :rotation loaded)) rotation))
          (format t "   ✓ Body loaded and verified~%")))
    (error (e)
      (format t "   ✗ Physics body test failed: ~A~%" e)))

  ;; Test 7: Batch save components
  (format t "~%8. Testing batch save...~%")
  (handler-case
      (let ((components (list
                         (list :id "magnet-001"
                               :canvas-id "test-canvas-002"
                               :component-type "magnet"
                               :properties '((:strength . 100) (:polarity . "attract"))
                               :created-by 1)
                         (list :id "emitter-001"
                               :canvas-id "test-canvas-002"
                               :component-type "emitter"
                               :properties '((:rate . 10) (:particle-type . "ball"))
                               :created-by 1))))
        (batch-save-physics-components components)
        (format t "   ✓ Batch saved ~A components~%" (length components))

        ;; Verify
        (let ((loaded (load-physics-components "test-canvas-002")))
          (assert (= (length loaded) 2))
          (format t "   ✓ Batch save verified~%")))
    (error (e)
      (format t "   ✗ Batch save test failed: ~A~%" e)))

  ;; Test 8: Component type validation
  (format t "~%9. Testing component type validation...~%")
  (handler-case
      (progn
        (assert (validate-component-type "ball"))
        (assert (validate-component-type "fan"))
        (assert (validate-component-type "block"))
        (assert (validate-component-type "emitter"))
        (assert (validate-component-type "magnet"))
        (assert (not (validate-component-type "invalid-type")))
        (format t "   ✓ Component type validation working~%"))
    (error (e)
      (format t "   ✗ Component type validation test failed: ~A~%" e)))

  ;; Test 9: Physics statistics
  (format t "~%10. Testing physics statistics...~%")
  (handler-case
      (let ((stats (get-physics-stats "test-canvas-001")))
        (assert (> (length stats) 0))
        (format t "   ✓ Statistics retrieved:~%")
        (dolist (stat stats)
          (format t "      - ~A: ~A~%"
                  (cdr (assoc :component-type stat))
                  (cdr (assoc :count stat)))))
    (error (e)
      (format t "   ✗ Statistics test failed: ~A~%" e)))

  ;; Cleanup
  (format t "~%11. Cleaning up...~%")
  (handler-case
      (progn
        (close-database-pool)
        (format t "   ✓ Database connections closed~%"))
    (error (e)
      (format t "   ✗ Cleanup failed: ~A~%" e)))

  (format t "~%=== All Physics Database Tests Complete ===~%"))

;; Export test runner
(export 'run-physics-database-tests)
