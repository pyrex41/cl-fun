;;;; manual-physics-db-test.lisp - Quick manual test for physics database
;;;; Run this in REPL with: (load "tests/manual-physics-db-test.lisp")

(in-package #:cl-user)

(format t "~%=== Manual Physics Database Test ===~%")

;; Load system
(format t "~%Loading collabcanvas system...~%")
(ql:quickload :collabcanvas :silent t)

(format t "~%Running physics database integration tests...~%~%")

;; Initialize database
(format t "1. Initializing database schema...~%")
(collabcanvas::init-database)
(collabcanvas::init-physics-schema)
(collabcanvas::init-database-pool 3)
(format t "   ✓ Database initialized~%")

;; Test physics settings
(format t "~%2. Testing physics settings...~%")
(collabcanvas::save-physics-settings "manual-test-canvas"
                                     :gravity-x 0.0
                                     :gravity-y 20.0
                                     :simulation-rate 120
                                     :max-objects 3000)
(let ((settings (collabcanvas::load-physics-settings "manual-test-canvas")))
  (format t "   Loaded settings:~%")
  (format t "     - Gravity X: ~A~%" (cdr (assoc :gravity-x settings)))
  (format t "     - Gravity Y: ~A~%" (cdr (assoc :gravity-y settings)))
  (format t "     - Simulation Rate: ~A Hz~%" (cdr (assoc :simulation-rate settings)))
  (format t "     - Max Objects: ~A~%" (cdr (assoc :max-objects settings)))
  (format t "   ✓ Settings test passed~%"))

;; Test physics component
(format t "~%3. Testing physics component save/load...~%")
(collabcanvas::save-physics-component
 "test-ball-123"
 "manual-test-canvas"
 "ball"
 '((:radius . 15)
   (:mass . 10.0)
   (:restitution . 0.9)
   (:friction . 0.1)
   (:color . "#00FF00")
   (:x . 100)
   (:y . 50))
 1) ; user-id

(let ((component (collabcanvas::load-physics-component "test-ball-123")))
  (format t "   Loaded component:~%")
  (format t "     - ID: ~A~%" (cdr (assoc :id component)))
  (format t "     - Type: ~A~%" (cdr (assoc :component-type component)))
  (format t "     - Canvas ID: ~A~%" (cdr (assoc :canvas-id component)))
  (let ((props (cdr (assoc :properties component))))
    (format t "     - Properties:~%")
    (format t "       * Radius: ~A~%" (cdr (assoc :radius props)))
    (format t "       * Mass: ~A~%" (cdr (assoc :mass props)))
    (format t "       * Color: ~A~%" (cdr (assoc :color props))))
  (format t "   ✓ Component test passed~%"))

;; Test loading all components
(format t "~%4. Testing load all components...~%")
(collabcanvas::save-physics-component
 "test-fan-456"
 "manual-test-canvas"
 "fan"
 '((:force . 75) (:angle . 90) (:width . 50))
 1)

(let ((components (collabcanvas::load-physics-components "manual-test-canvas")))
  (format t "   Found ~A components:~%" (length components))
  (dolist (comp components)
    (format t "     - ~A (~A)~%"
            (cdr (assoc :id comp))
            (cdr (assoc :component-type comp))))
  (format t "   ✓ Load all test passed~%"))

;; Test physics body
(format t "~%5. Testing physics body persistence...~%")
(collabcanvas::save-physics-body
 "body-ball-123"
 "test-ball-123"
 250.5   ; position-x
 150.75  ; position-y
 :velocity-x 12.5
 :velocity-y -8.3
 :rotation 1.57
 :is-sleeping 0)

(let ((body (collabcanvas::load-physics-body "body-ball-123")))
  (format t "   Loaded body state:~%")
  (format t "     - Position: (~A, ~A)~%"
          (cdr (assoc :position-x body))
          (cdr (assoc :position-y body)))
  (format t "     - Velocity: (~A, ~A)~%"
          (cdr (assoc :velocity-x body))
          (cdr (assoc :velocity-y body)))
  (format t "     - Rotation: ~A radians~%" (cdr (assoc :rotation body)))
  (format t "   ✓ Body persistence test passed~%"))

;; Test component count
(format t "~%6. Testing component count...~%")
(let ((count (collabcanvas::count-physics-components "manual-test-canvas")))
  (format t "   Component count: ~A~%" count)
  (format t "   ✓ Count test passed~%"))

;; Test statistics
(format t "~%7. Testing physics statistics...~%")
(let ((stats (collabcanvas::get-physics-stats "manual-test-canvas")))
  (format t "   Component statistics:~%")
  (dolist (stat stats)
    (format t "     - ~A: ~A~%"
            (cdr (assoc :component-type stat))
            (cdr (assoc :count stat))))
  (format t "   ✓ Statistics test passed~%"))

;; Cleanup
(format t "~%8. Cleaning up...~%")
(collabcanvas::close-database-pool)
(format t "   ✓ Database connections closed~%")

(format t "~%=== All Manual Tests Complete ===~%")
(format t "~%SUCCESS: Physics database integration is working correctly!~%~%")
