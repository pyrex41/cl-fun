;;;; test-particle-lifecycle.lisp - Test particle lifecycle management

(defpackage :collabcanvas (:use :cl))
(load "src/physics.lisp")

(in-package :collabcanvas)

(format t "~%=== Testing Particle Lifecycle Management ===~%~%")

;; Test 1: Create particles
(format t "Test 1: Creating particles...~%")
(create-particle "p1" 100.0 100.0 10.0 :lifespan 2.0 :color "#ff0000")
(create-particle "p2" 200.0 200.0 15.0 :lifespan 3.0 :color "#00ff00")
(create-particle "p3" 300.0 300.0 12.0 :lifespan 5.0 :color "#0000ff")
(create-physics-circle "c1" 400.0 400.0 20.0) ; Regular circle (not a particle)

(let ((stats (get-particle-statistics)))
  (format t "  Created ~D particles and ~D regular objects~%"
          (getf stats :total-particles)
          (getf stats :regular-objects)))

;; Test 2: Check particle counting
(format t "~%Test 2: Counting particles...~%")
(format t "  Total particles: ~D~%" (count-particles))
(format t "  Max particles: ~D~%" (get-max-particles))

;; Test 3: Particle age tracking
(format t "~%Test 3: Testing particle age tracking...~%")
(let ((p1 (get-physics-object "p1")))
  (format t "  Particle p1 age: ~D seconds~%" (get-particle-age p1))
  (format t "  Particle p1 expired? ~A~%" (is-particle-expired p1)))

;; Test 4: Simulate time passing (manually update birth-time)
(format t "~%Test 4: Simulating particle expiration...~%")
(let ((p1 (get-physics-object "p1")))
  ;; Set birth-time to 3 seconds ago (p1 has 2 second lifespan)
  (setf (physics-object-birth-time p1) (- (get-universal-time) 3))
  (format t "  Updated p1 birth-time to 3 seconds ago~%")
  (format t "  Particle p1 age: ~D seconds~%" (get-particle-age p1))
  (format t "  Particle p1 expired? ~A~%" (is-particle-expired p1)))

;; Test 5: Cleanup expired particles
(format t "~%Test 5: Cleaning up expired particles...~%")
(let ((removed (cleanup-expired-particles)))
  (format t "  Removed ~D expired particle(s)~%" removed)
  (format t "  Remaining particles: ~D~%" (count-particles)))

;; Test 6: Test particle limit enforcement
(format t "~%Test 6: Testing particle limit enforcement...~%")
(set-max-particles 5)
(format t "  Set max particles to 5~%")

;; Create 10 more particles
(loop for i from 1 to 10 do
  (create-particle (format nil "test-p~D" i)
                   (* i 50.0) (* i 50.0) 8.0
                   :lifespan 10.0))

(format t "  Created 10 additional particles~%")
(format t "  Total before limit enforcement: ~D~%" (count-particles))

(let ((removed (enforce-particle-limit)))
  (format t "  Removed ~D oldest particle(s) to enforce limit~%" removed)
  (format t "  Total after limit enforcement: ~D~%" (count-particles)))

;; Test 7: Get particle statistics
(format t "~%Test 7: Particle statistics...~%")
(let ((stats (get-particle-statistics)))
  (format t "  Total objects: ~D~%" (getf stats :total-objects))
  (format t "  Total particles: ~D~%" (getf stats :total-particles))
  (format t "  Regular objects: ~D~%" (getf stats :regular-objects))
  (format t "  Oldest particle age: ~D seconds~%" (getf stats :oldest-particle-age))
  (format t "  Particle capacity: ~D~%" (getf stats :particle-capacity))
  (format t "  Particle usage: ~,1F%~%" (getf stats :particle-usage-percent)))

(format t "~%=== All Tests Completed ===~%~%")
