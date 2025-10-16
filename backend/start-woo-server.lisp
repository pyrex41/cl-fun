;;;; start-woo-server.lisp - Start CollabCanvas with NEW Woo implementation
;;;; Ensures we load from the woo worktree, not the old codebase

;; Clear any cached fasl files and force reload from source
(setf asdf:*central-registry* nil)
(push (truename ".") asdf:*central-registry*)

(format t "~%Loading CollabCanvas from: ~A~%" (truename "."))

;; Force clean reload
(asdf:clear-system :collabcanvas)
(asdf:load-system :collabcanvas :force t)

(in-package :collabcanvas)

(format t "~%=== Starting CollabCanvas with Woo ===~%")
(format t "Port: 9090~%")
(format t "Implementation: Woo/Clack~%~%")

;; Start server on port 9090
(start-server :port 9090 :address "0.0.0.0")

(format t "~%✓ Server running!~%")
(format t "~%Test endpoints:~%")
(format t "  curl http://localhost:9090/health~%")
(format t "  curl -X POST http://localhost:9090/api/register \\~%")
(format t "    -H 'Content-Type: application/json' \\~%")
(format t "    -d '{\"email\":\"test@example.com\",\"username\":\"testuser\",\"password\":\"pass123\"}'~%")
(format t "~%Press Ctrl+C to stop~%~%")

;; Keep running
(loop (sleep 1))
