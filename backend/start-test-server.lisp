;;;; start-test-server.lisp - Start CollabCanvas test server with Woo
;;;; This script loads the system and starts the server for testing

;; Force reload system to get latest changes
(asdf:load-system :collabcanvas :force t)

(in-package :collabcanvas)

(format t "~%=== Starting CollabCanvas Test Server (Woo) ===~%~%")

;; Start server on port 8080 for testing (without debug flag)
(start-server :port 8080)

(format t "~%Server started! Press Ctrl+C to stop~%~%")
(format t "Test endpoints:~%")
(format t "  Health: curl http://localhost:8080/health~%")
(format t "  Register: curl -X POST http://localhost:8080/api/register \\~%")
(format t "            -H 'Content-Type: application/json' \\~%")
(format t "            -d '{\"email\":\"test@example.com\",\"username\":\"testuser\",\"password\":\"password123\"}'~%")
(format t "~%WebSocket: ws://localhost:8080/ws/<canvas-id>~%~%")

;; Keep server running
(loop (sleep 1))
