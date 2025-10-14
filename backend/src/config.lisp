;;;; config.lisp - Configuration settings for CollabCanvas

(in-package #:collabcanvas)

;;; Server Configuration
(defparameter *port* 8080
  "HTTP server port")

(defparameter *host* "0.0.0.0"
  "Host to bind the server to")

(defparameter *websocket-port* 8080
  "WebSocket server port (same as HTTP for simplicity)")

;;; Database Configuration
(defparameter *database-path*
  (or (uiop:getenv "DATABASE_PATH")
      "data/canvas.db")
  "Path to SQLite database file")

(defparameter *database-lock* (bt:make-lock "database-lock")
  "Lock for database operations")

(defparameter *database-pool-size* 10
  "Number of connections in the database pool")

(defparameter *database-connection-timeout* 5
  "Timeout in seconds when waiting for a database connection from pool")

;;; Frontend Configuration
(defparameter *frontend-path*
  (or (uiop:getenv "FRONTEND_PATH")
      (merge-pathnames "../frontend/dist/" (uiop:getcwd)))
  "Path to frontend static files")

;;; Session Configuration
(defparameter *session-timeout* (* 24 60 60)
  "Session timeout in seconds (24 hours)")

(defparameter *session-cookie-name* "collabcanvas-session"
  "Name of the session cookie")

;;; Canvas Configuration
(defparameter *max-canvas-size* (* 10000 10000)
  "Maximum canvas dimensions in pixels")

(defparameter *default-canvas-width* 1920
  "Default canvas width")

(defparameter *default-canvas-height* 1080
  "Default canvas height")

(defparameter *max-objects-per-canvas* 10000
  "Maximum number of objects per canvas")

;;; WebSocket Configuration
(defparameter *websocket-ping-interval* 30
  "WebSocket ping interval in seconds")

(defparameter *max-message-size* (* 1024 1024)
  "Maximum WebSocket message size in bytes (1MB)")

(defparameter *cursor-update-throttle* (/ 1.0 30)
  "Minimum time between cursor updates in seconds (30 FPS)")

;;; Performance Configuration
(defparameter *state-save-debounce* 0.5
  "Debounce time for saving canvas state in seconds")

(defparameter *max-concurrent-users* 100
  "Maximum concurrent users per canvas")

;;; Development Configuration
(defparameter *debug-mode* t
  "Enable debug logging and development features")

(defparameter *cors-enabled* t
  "Enable CORS for development")

(defparameter *allowed-origins*
  '("http://localhost:6465"
    "http://localhost:5173"
    "http://localhost:3000"
    "https://cl-fun.fly.dev")
  "List of allowed CORS origins")