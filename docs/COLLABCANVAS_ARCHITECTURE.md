# CollabCanvas - Common Lisp Implementation
## Real-Time Collaborative Design Tool with AI

### Technology Stack

**Backend:**
- Common Lisp (SBCL via Roswell)
- Hunchentoot (HTTP server)
- Hunchensocket (WebSocket support)
- SQLite (cl-sqlite)
- Jonathan (JSON parsing)
- Ironclad (password hashing)
- Bordeaux-threads (concurrency)

**Frontend:**
- PixiJS v7 (high-performance 2D rendering)
- Vanilla JavaScript (ES6+)
- WebSocket API (native)
- Vite (dev server and build tool)

**Infrastructure:**
- Fly.io (deployment)
- SQLite (embedded database)

---

## Project Structure

```
collabcanvas/
├── backend/
│   ├── collabcanvas.asd          # ASDF system definition
│   ├── src/
│   │   ├── main.lisp             # Entry point, server setup
│   │   ├── config.lisp           # Configuration
│   │   ├── database.lisp         # SQLite operations
│   │   ├── auth.lisp             # Authentication logic
│   │   ├── websocket.lisp        # WebSocket handlers
│   │   ├── canvas-state.lisp     # Canvas state management
│   │   ├── ai-agent.lisp         # AI integration
│   │   └── utils.lisp            # Utility functions
│   ├── db/
│   │   └── schema.sql            # Database schema
│   ├── start.sh                  # Development startup script
│   └── build.sh                  # Production build script
├── frontend/
│   ├── package.json
│   ├── vite.config.js
│   ├── index.html
│   ├── src/
│   │   ├── main.js               # Entry point
│   │   ├── canvas.js             # PixiJS canvas logic
│   │   ├── websocket.js          # WebSocket client
│   │   ├── auth.js               # Auth UI and logic
│   │   ├── ai-commands.js        # AI command interface
│   │   └── styles.css
│   └── public/
│       └── assets/
├── fly.toml                      # Fly.io configuration
├── Dockerfile                    # Container definition
└── README.md
```

---

## Database Schema

```sql
-- Users table
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    username TEXT UNIQUE NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Sessions table
CREATE TABLE sessions (
    id TEXT PRIMARY KEY,
    user_id INTEGER NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    expires_at DATETIME NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Canvas states table
CREATE TABLE canvas_states (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    canvas_id TEXT UNIQUE NOT NULL,
    state TEXT NOT NULL,  -- JSON blob
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Indexes
CREATE INDEX idx_sessions_user ON sessions(user_id);
CREATE INDEX idx_sessions_expires ON sessions(expires_at);
CREATE INDEX idx_canvas_id ON canvas_states(canvas_id);
```

---

## Backend Architecture

### 1. System Definition (collabcanvas.asd)

```lisp
(asdf:defsystem "collabcanvas"
  :description "Real-time collaborative canvas with AI"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (#:hunchentoot
               #:hunchensocket
               #:bordeaux-threads
               #:sqlite
               #:jonathan
               #:ironclad
               #:cl-base64
               #:babel
               #:uuid
               #:local-time
               #:cl-ppcre)
  :components ((:module "src"
                :serial t
                :components ((:file "config")
                            (:file "utils")
                            (:file "database")
                            (:file "auth")
                            (:file "canvas-state")
                            (:file "websocket")
                            (:file "ai-agent")
                            (:file "main")))))
```

### 2. Configuration Module

```lisp
;; src/config.lisp
(defpackage #:collabcanvas.config
  (:use #:cl)
  (:export #:*port*
           #:*db-path*
           #:*jwt-secret*
           #:*session-timeout*
           #:*cors-origin*))

(in-package #:collabcanvas.config)

(defparameter *port* 8080)
(defparameter *db-path* "./data/canvas.db")
(defparameter *jwt-secret* "your-secret-key-change-in-production")
(defparameter *session-timeout* (* 24 60 60)) ; 24 hours in seconds
(defparameter *cors-origin* "*")
```

### 3. Database Module

```lisp
;; src/database.lisp
(defpackage #:collabcanvas.database
  (:use #:cl #:sqlite)
  (:import-from #:collabcanvas.config #:*db-path*)
  (:export #:init-db
           #:create-user
           #:find-user-by-email
           #:create-session
           #:find-session
           #:delete-session
           #:save-canvas-state
           #:load-canvas-state))

(in-package #:collabcanvas.database)

(defvar *db* nil)

(defun init-db ()
  "Initialize database connection and create tables"
  (setf *db* (connect *db-path*))
  (execute-non-query *db*
    "CREATE TABLE IF NOT EXISTS users (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       email TEXT UNIQUE NOT NULL,
       password_hash TEXT NOT NULL,
       username TEXT UNIQUE NOT NULL,
       created_at DATETIME DEFAULT CURRENT_TIMESTAMP)")
  (execute-non-query *db*
    "CREATE TABLE IF NOT EXISTS sessions (
       id TEXT PRIMARY KEY,
       user_id INTEGER NOT NULL,
       created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
       expires_at DATETIME NOT NULL,
       FOREIGN KEY (user_id) REFERENCES users(id))")
  (execute-non-query *db*
    "CREATE TABLE IF NOT EXISTS canvas_states (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       canvas_id TEXT UNIQUE NOT NULL,
       state TEXT NOT NULL,
       updated_at DATETIME DEFAULT CURRENT_TIMESTAMP)")
  (format t "Database initialized~%"))

(defun create-user (email password-hash username)
  "Create a new user"
  (execute-non-query *db*
    "INSERT INTO users (email, password_hash, username) VALUES (?, ?, ?)"
    email password-hash username))

(defun find-user-by-email (email)
  "Find user by email"
  (execute-single *db*
    "SELECT id, email, password_hash, username FROM users WHERE email = ?"
    email))

(defun create-session (session-id user-id expires-at)
  "Create a new session"
  (execute-non-query *db*
    "INSERT INTO sessions (id, user_id, expires_at) VALUES (?, ?, ?)"
    session-id user-id expires-at))

(defun find-session (session-id)
  "Find session by ID"
  (execute-single *db*
    "SELECT id, user_id, expires_at FROM sessions WHERE id = ? AND expires_at > datetime('now')"
    session-id))

(defun delete-session (session-id)
  "Delete a session"
  (execute-non-query *db*
    "DELETE FROM sessions WHERE id = ?"
    session-id))

(defun save-canvas-state (canvas-id state-json)
  "Save or update canvas state"
  (execute-non-query *db*
    "INSERT OR REPLACE INTO canvas_states (canvas_id, state, updated_at) 
     VALUES (?, ?, datetime('now'))"
    canvas-id state-json))

(defun load-canvas-state (canvas-id)
  "Load canvas state"
  (let ((result (execute-single *db*
                  "SELECT state FROM canvas_states WHERE canvas_id = ?"
                  canvas-id)))
    (when result
      (car result))))
```

### 4. Authentication Module

```lisp
;; src/auth.lisp
(defpackage #:collabcanvas.auth
  (:use #:cl)
  (:import-from #:ironclad
                #:digest-sequence
                #:ascii-string-to-byte-array)
  (:import-from #:cl-base64
                #:usb8-array-to-base64-string)
  (:import-from #:uuid
                #:make-v4-uuid)
  (:import-from #:collabcanvas.database
                #:create-user
                #:find-user-by-email
                #:create-session
                #:find-session
                #:delete-session)
  (:import-from #:collabcanvas.config
                #:*session-timeout*)
  (:export #:hash-password
           #:verify-password
           #:register-user
           #:login-user
           #:verify-session
           #:logout-user))

(in-package #:collabcanvas.auth)

(defun hash-password (password)
  "Hash password using SHA-256"
  (let* ((password-bytes (babel:string-to-octets password :encoding :utf-8))
         (hash (digest-sequence :sha256 password-bytes)))
    (usb8-array-to-base64-string hash)))

(defun verify-password (password hash)
  "Verify password against hash"
  (string= (hash-password password) hash))

(defun register-user (email password username)
  "Register a new user"
  (let ((password-hash (hash-password password)))
    (handler-case
        (progn
          (create-user email password-hash username)
          (list :success t :message "User registered successfully"))
      (error (e)
        (list :success nil :message (format nil "Registration failed: ~A" e))))))

(defun login-user (email password)
  "Authenticate user and create session"
  (let ((user (find-user-by-email email)))
    (if (and user (verify-password password (third user)))
        (let* ((session-id (format nil "~A" (make-v4-uuid)))
               (user-id (first user))
               (username (fourth user))
               (expires-at (+ (get-universal-time) *session-timeout*)))
          (create-session session-id user-id 
                         (format nil "~A" (local-time:unix-to-timestamp expires-at)))
          (list :success t 
                :session-id session-id 
                :user-id user-id
                :username username))
        (list :success nil :message "Invalid credentials"))))

(defun verify-session (session-id)
  "Verify session is valid"
  (find-session session-id))

(defun logout-user (session-id)
  "Logout user by deleting session"
  (delete-session session-id)
  (list :success t))
```

### 5. WebSocket Module

```lisp
;; src/websocket.lisp
(defpackage #:collabcanvas.websocket
  (:use #:cl #:hunchensocket)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:import-from #:jonathan
                #:parse
                #:to-json)
  (:import-from #:collabcanvas.auth
                #:verify-session)
  (:export #:canvas-room
           #:start-websocket))

(in-package #:collabcanvas.websocket)

;; Global state
(defvar *rooms* (make-hash-table :test 'equal))
(defvar *rooms-lock* (make-lock "rooms-lock"))

;; Client tracking
(defclass canvas-client ()
  ((user-id :initarg :user-id :accessor client-user-id)
   (username :initarg :username :accessor client-username)
   (canvas-id :initarg :canvas-id :accessor client-canvas-id)
   (cursor-x :initform 0 :accessor client-cursor-x)
   (cursor-y :initform 0 :accessor client-cursor-y)))

;; WebSocket resource
(defclass canvas-room (websocket-resource)
  ((canvas-id :initarg :canvas-id :accessor room-canvas-id)
   (clients :initform '() :accessor room-clients)
   (clients-lock :initform (make-lock "clients-lock") :accessor room-clients-lock)))

(defun get-or-create-room (canvas-id)
  "Get existing room or create new one"
  (with-lock-held (*rooms-lock*)
    (or (gethash canvas-id *rooms*)
        (setf (gethash canvas-id *rooms*)
              (make-instance 'canvas-room :canvas-id canvas-id)))))

(defmethod client-connected ((room canvas-room) client)
  "Handle new client connection"
  (format t "Client connected to canvas ~A~%" (room-canvas-id room)))

(defmethod client-disconnected ((room canvas-room) client)
  "Handle client disconnection"
  (with-lock-held ((room-clients-lock room))
    (setf (room-clients room)
          (remove client (room-clients room))))
  (broadcast-presence room)
  (format t "Client disconnected from canvas ~A~%" (room-canvas-id room)))

(defmethod text-message-received ((room canvas-room) client message)
  "Handle incoming WebSocket message"
  (handler-case
      (let* ((data (parse message))
             (msg-type (gethash "type" data)))
        (cond
          ((string= msg-type "auth")
           (handle-auth room client data))
          ((string= msg-type "cursor")
           (handle-cursor room client data))
          ((string= msg-type "object-create")
           (handle-object-create room client data))
          ((string= msg-type "object-update")
           (handle-object-update room client data))
          ((string= msg-type "object-delete")
           (handle-object-delete room client data))
          ((string= msg-type "ai-command")
           (handle-ai-command room client data))
          (t
           (format t "Unknown message type: ~A~%" msg-type))))
    (error (e)
      (format t "Error processing message: ~A~%" e))))

(defun handle-auth (room client data)
  "Authenticate client"
  (let* ((session-id (gethash "sessionId" data))
         (canvas-id (gethash "canvasId" data))
         (session (verify-session session-id)))
    (if session
        (let* ((user-id (second session))
               (username (gethash "username" data))
               (canvas-client (make-instance 'canvas-client
                                            :user-id user-id
                                            :username username
                                            :canvas-id canvas-id)))
          (with-lock-held ((room-clients-lock room))
            (push (cons client canvas-client) (room-clients room)))
          (send-text-message client 
                            (to-json (list :|type| "auth-success"
                                         :|userId| user-id
                                         :|username| username)))
          (broadcast-presence room)
          (format t "Client authenticated: ~A~%" username))
        (send-text-message client 
                          (to-json (list :|type| "auth-failed"))))))

(defun handle-cursor (room client data)
  "Handle cursor position update"
  (let ((canvas-client (find-canvas-client room client)))
    (when canvas-client
      (setf (client-cursor-x canvas-client) (gethash "x" data))
      (setf (client-cursor-y canvas-client) (gethash "y" data))
      (broadcast-to-others room client
                          (to-json (list :|type| "cursor"
                                       :|userId| (client-user-id canvas-client)
                                       :|username| (client-username canvas-client)
                                       :|x| (client-cursor-x canvas-client)
                                       :|y| (client-cursor-y canvas-client)))))))

(defun handle-object-create (room client data)
  "Handle object creation"
  (broadcast-to-all room
                   (to-json (list :|type| "object-create"
                                :|object| (gethash "object" data)))))

(defun handle-object-update (room client data)
  "Handle object update"
  (broadcast-to-all room
                   (to-json (list :|type| "object-update"
                                :|objectId| (gethash "objectId" data)
                                :|updates| (gethash "updates" data)))))

(defun handle-object-delete (room client data)
  "Handle object deletion"
  (broadcast-to-all room
                   (to-json (list :|type| "object-delete"
                                :|objectId| (gethash "objectId" data)))))

(defun handle-ai-command (room client data)
  "Handle AI command - to be implemented"
  (format t "AI command received: ~A~%" (gethash "command" data))
  ;; Will integrate with AI agent module
  )

(defun find-canvas-client (room client)
  "Find canvas client info for WebSocket client"
  (cdr (assoc client (room-clients room))))

(defun broadcast-to-all (room message)
  "Broadcast message to all clients in room"
  (with-lock-held ((room-clients-lock room))
    (dolist (client-pair (room-clients room))
      (send-text-message (car client-pair) message))))

(defun broadcast-to-others (room sender message)
  "Broadcast message to all clients except sender"
  (with-lock-held ((room-clients-lock room))
    (dolist (client-pair (room-clients room))
      (unless (eq (car client-pair) sender)
        (send-text-message (car client-pair) message)))))

(defun broadcast-presence (room)
  "Broadcast presence information"
  (with-lock-held ((room-clients-lock room))
    (let ((users (loop for (ws . canvas-client) in (room-clients room)
                      collect (list :|userId| (client-user-id canvas-client)
                                  :|username| (client-username canvas-client)))))
      (broadcast-to-all room
                       (to-json (list :|type| "presence"
                                    :|users| users))))))
```

### 6. Main Server Module

```lisp
;; src/main.lisp
(defpackage #:collabcanvas
  (:use #:cl #:hunchentoot)
  (:import-from #:jonathan
                #:parse
                #:to-json)
  (:import-from #:collabcanvas.config
                #:*port*
                #:*cors-origin*)
  (:import-from #:collabcanvas.database
                #:init-db)
  (:import-from #:collabcanvas.auth
                #:register-user
                #:login-user
                #:logout-user)
  (:import-from #:collabcanvas.websocket
                #:canvas-room
                #:get-or-create-room)
  (:export #:start
           #:stop))

(in-package #:collabcanvas)

(defvar *server* nil)
(defvar *websocket-server* nil)

;; CORS handler
(defun add-cors-headers ()
  (setf (header-out :access-control-allow-origin) *cors-origin*)
  (setf (header-out :access-control-allow-methods) "GET, POST, PUT, DELETE, OPTIONS")
  (setf (header-out :access-control-allow-headers) "Content-Type, Authorization"))

;; HTTP API handlers
(define-easy-handler (api-register :uri "/api/register") (email password username)
  (add-cors-headers)
  (setf (content-type*) "application/json")
  (let* ((body (raw-post-data :force-text t))
         (data (parse body))
         (result (register-user (gethash "email" data)
                               (gethash "password" data)
                               (gethash "username" data))))
    (to-json result)))

(define-easy-handler (api-login :uri "/api/login") ()
  (add-cors-headers)
  (setf (content-type*) "application/json")
  (let* ((body (raw-post-data :force-text t))
         (data (parse body))
         (result (login-user (gethash "email" data)
                            (gethash "password" data))))
    (to-json result)))

(define-easy-handler (api-logout :uri "/api/logout") ()
  (add-cors-headers)
  (setf (content-type*) "application/json")
  (let* ((body (raw-post-data :force-text t))
         (data (parse body))
         (result (logout-user (gethash "sessionId" data))))
    (to-json result)))

;; WebSocket endpoint
(defun websocket-dispatch (request)
  "Dispatch WebSocket connections to appropriate rooms"
  (let* ((uri (request-uri request))
         (canvas-id (second (cl-ppcre:split "/" uri))))
    (when (and canvas-id (string= (first (cl-ppcre:split "/" uri)) "ws"))
      (get-or-create-room canvas-id))))

(defun start ()
  "Start the server"
  (format t "Initializing database...~%")
  (init-db)
  
  (format t "Starting HTTP server on port ~A...~%" *port*)
  (setf *server* (make-instance 'easy-acceptor :port *port*))
  (start *server*)
  
  (format t "Starting WebSocket server...~%")
  (setf *websocket-server* 
        (make-instance 'websocket-acceptor
                      :port *port*
                      :dispatch-table (list #'websocket-dispatch)))
  
  (format t "CollabCanvas server running on http://localhost:~A~%" *port*)
  (format t "WebSocket endpoint: ws://localhost:~A/ws/<canvas-id>~%" *port*))

(defun stop ()
  "Stop the server"
  (when *server*
    (stop *server*)
    (setf *server* nil))
  (when *websocket-server*
    (stop *websocket-server*)
    (setf *websocket-server* nil))
  (format t "Server stopped~%"))
```

---

## Frontend Architecture (PixiJS)

### 1. Package Configuration

```json
{
  "name": "collabcanvas-frontend",
  "version": "0.1.0",
  "type": "module",
  "scripts": {
    "dev": "vite",
    "build": "vite build",
    "preview": "vite preview"
  },
  "dependencies": {
    "pixi.js": "^7.3.0"
  },
  "devDependencies": {
    "vite": "^5.0.0"
  }
}
```

### 2. Vite Configuration

```javascript
// vite.config.js
import { defineConfig } from 'vite';

export default defineConfig({
  server: {
    port: 5173,
    proxy: {
      '/api': {
        target: 'http://localhost:8080',
        changeOrigin: true
      }
    }
  },
  build: {
    outDir: 'dist',
    assetsDir: 'assets',
    sourcemap: true
  }
});
```

### 3. Main Application Entry

```javascript
// src/main.js
import { Application } from 'pixi.js';
import { CanvasManager } from './canvas.js';
import { WebSocketClient } from './websocket.js';
import { AuthManager } from './auth.js';
import './styles.css';

class CollabCanvas {
  constructor() {
    this.auth = new AuthManager();
    this.canvas = null;
    this.ws = null;
    this.canvasId = this.getCanvasId();
    
    this.init();
  }
  
  getCanvasId() {
    // Extract canvas ID from URL or generate new one
    const params = new URLSearchParams(window.location.search);
    return params.get('canvas') || this.generateCanvasId();
  }
  
  generateCanvasId() {
    return 'canvas-' + Math.random().toString(36).substr(2, 9);
  }
  
  async init() {
    // Check if user is authenticated
    if (!this.auth.isAuthenticated()) {
      this.showAuthUI();
      return;
    }
    
    // Initialize canvas
    await this.initCanvas();
    
    // Connect WebSocket
    this.connectWebSocket();
  }
  
  showAuthUI() {
    document.getElementById('auth-modal').style.display = 'block';
    
    document.getElementById('login-form').addEventListener('submit', async (e) => {
      e.preventDefault();
      const email = document.getElementById('login-email').value;
      const password = document.getElementById('login-password').value;
      
      const result = await this.auth.login(email, password);
      if (result.success) {
        document.getElementById('auth-modal').style.display = 'none';
        this.init();
      } else {
        alert('Login failed: ' + result.message);
      }
    });
    
    document.getElementById('register-form').addEventListener('submit', async (e) => {
      e.preventDefault();
      const email = document.getElementById('register-email').value;
      const password = document.getElementById('register-password').value;
      const username = document.getElementById('register-username').value;
      
      const result = await this.auth.register(email, password, username);
      if (result.success) {
        alert('Registration successful! Please login.');
        // Switch to login tab
      } else {
        alert('Registration failed: ' + result.message);
      }
    });
  }
  
  async initCanvas() {
    const app = new Application({
      width: window.innerWidth,
      height: window.innerHeight,
      backgroundColor: 0x1a1a1a,
      resolution: window.devicePixelRatio || 1,
      autoDensity: true,
    });
    
    document.getElementById('canvas-container').appendChild(app.view);
    
    this.canvas = new CanvasManager(app);
    
    // Handle window resize
    window.addEventListener('resize', () => {
      app.renderer.resize(window.innerWidth, window.innerHeight);
    });
  }
  
  connectWebSocket() {
    const wsUrl = `ws://localhost:8080/ws/${this.canvasId}`;
    this.ws = new WebSocketClient(wsUrl, this.canvas, this.auth);
    this.ws.connect();
  }
}

// Start application
window.addEventListener('DOMContentLoaded', () => {
  new CollabCanvas();
});
```

---

## Deployment Configuration

### Dockerfile

```dockerfile
FROM debian:bullseye-slim

# Install dependencies
RUN apt-get update && apt-get install -y \
    curl \
    gcc \
    make \
    libssl-dev \
    sqlite3 \
    && rm -rf /var/lib/apt/lists/*

# Install Roswell
RUN curl -L https://github.com/roswell/roswell/releases/download/v21.10.14.111/roswell_21.10.14.111-1_amd64.deb \
    -o roswell.deb && \
    dpkg -i roswell.deb && \
    rm roswell.deb

# Setup Roswell
RUN ros install sbcl-bin && \
    ros install quicklisp

# Create app directory
WORKDIR /app

# Copy backend code
COPY backend/ /app/

# Install dependencies
RUN ros -e '(ql:quickload :collabcanvas)'

# Build binary
RUN ros build /app/collabcanvas.ros

EXPOSE 8080

CMD ["./collabcanvas"]
```

### Fly.io Configuration

```toml
# fly.toml
app = "collabcanvas"
primary_region = "ord"

[build]
  dockerfile = "Dockerfile"

[env]
  PORT = "8080"

[[services]]
  internal_port = 8080
  protocol = "tcp"

  [[services.ports]]
    handlers = ["http"]
    port = 80

  [[services.ports]]
    handlers = ["tls", "http"]
    port = 443

[[services.http_checks]]
  interval = 10000
  timeout = 2000
  grace_period = "5s"
  method = "get"
  path = "/health"
```

---

## Implementation Roadmap

### Phase 1: MVP (24 hours)
1. ✅ Backend: Basic HTTP + WebSocket server
2. ✅ Database: SQLite with user auth
3. ✅ Frontend: PixiJS canvas with pan/zoom
4. ✅ Real-time: Cursor sync and basic shapes
5. ✅ Deployment: Basic Fly.io setup

### Phase 2: Core Features (Days 2-4)
1. Object transformations (move, resize, rotate)
2. Multi-select and layer management
3. State persistence
4. Conflict resolution
5. Performance optimization

### Phase 3: AI Integration (Days 5-7)
1. AI agent infrastructure
2. Function calling setup
3. Basic commands (create, modify)
4. Complex commands (layouts)
5. Multi-user AI coordination

---

## Performance Targets

- **FPS**: 60 FPS during all interactions
- **Sync Latency**: 
  - Cursors: <50ms
  - Objects: <100ms
- **Scale**:
  - 500+ objects without FPS drop
  - 5+ concurrent users
- **Bundle Size**: <500KB (gzipped)

---

## Development Workflow

1. **Start Backend**:
   ```bash
   cd backend
   ./start.sh
   ```

2. **Start Frontend**:
   ```bash
   cd frontend
   npm run dev
   ```

3. **Test Multiplayer**:
   - Open multiple browser windows
   - Use different user accounts
   - Test concurrent editing

4. **Deploy**:
   ```bash
   fly deploy
   ```

---

## Key Technical Decisions

1. **Why PixiJS?**: Best performance for 2D canvas with WebGL acceleration
2. **Why SQLite?**: Simple, embedded, perfect for this scale
3. **Why Hunchensocket?**: Battle-tested WebSocket for Common Lisp
4. **Last Write Wins**: Simple conflict resolution, document clearly
5. **Session-based Auth**: Simpler than JWT for this use case

---

This architecture provides a solid foundation for building a production-ready collaborative canvas with AI capabilities!
