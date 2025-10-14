# CollabCanvas Quick Start Guide
## Build a Real-Time Collaborative Canvas with Common Lisp + PixiJS

This guide will get you from zero to a working multiplayer canvas in 24 hours.

**🎯 Task Master Integration:** This guide corresponds to 15 main tasks, each with 5 detailed subtasks (75 total). Use `task-master list` to track progress alongside this guide.

---

## Prerequisites

- **Roswell** installed (see ROSWELL_GUIDE.md)
- **Node.js** 18+ and pnpm/npm
- **SQLite** 3+
- **Fly.io CLI** (for deployment)
- **Task Master** (optional) - for structured task tracking

```bash
# Track implementation progress with Task Master
task-master status      # View overall progress
task-master next        # Get next subtask
task-master show 1      # View Task 1 with all subtasks
```

---

## Step 1: Initialize Backend (30 minutes)

### 1.1 Create Project Structure

```bash
mkdir collabcanvas
cd collabcanvas
mkdir -p backend/{src,db,data} frontend/{src,public}
```

### 1.2 Create System Definition

File: `backend/collabcanvas.asd`

```lisp
(asdf:defsystem "collabcanvas"
  :description "Real-time collaborative canvas with AI"
  :depends-on (#:hunchentoot #:hunchensocket #:bordeaux-threads
               #:sqlite #:jonathan #:ironclad #:cl-base64
               #:babel #:uuid #:local-time #:cl-ppcre)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                            (:file "config")
                            (:file "utils")
                            (:file "database")
                            (:file "auth")
                            (:file "canvas-state")
                            (:file "websocket")
                            (:file "main")))))
```

### 1.3 Link and Load

```bash
cd backend
ln -s $(pwd) ~/.roswell/local-projects/collabcanvas
ros -e '(ql:register-local-projects)'
ros -e '(ql:quickload :collabcanvas)'
```

---

## Step 2: Implement Core Backend (2 hours)

### 2.1 Database Schema

File: `backend/db/schema.sql`

```sql
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    username TEXT UNIQUE NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS sessions (
    id TEXT PRIMARY KEY,
    user_id INTEGER NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    expires_at DATETIME NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

CREATE TABLE IF NOT EXISTS canvas_states (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    canvas_id TEXT UNIQUE NOT NULL,
    state TEXT NOT NULL,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_sessions_user ON sessions(user_id);
CREATE INDEX idx_sessions_expires ON sessions(expires_at);
CREATE INDEX idx_canvas_id ON canvas_states(canvas_id);
```

### 2.2 Critical Implementation Pattern

**The WebSocket Handler** (most important part):

```lisp
;; backend/src/websocket.lisp
(defmethod text-message-received ((room canvas-room) client message)
  (handler-case
      (let* ((data (jonathan:parse message :as :hash-table))
             (msg-type (gethash "type" data)))
        (cond
          ((string= msg-type "cursor")
           ;; Broadcast cursor position to all other clients
           (broadcast-to-others room client
             (jonathan:to-json 
               (list :|type| "cursor"
                     :|userId| (get-user-id client)
                     :|x| (gethash "x" data)
                     :|y| (gethash "y" data)))))
          
          ((string= msg-type "object-create")
           ;; Broadcast new object to all clients
           (let ((obj (gethash "object" data)))
             (broadcast-to-all room
               (jonathan:to-json
                 (list :|type| "object-create"
                       :|object| obj)))))
          
          ((string= msg-type "object-update")
           ;; Broadcast object updates
           (broadcast-to-all room
             (jonathan:to-json
               (list :|type| "object-update"
                     :|objectId| (gethash "objectId" data)
                     :|updates| (gethash "updates" data)))))))
    (error (e)
      (log:error "WebSocket error: ~A" e))))
```

### 2.3 Start Script

File: `backend/start.sh`

```bash
#!/bin/bash
echo "Starting CollabCanvas backend..."

# Ensure data directory exists
mkdir -p data

# Initialize database if needed
if [ ! -f data/canvas.db ]; then
    echo "Initializing database..."
    sqlite3 data/canvas.db < db/schema.sql
fi

# Start Lisp server
ros -s collabcanvas \
    -e '(collabcanvas:start)' \
    -e '(format t "Server running on http://localhost:8080~%")' \
    -e '(format t "WebSocket: ws://localhost:8080/ws/~%")' \
    -e '(loop (sleep 1))'
```

```bash
chmod +x start.sh
```

---

## Step 3: Frontend with PixiJS (3 hours)

### 3.1 Initialize Frontend

```bash
cd frontend
npm init -y
npm install pixi.js@^7.3.0 vite@^5.0.0 --save
```

### 3.2 Package.json Scripts

```json
{
  "scripts": {
    "dev": "vite",
    "build": "vite build",
    "preview": "vite preview"
  }
}
```

### 3.3 Core Canvas Manager

File: `frontend/src/canvas.js`

```javascript
import * as PIXI from 'pixi.js';

export class CanvasManager {
  constructor(app) {
    this.app = app;
    this.viewport = new PIXI.Container();
    this.objects = new Map(); // objectId -> PIXI object
    this.selectedObjects = new Set();
    
    // Setup viewport
    this.app.stage.addChild(this.viewport);
    this.setupInteraction();
    this.setupPanZoom();
  }
  
  setupPanZoom() {
    let isDragging = false;
    let dragStart = { x: 0, y: 0 };
    
    this.app.view.addEventListener('mousedown', (e) => {
      if (e.button === 1 || (e.button === 0 && e.altKey)) { // Middle click or Alt+click
        isDragging = true;
        dragStart = { x: e.clientX, y: e.clientY };
      }
    });
    
    this.app.view.addEventListener('mousemove', (e) => {
      if (isDragging) {
        const dx = e.clientX - dragStart.x;
        const dy = e.clientY - dragStart.y;
        this.viewport.x += dx;
        this.viewport.y += dy;
        dragStart = { x: e.clientX, y: e.clientY };
      }
    });
    
    this.app.view.addEventListener('mouseup', () => {
      isDragging = false;
    });
    
    this.app.view.addEventListener('wheel', (e) => {
      e.preventDefault();
      const zoomFactor = e.deltaY > 0 ? 0.9 : 1.1;
      const newScale = this.viewport.scale.x * zoomFactor;
      
      // Limit zoom
      if (newScale >= 0.1 && newScale <= 5) {
        this.viewport.scale.set(newScale);
      }
    });
  }
  
  createRectangle(id, x, y, width, height, color) {
    const rect = new PIXI.Graphics();
    rect.beginFill(color);
    rect.drawRect(0, 0, width, height);
    rect.endFill();
    rect.x = x;
    rect.y = y;
    rect.interactive = true;
    rect.buttonMode = true;
    
    // Make draggable
    this.makeDraggable(rect, id);
    
    this.objects.set(id, rect);
    this.viewport.addChild(rect);
    
    return rect;
  }
  
  createCircle(id, x, y, radius, color) {
    const circle = new PIXI.Graphics();
    circle.beginFill(color);
    circle.drawCircle(0, 0, radius);
    circle.endFill();
    circle.x = x;
    circle.y = y;
    circle.interactive = true;
    circle.buttonMode = true;
    
    this.makeDraggable(circle, id);
    
    this.objects.set(id, circle);
    this.viewport.addChild(circle);
    
    return circle;
  }
  
  makeDraggable(obj, id) {
    let dragData = null;
    
    obj.on('pointerdown', (event) => {
      dragData = event.data;
      obj.alpha = 0.7;
      dragData.dragging = true;
    });
    
    obj.on('pointerup', () => {
      if (dragData) {
        obj.alpha = 1;
        dragData.dragging = false;
        dragData = null;
        
        // Notify WebSocket of final position
        this.onObjectMoved(id, obj.x, obj.y);
      }
    });
    
    obj.on('pointermove', () => {
      if (dragData && dragData.dragging) {
        const newPosition = dragData.getLocalPosition(obj.parent);
        obj.x = newPosition.x;
        obj.y = newPosition.y;
      }
    });
  }
  
  updateObject(id, updates) {
    const obj = this.objects.get(id);
    if (!obj) return;
    
    if (updates.x !== undefined) obj.x = updates.x;
    if (updates.y !== undefined) obj.y = updates.y;
    if (updates.width !== undefined && obj.width !== undefined) {
      obj.width = updates.width;
    }
    if (updates.height !== undefined && obj.height !== undefined) {
      obj.height = updates.height;
    }
  }
  
  deleteObject(id) {
    const obj = this.objects.get(id);
    if (obj) {
      this.viewport.removeChild(obj);
      this.objects.delete(id);
    }
  }
  
  // Callbacks - override these
  onObjectMoved(id, x, y) {
    console.log('Object moved:', id, x, y);
  }
}
```

### 3.4 WebSocket Client

File: `frontend/src/websocket.js`

```javascript
export class WebSocketClient {
  constructor(url, canvasManager, authManager) {
    this.url = url;
    this.canvas = canvasManager;
    this.auth = authManager;
    this.ws = null;
    this.cursors = new Map(); // userId -> cursor PIXI object
    this.reconnectAttempts = 0;
    this.maxReconnectAttempts = 5;
  }
  
  connect() {
    this.ws = new WebSocket(this.url);
    
    this.ws.onopen = () => {
      console.log('WebSocket connected');
      this.reconnectAttempts = 0;
      
      // Authenticate
      this.send({
        type: 'auth',
        sessionId: this.auth.getSessionId(),
        username: this.auth.getUsername(),
        canvasId: this.getCanvasId()
      });
    };
    
    this.ws.onmessage = (event) => {
      const data = JSON.parse(event.data);
      this.handleMessage(data);
    };
    
    this.ws.onclose = () => {
      console.log('WebSocket closed');
      this.attemptReconnect();
    };
    
    this.ws.onerror = (error) => {
      console.error('WebSocket error:', error);
    };
    
    // Send cursor position
    document.addEventListener('mousemove', (e) => {
      if (this.ws && this.ws.readyState === WebSocket.OPEN) {
        // Convert screen coordinates to canvas coordinates
        const canvasPos = this.screenToCanvas(e.clientX, e.clientY);
        this.send({
          type: 'cursor',
          x: canvasPos.x,
          y: canvasPos.y
        });
      }
    });
  }
  
  handleMessage(data) {
    switch (data.type) {
      case 'auth-success':
        console.log('Authenticated as:', data.username);
        break;
        
      case 'cursor':
        this.updateRemoteCursor(data.userId, data.username, data.x, data.y);
        break;
        
      case 'object-create':
        this.createObjectFromData(data.object);
        break;
        
      case 'object-update':
        this.canvas.updateObject(data.objectId, data.updates);
        break;
        
      case 'object-delete':
        this.canvas.deleteObject(data.objectId);
        break;
        
      case 'presence':
        this.updatePresence(data.users);
        break;
    }
  }
  
  updateRemoteCursor(userId, username, x, y) {
    // Create or update cursor for remote user
    // Implementation depends on your cursor visualization strategy
    console.log(`User ${username} cursor at`, x, y);
  }
  
  createObjectFromData(objData) {
    switch (objData.type) {
      case 'rectangle':
        this.canvas.createRectangle(
          objData.id,
          objData.x,
          objData.y,
          objData.width,
          objData.height,
          objData.color
        );
        break;
      case 'circle':
        this.canvas.createCircle(
          objData.id,
          objData.x,
          objData.y,
          objData.radius,
          objData.color
        );
        break;
    }
  }
  
  send(data) {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(data));
    }
  }
  
  screenToCanvas(screenX, screenY) {
    // Convert screen coordinates to canvas viewport coordinates
    const viewport = this.canvas.viewport;
    return {
      x: (screenX - viewport.x) / viewport.scale.x,
      y: (screenY - viewport.y) / viewport.scale.y
    };
  }
  
  attemptReconnect() {
    if (this.reconnectAttempts < this.maxReconnectAttempts) {
      this.reconnectAttempts++;
      console.log(`Reconnecting... attempt ${this.reconnectAttempts}`);
      setTimeout(() => this.connect(), 2000);
    }
  }
  
  getCanvasId() {
    const params = new URLSearchParams(window.location.search);
    return params.get('canvas') || 'default';
  }
}
```

---

## Step 4: Testing Multiplayer (1 hour)

### 4.1 Start Both Servers

Terminal 1:
```bash
cd backend
./start.sh
```

Terminal 2:
```bash
cd frontend
npm run dev
```

### 4.2 Test Scenarios

1. **Open two browser windows** at `http://localhost:5173`
2. **Register/login** with different accounts
3. **Move cursor** in one window - see it in the other
4. **Create objects** - they should appear in both windows
5. **Drag objects** - updates should sync

### 4.3 Performance Check

Open DevTools → Performance:
- **Target:** 60 FPS during pan/zoom
- **Network latency:** <100ms for object updates
- **Memory:** Stable, no leaks

---

## Step 5: Deploy to Fly.io (1 hour)

### 5.1 Create Dockerfile

File: `Dockerfile`

```dockerfile
FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y \
    curl gcc make libssl-dev sqlite3 \
    && rm -rf /var/lib/apt/lists/*

# Install Roswell
RUN curl -L https://github.com/roswell/roswell/releases/download/v21.10.14.111/roswell_21.10.14.111-1_amd64.deb \
    -o roswell.deb && dpkg -i roswell.deb && rm roswell.deb

RUN ros install sbcl-bin && ros install quicklisp

WORKDIR /app
COPY backend/ /app/backend/
COPY frontend/dist/ /app/frontend/

# Build backend
WORKDIR /app/backend
RUN ros -e '(ql:quickload :collabcanvas)'

# Create startup script
RUN echo '#!/bin/bash\ncd /app/backend && ros -s collabcanvas -e "(collabcanvas:start)" -e "(loop (sleep 1))"' > /app/start.sh
RUN chmod +x /app/start.sh

EXPOSE 8080
CMD ["/app/start.sh"]
```

### 5.2 Fly Configuration

File: `fly.toml`

```toml
app = "collabcanvas-yourname"
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
```

### 5.3 Deploy

```bash
# Build frontend
cd frontend
npm run build

# Deploy
cd ..
fly launch
fly deploy
```

---

## Critical Success Factors

### ✅ Must Have for MVP

1. **Two cursors syncing** in real-time
2. **Create rectangle** - appears in both windows
3. **Move object** - updates in both windows
4. **Page refresh** - objects persist
5. **Authentication** - users have names/accounts

### ⚠️ Common Pitfalls

1. **WebSocket port mismatch** - frontend must point to correct backend port
2. **CORS errors** - ensure backend allows frontend origin
3. **JSON parsing** - always validate message format
4. **State out of sync** - implement reconnection properly
5. **Performance** - use requestAnimationFrame for smooth rendering

### 🚀 Performance Tips

1. **Throttle cursor updates** - max 30/second
2. **Batch object updates** - debounce 16ms
3. **Use PIXI containers** - group related objects
4. **Implement object pooling** - reuse graphics objects
5. **Profile regularly** - Chrome DevTools Performance tab

---

## Next Steps After MVP

1. **Multi-select** with shift-click
2. **Undo/redo** with command stack
3. **AI integration** with OpenAI function calling
4. **Export to JSON/PNG**
5. **Collaborative cursor trails**

---

## Debugging Checklist

- [ ] Backend running on port 8080
- [ ] Frontend dev server on port 5173
- [ ] Database file exists at `backend/data/canvas.db`
- [ ] WebSocket connects (check browser console)
- [ ] No CORS errors in console
- [ ] Can create account and login
- [ ] Objects appear in PixiJS viewport
- [ ] Multiple browser windows show same state

---

## Resources

- **PixiJS Docs:** https://pixijs.download/release/docs/index.html
- **Hunchensocket Example:** https://github.com/joaotavora/hunchensocket
- **Fly.io Docs:** https://fly.io/docs/
- **Common Lisp Cookbook:** https://lispcookbook.github.io/cl-cookbook/

---

**Remember:** Start simple, test continuously, and iterate. A working 2-user cursor sync is infinitely more valuable than a broken 10-feature canvas.

Good luck building! 🚀
