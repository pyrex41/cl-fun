# Product Requirements Document: CollabCanvas
## Real-Time Collaborative Design Tool with AI

**Version:** 1.0
**Last Updated:** October 2025
**Project Type:** Real-time Collaborative Canvas Application

---

## Executive Summary

CollabCanvas is a Figma-like collaborative design tool built with Common Lisp (backend) and PixiJS (frontend). Multiple designers can work together in real-time on a shared canvas, with live cursor tracking, object manipulation, and WebSocket-based synchronization. The system is designed to support an AI agent that can create and manipulate designs through natural language commands.

### Key Differentiators
- Built with Common Lisp for backend (unique approach)
- 60 FPS performance target for all interactions
- WebSocket-based real-time collaboration
- AI agent integration for natural language design commands
- Embedded SQLite for simplicity and portability

---

## Product Vision

### Problem Statement
Existing collaborative design tools are complex, proprietary, and don't leverage modern AI capabilities. Developers need a simple, open-source alternative that demonstrates real-time collaboration patterns and can be extended with AI features.

### Target Users
- **Primary:** Developers learning real-time collaborative systems
- **Secondary:** Small teams needing a lightweight collaborative canvas
- **Future:** Designers wanting AI-assisted design workflows

### Success Criteria
- 2+ users can edit simultaneously with <100ms sync latency
- Canvas maintains 60 FPS during pan/zoom/interactions
- State persists across sessions
- AI agent can execute basic design commands
- System is deployed and publicly accessible

---

## Technical Stack

### Backend
- **Runtime:** SBCL Common Lisp via Roswell
- **HTTP Server:** Hunchentoot
- **WebSocket:** Hunchensocket
- **Database:** SQLite with cl-sqlite
- **JSON:** Jonathan
- **Authentication:** Ironclad (password hashing)
- **Concurrency:** Bordeaux-threads

### Frontend
- **Rendering:** PixiJS v7 (WebGL 2D rendering)
- **Language:** Vanilla JavaScript (ES6+)
- **Build Tool:** Vite
- **WebSocket:** Native browser WebSocket API

### Infrastructure
- **Deployment:** Fly.io
- **Database:** Embedded SQLite
- **Container:** Docker with Debian base

---

## Core Features & Requirements

### 1. Authentication System

**Priority:** P0 (Must Have)

**User Stories:**
- As a user, I can register with email/password
- As a user, I can login and receive a session token
- As a user, my session persists across page refreshes
- As a user, I can logout to end my session

**Technical Requirements:**
- Password hashing using SHA-256 via Ironclad
- Session-based authentication (not JWT)
- Session timeout: 24 hours (configurable)
- Session storage in SQLite

**API Endpoints:**
```
POST /api/register
Body: {email, password, username}
Response: {success, message}

POST /api/login
Body: {email, password}
Response: {success, sessionId, userId, username}

POST /api/logout
Body: {sessionId}
Response: {success}
```

**Database Schema:**
```sql
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    username TEXT UNIQUE NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE sessions (
    id TEXT PRIMARY KEY,
    user_id INTEGER NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    expires_at DATETIME NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(id)
);
```

**Acceptance Criteria:**
- [ ] Can register new user successfully
- [ ] Cannot register duplicate email
- [ ] Can login with valid credentials
- [ ] Login fails with invalid credentials
- [ ] Session persists across page refreshes
- [ ] Session expires after 24 hours
- [ ] Logout invalidates session

---

### 2. Canvas System

**Priority:** P0 (Must Have)

**User Stories:**
- As a user, I can pan the canvas by middle-click dragging or Alt+dragging
- As a user, I can zoom in/out with mouse wheel
- As a user, the canvas maintains 60 FPS during all interactions
- As a user, I can see a grid for spatial orientation

**Technical Requirements:**
- Infinite canvas implemented with PixiJS Container
- Pan: Middle-click or Alt+left-click drag
- Zoom: Mouse wheel (0.1x to 10x range)
- Zoom centers on mouse cursor position
- Grid rendering at 50px intervals
- Viewport tracking for coordinate conversion

**Coordinate Systems:**
- **Screen coordinates:** Browser viewport pixels (e.g., mouse position)
- **World coordinates:** Canvas space coordinates (objects stored here)
- Conversion functions: `screenToWorld()` and `worldToScreen()`

**Performance Targets:**
- 60 FPS during pan operations
- 60 FPS during zoom operations
- No frame drops with 500+ objects on canvas
- Smooth rendering at 0.1x to 10x zoom levels

**Acceptance Criteria:**
- [ ] Middle-click drag pans canvas smoothly
- [ ] Alt+drag pans canvas smoothly
- [ ] Mouse wheel zooms in/out
- [ ] Zoom centers on cursor position
- [ ] Canvas maintains 60 FPS during interaction
- [ ] Grid visible and scales with zoom
- [ ] Canvas accessible from -5000 to +5000 in both axes

---

### 3. Object Creation & Manipulation

**Priority:** P0 (Must Have)

**User Stories:**
- As a user, I can create rectangles on the canvas
- As a user, I can create circles on the canvas
- As a user, I can drag objects to reposition them
- As a user, I can select objects by clicking
- As a user, I can multi-select objects with Shift+click
- As a user, I can delete selected objects with Delete/Backspace
- As a user, I can switch tools with keyboard shortcuts

**Supported Shapes:**
1. **Rectangle**
   - Properties: x, y, width, height, color
   - Creation: Click and drag
   - Keyboard shortcut: 'R'

2. **Circle**
   - Properties: x, y, radius, color
   - Creation: Click and drag (radius from start point)
   - Keyboard shortcut: 'C'

3. **Text** (Phase 2)
   - Properties: x, y, text, fontSize, color
   - Creation: Click to place
   - Keyboard shortcut: 'T'

**Interaction Modes:**
- **Select Mode:** 'V' or Escape - Select and drag objects
- **Rectangle Mode:** 'R' - Draw rectangles
- **Circle Mode:** 'C' - Draw circles
- **Text Mode:** 'T' - Add text (Phase 2)

**Object Properties:**
```javascript
{
  id: 'obj-abc123',
  type: 'rectangle' | 'circle' | 'text',
  x: number,
  y: number,
  // Rectangle
  width?: number,
  height?: number,
  // Circle
  radius?: number,
  // Text
  text?: string,
  fontSize?: number,
  // Common
  color: number (hex)
}
```

**Acceptance Criteria:**
- [ ] Can create rectangle with click-and-drag
- [ ] Can create circle with click-and-drag
- [ ] Can drag objects to new positions
- [ ] Can select single object by clicking
- [ ] Can multi-select with Shift+click
- [ ] Can delete selected objects with Delete key
- [ ] Keyboard shortcuts switch tools correctly
- [ ] Object IDs are unique and persistent

---

### 4. Real-Time Synchronization

**Priority:** P0 (Must Have)

**User Stories:**
- As a user, when I create an object, all connected users see it immediately
- As a user, when I move an object, all connected users see the update
- As a user, when I delete an object, it disappears for all users
- As a user, I see other users' cursors with their names
- As a user, cursor positions update in real-time (<50ms latency)

**WebSocket Protocol:**

**Connection:** `ws://host:port/ws/{canvas-id}`

**Client → Server Messages:**
```javascript
// Authenticate connection
{type: 'auth', sessionId: string, username: string, canvasId: string}

// Cursor position update (throttled to 30/sec)
{type: 'cursor', x: number, y: number}

// Create object
{type: 'object-create', object: {id, type, x, y, ...}}

// Update object
{type: 'object-update', objectId: string, updates: {x?, y?, ...}}

// Delete object
{type: 'object-delete', objectId: string}

// AI command (Phase 3)
{type: 'ai-command', command: string}
```

**Server → Client Messages:**
```javascript
// Authentication result
{type: 'auth-success', userId: number, username: string}
{type: 'auth-failed'}

// Remote cursor position
{type: 'cursor', userId: number, username: string, x: number, y: number}

// Object operations (broadcast to all)
{type: 'object-create', object: {...}}
{type: 'object-update', objectId: string, updates: {...}}
{type: 'object-delete', objectId: string}

// Presence information
{type: 'presence', users: [{userId, username}, ...]}
```

**WebSocket Room Management:**
- Each canvas has a unique canvas-id
- Server maintains hash table: canvas-id → room
- Rooms track connected clients
- Messages broadcast to all clients in room
- Clients removed from room on disconnect

**Conflict Resolution Strategy:**
- **Last-write-wins** for object properties
- No optimistic updates (server is source of truth)
- Client changes immediately broadcast
- Simple and predictable for MVP

**Performance Requirements:**
- Cursor sync latency: <50ms
- Object sync latency: <100ms
- Support 5+ concurrent users per canvas
- No message loss during normal operation
- Graceful handling of network interruptions

**Acceptance Criteria:**
- [ ] WebSocket connection establishes on canvas load
- [ ] Authentication message sent and confirmed
- [ ] Object creation broadcasts to all users
- [ ] Object updates broadcast to all users
- [ ] Object deletion broadcasts to all users
- [ ] Cursor positions sync with <50ms latency
- [ ] Presence list updates on connect/disconnect
- [ ] Reconnection works after network interruption
- [ ] No sync issues with 5+ concurrent users

---

### 5. State Persistence

**Priority:** P0 (Must Have)

**User Stories:**
- As a user, when I refresh the page, my canvas state is preserved
- As a user, when all users leave, the canvas state is saved
- As a user, when I rejoin a canvas, I see the current state

**Database Schema:**
```sql
CREATE TABLE canvas_states (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    canvas_id TEXT UNIQUE NOT NULL,
    state TEXT NOT NULL,  -- JSON blob
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

**State Format:**
```javascript
{
  objects: [
    {id: 'obj-1', type: 'rectangle', x: 100, y: 100, width: 200, height: 100, color: 0xFF0000},
    {id: 'obj-2', type: 'circle', x: 300, y: 200, radius: 50, color: 0x00FF00}
  ],
  version: number  // For future versioning
}
```

**Save Strategy:**
- Debounce saves (500ms after last change)
- Save on object create/update/delete
- Use INSERT OR REPLACE for upsert
- Store as JSON blob for simplicity

**Load Strategy:**
- Load on canvas connection (after auth)
- Send initial state to new clients
- Merge with any existing objects

**API Endpoint:**
```
GET /api/canvas/state?canvasId={canvas-id}
Response: {objects: [...]}
```

**Acceptance Criteria:**
- [ ] Canvas state saves to database on changes
- [ ] State persists across page refreshes
- [ ] State persists when all users disconnect
- [ ] New users see existing canvas state
- [ ] State saves are debounced (not on every change)

---

### 6. Presence Awareness

**Priority:** P0 (Must Have)

**User Stories:**
- As a user, I can see who else is online
- As a user, I see other users' cursors with their names
- As a user, when someone joins, I'm notified
- As a user, when someone leaves, their cursor disappears

**UI Components:**
- Online user list (top-right corner)
- Remote cursor visualization (colored pointer + name label)
- User count indicator

**Remote Cursor Design:**
- Triangular pointer (12x24px)
- Unique color per user (hash username to color)
- Name label next to cursor
- Position in world coordinates
- Z-index above all objects

**Presence Updates:**
- Send on user connect
- Send on user disconnect
- Include userId and username
- Broadcast to all users in room

**Acceptance Criteria:**
- [ ] Can see list of online users
- [ ] Can see remote user cursors
- [ ] Cursor shows username label
- [ ] Cursor updates smoothly (<50ms)
- [ ] Cursor disappears when user disconnects
- [ ] User count is accurate

---

### 7. User Interface

**Priority:** P0 (Must Have)

**Components:**

1. **Authentication Modal**
   - Login/Register tabs
   - Email, password, username fields
   - Error message display
   - Modal overlay (blocks canvas access)

2. **Canvas Container**
   - Full viewport coverage
   - PixiJS canvas element
   - Transparent to allow overlays

3. **Toolbar** (Phase 2)
   - Tool buttons (Select, Rectangle, Circle, Text)
   - Active tool indicator
   - Color picker
   - Position: Left side, vertical

4. **User Presence**
   - Online user list
   - User count badge
   - Position: Top-right corner

5. **Status Bar** (Phase 2)
   - Current tool
   - Cursor position
   - Zoom level
   - Position: Bottom

**Styling:**
- Dark theme (background: #1a1a1a)
- Accent color: #3498db (blue)
- Grid color: #333333 (30% opacity)
- Clean, minimal design
- Responsive layout

**Acceptance Criteria:**
- [ ] Auth modal blocks canvas until logged in
- [ ] Canvas fills viewport
- [ ] UI is usable and intuitive
- [ ] Dark theme is consistent
- [ ] Components don't obstruct canvas

---

### 8. AI Agent Integration (Phase 3)

**Priority:** P2 (Nice to Have)

**User Stories:**
- As a user, I can type natural language commands
- As a user, the AI creates objects based on my commands
- As a user, the AI can modify existing objects
- As a user, the AI can create complex layouts

**Example Commands:**
- "Create a blue rectangle at position 200, 300"
- "Make the circle twice as big"
- "Create a login form with username and password fields"
- "Arrange all rectangles in a grid"

**Implementation Approach:**
1. OpenAI function calling API
2. Define canvas operations as functions
3. Parse natural language to function calls
4. Execute operations and broadcast

**AI Agent Functions:**
```javascript
createRectangle(x, y, width, height, color)
createCircle(x, y, radius, color)
createText(x, y, text, fontSize, color)
moveObject(objectId, x, y)
resizeObject(objectId, width, height)
deleteObject(objectId)
arrangeGrid(objectIds, columns, spacing)
alignObjects(objectIds, alignment)
```

**Backend Module:** `src/ai-agent.lisp`
```lisp
(defun process-ai-command (command canvas-state)
  "Process natural language command and return operations"
  ;; Call OpenAI API with function definitions
  ;; Parse response
  ;; Return list of operations
  )
```

**Acceptance Criteria (Phase 3):**
- [ ] Can send natural language command
- [ ] AI creates simple shapes from commands
- [ ] AI can modify existing objects
- [ ] AI can create multi-object layouts
- [ ] Commands execute with <2 second latency

---

## Architecture

### Backend Architecture

**Module Structure:**
```
backend/
├── collabcanvas.asd           # ASDF system definition
├── src/
│   ├── package.lisp           # Package definitions
│   ├── config.lisp            # Configuration parameters
│   ├── utils.lisp             # Utility functions
│   ├── database.lisp          # SQLite operations
│   ├── auth.lisp              # Authentication logic
│   ├── websocket.lisp         # WebSocket handlers
│   ├── canvas-state.lisp      # State management
│   ├── ai-agent.lisp          # AI integration (Phase 3)
│   └── main.lisp              # Server entry point
├── db/
│   └── schema.sql             # Database schema
└── data/                      # Runtime data (git-ignored)
    └── canvas.db              # SQLite database
```

**Key Patterns:**

1. **WebSocket Message Routing:**
```lisp
(defmethod text-message-received ((room canvas-room) client message)
  (let* ((data (jonathan:parse message :as :hash-table))
         (msg-type (gethash "type" data)))
    (cond
      ((string= msg-type "cursor") (handle-cursor room client data))
      ((string= msg-type "object-create") (handle-object-create room client data))
      ...)))
```

2. **Broadcasting:**
```lisp
(defun broadcast-to-all (room message)
  (with-lock-held ((room-clients-lock room))
    (dolist (client-pair (room-clients room))
      (send-text-message (car client-pair) message))))
```

3. **Room Management:**
```lisp
(defun get-or-create-room (canvas-id)
  (with-lock-held (*rooms-lock*)
    (or (gethash canvas-id *rooms*)
        (setf (gethash canvas-id *rooms*)
              (make-instance 'canvas-room :canvas-id canvas-id)))))
```

**Concurrency Model:**
- One WebSocket acceptor handles all connections
- Each canvas has a room (thread-safe)
- Rooms maintain list of connected clients
- Message broadcasting uses locks
- No complex locking - keep it simple

---

### Frontend Architecture

**Module Structure:**
```
frontend/
├── package.json
├── vite.config.js
├── index.html
└── src/
    ├── main.js                # Entry point
    ├── canvas.js              # PixiJS canvas logic
    ├── websocket.js           # WebSocket client
    ├── auth.js                # Auth UI and logic
    └── styles.css
```

**Key Components:**

1. **CanvasManager** (canvas.js)
   - Manages PixiJS Application
   - Handles pan/zoom
   - Creates and tracks objects
   - Implements drag-and-drop
   - Exposes callbacks for sync

2. **WebSocketClient** (websocket.js)
   - Manages WebSocket connection
   - Sends/receives messages
   - Handles reconnection
   - Throttles cursor updates
   - Coordinates with CanvasManager

3. **AuthManager** (auth.js)
   - Login/register UI
   - Session storage
   - API calls
   - Modal management

**Data Flow:**

```
User Action
    ↓
CanvasManager (local update)
    ↓
Callback (e.g., onObjectCreated)
    ↓
WebSocketClient.send()
    ↓
Server broadcasts
    ↓
WebSocketClient.onMessage()
    ↓
CanvasManager (remote update)
```

**State Management:**
- CanvasManager: Map<id, PIXI.Graphics>
- WebSocketClient: Map<userId, cursor>
- No global state - component-based
- Callbacks for coordination

---

### Communication Protocol

**WebSocket Lifecycle:**

1. **Connection:**
   - Client: `new WebSocket('ws://host/ws/canvas-123')`
   - Server: Accept connection, add to room
   - Client: Send auth message
   - Server: Verify session, send auth-success
   - Server: Send presence update to all
   - Server: Send initial canvas state to new client

2. **Operation:**
   - Client: Send cursor updates (throttled to 30/sec)
   - Client: Send object operations (create/update/delete)
   - Server: Broadcast to all clients in room
   - Clients: Update local state

3. **Disconnection:**
   - Client: Close connection or network error
   - Server: Remove from room
   - Server: Broadcast presence update

**Message Format:**
- All messages are JSON
- Always include `type` field
- Server echoes object operations to all clients
- Cursors only broadcast to others (not sender)

---

### Database Design

**Schema:**
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

**Design Decisions:**
- SQLite for simplicity (no separate DB server)
- Session-based auth (not JWT)
- Canvas state as JSON blob (flexible schema)
- No object-relational mapping (direct SQL)

---

## Implementation Plan

### Phase 1: MVP (24 Hours)

**Hours 0-2: Foundation**
- Set up Roswell and project structure
- Create ASDF system definition
- Initialize database with schema
- Test REPL loads system

**Hours 2-4: Authentication**
- Implement password hashing
- Create user registration
- Create login with session management
- Add HTTP API endpoints
- Test with curl

**Hours 4-8: WebSocket Foundation**
- Create canvas-room class
- Implement message handlers
- Add room management
- Set up WebSocket dispatcher
- Test connection from browser console

**Hours 8-12: Frontend Core**
- Initialize PixiJS application
- Implement pan and zoom
- Create shape drawing (rectangle, circle)
- Add drag-and-drop
- Test locally

**Hours 12-16: Real-Time Sync**
- Implement WebSocket client
- Connect to server
- Sync object creation
- Sync object updates
- Test with two browser windows

**Hours 16-20: Polish & Persistence**
- Save canvas state to database
- Load state on connection
- Implement presence awareness
- Add remote cursor visualization
- Polish UI

**Hours 20-24: Testing & Deployment**
- Integration testing (2-3 users)
- Performance testing (60 FPS check)
- Build frontend
- Create Dockerfile
- Deploy to Fly.io
- Write documentation

### Phase 2: Enhanced Canvas (Days 2-4)

**Features:**
- Multi-select with shift-click
- Layers and z-index management
- Text tool with editing
- Lines and paths
- Undo/redo with command pattern
- Export to PNG/JSON
- Keyboard shortcuts

### Phase 3: AI Integration (Days 5-7)

**Features:**
- OpenAI API integration
- Function calling setup
- Basic commands (create, move, resize)
- Layout commands (grid, align, distribute)
- Complex commands (create forms, components)
- Natural language query

---

## Performance Requirements

### Target Metrics

**Rendering Performance:**
- 60 FPS during pan operations
- 60 FPS during zoom operations
- 60 FPS during object manipulation
- No frame drops with 500+ objects
- Smooth animation at all zoom levels

**Network Performance:**
- Cursor sync latency: <50ms
- Object sync latency: <100ms
- WebSocket message throughput: 100+ messages/sec
- Reconnection time: <2 seconds

**Scalability:**
- 5+ concurrent users per canvas
- 500+ objects per canvas
- 10+ simultaneous canvas rooms
- Memory stable over 1 hour session

**Bundle Size:**
- Frontend bundle: <500KB (gzipped)
- Initial load time: <2 seconds
- Time to interactive: <3 seconds

### Optimization Strategies

**Frontend:**
- Throttle cursor updates to 30/sec
- Debounce object position updates
- Use requestAnimationFrame for rendering
- Implement object pooling for graphics
- Use PIXI.Container for grouping
- Minimize draw calls

**Backend:**
- Efficient JSON parsing with Jonathan
- Lock-free read operations where possible
- Debounce database saves (500ms)
- WebSocket message batching
- Efficient room management

**Network:**
- Compress WebSocket messages (future)
- Delta updates instead of full state (future)
- Binary protocol for efficiency (future)

---

## Testing Strategy

### Unit Testing

**Backend (Common Lisp):**
- Test authentication functions
- Test database operations
- Test message parsing
- Test broadcast logic

**Frontend (JavaScript):**
- Test coordinate conversion
- Test object creation
- Test state management
- Test WebSocket client

### Integration Testing

**Scenarios:**
1. Two users create objects simultaneously
2. User refreshes page during editing
3. User disconnects and reconnects
4. Rapid object creation (stress test)
5. Network throttling (3G simulation)
6. 5+ concurrent users

**Test Checklist:**
- [ ] Auth flow works end-to-end
- [ ] Objects sync between users
- [ ] Cursors sync in real-time
- [ ] State persists across refreshes
- [ ] Presence updates correctly
- [ ] Performance meets targets
- [ ] No memory leaks
- [ ] Graceful error handling

### Performance Testing

**Tools:**
- Chrome DevTools (Performance, Memory, Network tabs)
- Lighthouse for bundle analysis
- Manual FPS monitoring

**Tests:**
- Create 100+ objects, verify 60 FPS
- Pan/zoom for 1 minute, check for frame drops
- Monitor memory over 30 minutes
- Test with 5+ concurrent users
- Test with network throttling

---

## Deployment

### Build Process

**Frontend:**
```bash
cd frontend
npm run build
# Output: frontend/dist/
```

**Backend:**
```bash
cd backend
ros build collabcanvas.ros
# Output: collabcanvas binary
```

### Docker Configuration

**Dockerfile:**
```dockerfile
FROM debian:bullseye-slim

# Install dependencies
RUN apt-get update && apt-get install -y \
    curl gcc make libssl-dev sqlite3 \
    && rm -rf /var/lib/apt/lists/*

# Install Roswell
RUN curl -L https://github.com/roswell/roswell/releases/download/v21.10.14.111/roswell_21.10.14.111-1_amd64.deb \
    -o roswell.deb && dpkg -i roswell.deb && rm roswell.deb

# Setup Roswell
RUN ros install sbcl-bin && ros install quicklisp

# Copy application
WORKDIR /app
COPY backend/ /app/backend/
COPY frontend/dist/ /app/frontend/

# Install dependencies
WORKDIR /app/backend
RUN ros -e '(ql:quickload :collabcanvas)'

EXPOSE 8080
CMD ["ros", "-s", "collabcanvas", "-e", "(collabcanvas:start)", "-e", "(loop (sleep 1))"]
```

### Fly.io Configuration

**fly.toml:**
```toml
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

### Deployment Commands

```bash
# Install Fly CLI
curl -L https://fly.io/install.sh | sh

# Login
fly auth login

# Launch (first time)
fly launch

# Deploy updates
fly deploy

# View logs
fly logs

# Check status
fly status

# Open app
fly open
```

---

## Development Workflow

### Daily Development

**Backend Development:**
```bash
# Start REPL
ros run

# In REPL
(ql:quickload :collabcanvas)
(collabcanvas:start)

# Make code changes in editor

# Reload specific file
(load "src/websocket.lisp")

# Or reload entire system
(asdf:load-system :collabcanvas :force t)

# Stop server
(collabcanvas:stop)
```

**Frontend Development:**
```bash
cd frontend
npm run dev
# Vite hot-reloads automatically
```

**Testing Multiplayer:**
1. Start backend: `cd backend && ./start.sh`
2. Start frontend: `cd frontend && npm run dev`
3. Open http://localhost:5173 in two windows
4. Register different accounts
5. Test collaboration features

### Debugging

**Backend Debugging:**
- Use REPL for interactive debugging
- Add logging with `(format t "...")`
- Check SQLite database directly
- Monitor WebSocket connections

**Frontend Debugging:**
- Browser DevTools Console
- Network tab for WebSocket messages
- Performance tab for FPS monitoring
- React DevTools (if using React later)

**Common Issues:**
1. WebSocket won't connect → Check backend is running and port matches
2. Objects not syncing → Check message format and WebSocket status
3. Performance issues → Profile with DevTools, reduce cursor update frequency
4. State not persisting → Check database file exists and is writable

---

## Success Criteria

### MVP Acceptance Criteria

**Must Have All:**

1. **Authentication**
   - ✅ Can register with email/password
   - ✅ Can login and get session
   - ✅ Session persists across refreshes

2. **Canvas**
   - ✅ Infinite pan with middle-click or Alt+drag
   - ✅ Smooth zoom with mouse wheel
   - ✅ Can create at least one shape type
   - ✅ Can move shapes by dragging

3. **Real-Time Sync**
   - ✅ Two users can connect simultaneously
   - ✅ Cursor positions sync in real-time
   - ✅ Object creation syncs immediately
   - ✅ Object updates sync immediately
   - ✅ All users see same state

4. **Persistence**
   - ✅ Canvas state saves to database
   - ✅ Refresh page - objects remain
   - ✅ All users leave and rejoin - work is saved

5. **Presence**
   - ✅ Show who's online
   - ✅ Multiplayer cursors with names
   - ✅ Update presence on connect/disconnect

6. **Deployment**
   - ✅ Publicly accessible URL
   - ✅ Multiple users can connect
   - ✅ Works in different browsers

7. **Performance**
   - ✅ 60 FPS during all interactions
   - ✅ Object sync <100ms latency
   - ✅ Cursor sync <50ms latency
   - ✅ No memory leaks over 5 minutes

### Post-MVP Success Metrics

**Phase 2:**
- Multi-select works reliably
- Undo/redo functional
- Export to PNG/JSON works
- Text tool fully functional

**Phase 3:**
- AI creates simple objects from commands
- AI can modify existing objects
- AI can create multi-object layouts
- Commands execute with <2 second latency

**Production Readiness:**
- 99% uptime over 30 days
- Support 20+ concurrent users
- <500ms initial load time
- Zero data loss incidents

---

## Risk Assessment

### Technical Risks

**Risk: WebSocket connection instability**
- Mitigation: Implement robust reconnection logic
- Fallback: Consider long-polling as backup

**Risk: State conflicts with concurrent editing**
- Mitigation: Last-write-wins strategy
- Future: Implement CRDT for true conflict resolution

**Risk: Performance degradation with many objects**
- Mitigation: Object culling, canvas virtualization
- Test early with 500+ objects

**Risk: Common Lisp deployment complexity**
- Mitigation: Docker containerization
- Documentation of Roswell setup

### Project Risks

**Risk: 24-hour timeline too aggressive**
- Mitigation: Clear MVP scope, defer nice-to-haves
- Focus on core collaboration features first

**Risk: AI integration complexity**
- Mitigation: Phase 3 feature, not required for MVP
- Use existing OpenAI SDKs

**Risk: Browser compatibility issues**
- Mitigation: Test in Chrome, Firefox, Safari
- Use standard WebSocket API

---

## Documentation Requirements

### Code Documentation

**Backend:**
- Docstrings for all exported functions
- Comments for complex algorithms
- API endpoint documentation
- WebSocket protocol specification

**Frontend:**
- JSDoc for public methods
- Comments for coordinate conversions
- Component interaction diagrams

### User Documentation

- README with setup instructions
- Quick start guide
- API reference
- WebSocket message format
- Troubleshooting guide

### Architecture Documentation

- System architecture diagram
- Data flow diagrams
- Database schema documentation
- Deployment guide

---

## Future Enhancements

### Phase 4: Advanced Features

**Collaboration:**
- Comments and annotations
- Version history
- Branching (like Git for designs)
- Real-time chat

**Canvas Features:**
- Components and symbols
- Auto-layout
- Constraints and responsive design
- Animation timeline

**AI Features:**
- Style transfer
- Design suggestions
- Accessibility checking
- Code generation from designs

**Developer Features:**
- Plugin system
- Custom shapes
- Scripting API
- Export to code (HTML/CSS)

### Scalability Improvements

**Performance:**
- Canvas virtualization (only render visible)
- Object pooling
- Web Workers for computation
- WebGL shaders for effects

**Backend:**
- Redis for session storage
- PostgreSQL for production
- Message queue for async operations
- Horizontal scaling with load balancer

**Protocol:**
- Binary WebSocket protocol
- Delta compression
- CRDT for conflict-free replication

---

## Appendix

### Glossary

- **Canvas:** The infinite 2D workspace where objects are placed
- **Object:** A visual element (rectangle, circle, text, etc.)
- **Viewport:** The visible portion of the canvas
- **World Coordinates:** Object positions in canvas space
- **Screen Coordinates:** Pixel positions in browser viewport
- **Room:** WebSocket group for a specific canvas
- **Session:** Authentication token for a user
- **Sync:** Broadcasting changes to all connected users

### References

- [PixiJS Documentation](https://pixijs.download/release/docs/index.html)
- [Hunchensocket Guide](https://github.com/joaotavora/hunchensocket)
- [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/)
- [Fly.io Documentation](https://fly.io/docs/)
- [WebSocket Protocol RFC 6455](https://tools.ietf.org/html/rfc6455)

### Contact & Support

**Project Repository:** [GitHub URL]
**Documentation:** [Docs URL]
**Demo:** [Live Demo URL]

---

## Revision History

| Version | Date | Changes | Author |
|---------|------|---------|--------|
| 1.0 | Oct 2025 | Initial PRD | Team |

---

**End of Product Requirements Document**
