# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Status

**Current Phase:** Planning & Documentation
**Implementation Status:** Not started - directories need to be created

This repository contains comprehensive planning documents and reference implementations for CollabCanvas, a real-time collaborative design tool. The actual backend/ and frontend/ directories have not been created yet.

## Project Overview

CollabCanvas is a real-time collaborative design tool (Figma-like) built with Common Lisp (backend) and PixiJS (frontend). Multiple users can simultaneously edit a canvas, with live cursor tracking, object manipulation, and WebSocket-based synchronization.

## Technology Stack

**Backend:**
- Common Lisp (SBCL via Roswell)
- Hunchentoot (HTTP server) + Hunchensocket (WebSocket)
- SQLite (persistence)
- Jonathan (JSON)
- Ironclad (password hashing)

**Frontend:**
- PixiJS v7 (2D WebGL rendering)
- Vanilla JavaScript (ES6+)
- Vite (dev server/build)

## Available Resources

### Planning Documents (Root Directory)
- **README.md** - Complete project overview with architecture diagram, quick start, API reference
- **COLLABCANVAS_ARCHITECTURE.md** - Full technical architecture with all module implementations
- **QUICKSTART_GUIDE.md** - Step-by-step 24-hour implementation guide
- **MVP_CHECKLIST.md** - Hour-by-hour sprint plan with acceptance criteria
- **PROJECT_SUMMARY.md** - Implementation package contents and learning path
- **AGENTS.md** - Task Master AI integration guide

### Reference Implementations (Root Directory)
- **backend-main.lisp** (7KB) - Complete working main.lisp with HTTP API, WebSocket dispatch, CORS
- **frontend-canvas.js** (14KB) - Complete CanvasManager with pan/zoom, shapes, drag-and-drop

### Task Master Integration
- **.taskmaster/docs/prd.md** - Comprehensive Product Requirements Document
- **.taskmaster/tasks/tasks.json** - 15 structured tasks with dependencies (parsed from PRD)
- **.taskmaster/CLAUDE.md** - Task Master workflow and command reference

## Getting Started with Implementation

### Step 1: Create Project Structure

```bash
# Create backend directory structure
mkdir -p backend/{src,db,data}

# Create frontend directory structure
mkdir -p frontend/{src,public}
```

### Step 2: Set Up Backend (Common Lisp)

**Reference:** See COLLABCANVAS_ARCHITECTURE.md sections 1-6 for complete code

```bash
cd backend

# Copy ASDF system definition from ARCHITECTURE.md to:
# backend/collabcanvas.asd

# Copy module files from ARCHITECTURE.md to:
# backend/src/package.lisp
# backend/src/config.lisp
# backend/src/utils.lisp
# backend/src/database.lisp
# backend/src/auth.lisp
# backend/src/websocket.lisp
# backend/src/canvas-state.lisp

# Copy backend-main.lisp to:
# backend/src/main.lisp

# Copy database schema from ARCHITECTURE.md to:
# backend/db/schema.sql

# Link to Roswell
ln -s $(pwd) ~/.roswell/local-projects/collabcanvas
ros -e '(ql:register-local-projects)'
ros -e '(ql:quickload :collabcanvas)'
```

### Step 3: Set Up Frontend (JavaScript/PixiJS)

**Reference:** See COLLABCANVAS_ARCHITECTURE.md sections 7-8 and QUICKSTART_GUIDE.md

```bash
cd frontend

# Initialize npm project
npm init -y
npm install pixi.js@^7.3.0 vite@^5.0.0

# Copy package.json scripts from ARCHITECTURE.md
# Copy vite.config.js from ARCHITECTURE.md

# Copy frontend-canvas.js to:
# frontend/src/canvas.js

# Implement remaining modules based on ARCHITECTURE.md:
# frontend/src/main.js
# frontend/src/websocket.js
# frontend/src/auth.js
# frontend/index.html
```

### Step 4: Use Task Master for Implementation

**Note:** All 15 tasks have been expanded with 5 detailed subtasks each (75 total subtasks).

```bash
# View all tasks with subtasks
task-master list              # Shows all tasks and subtasks

# Start with first subtask
task-master next              # Get next task (should be Task 1.1)
task-master show 1            # View main task with all subtasks
task-master show 1.1          # View specific subtask details

# Work through subtasks sequentially
task-master set-status --id=1.1 --status=in-progress
# ... implement subtask 1.1 ...
task-master set-status --id=1.1 --status=done

# Progress tracking
task-master status            # View project progress
```

**Task Structure:**
- Each main task (1-15) has 5 subtasks
- Subtasks have proper dependencies
- Average time: 30-60 minutes per subtask
- Total implementation: ~40-75 hours

## Development Commands (Once Implemented)

### Backend (Common Lisp)

**Start REPL and server:**
```bash
cd backend
ros run
```

In REPL:
```lisp
(ql:quickload :collabcanvas)
(collabcanvas:start)          ; Start server on port 8080
(collabcanvas:stop)           ; Stop server
(collabcanvas:restart)        ; Restart server
```

**Reload after code changes:**
```lisp
;; Reload specific file
(load "src/websocket.lisp")

;; Force reload entire system
(asdf:load-system :collabcanvas :force t)
```

**Create start script:**
```bash
#!/bin/bash
# backend/start.sh
mkdir -p data
if [ ! -f data/canvas.db ]; then
    sqlite3 data/canvas.db < db/schema.sql
fi
ros -s collabcanvas -e '(collabcanvas:start)' -e '(loop (sleep 1))'
```

### Frontend (JavaScript/PixiJS)

**Development server:**
```bash
cd frontend
npm install
npm run dev        # Starts on http://localhost:5173
```

**Build for production:**
```bash
cd frontend
npm run build      # Outputs to frontend/dist/
```

### Testing Multiplayer

1. Start backend: `cd backend && ./start.sh`
2. Start frontend: `cd frontend && npm run dev`
3. Open http://localhost:5173 in two browser windows
4. Register different accounts in each
5. Test real-time collaboration features

**Health check:**
```bash
curl http://localhost:8080/health
```

## Architecture

### Backend Structure

The backend follows a modular Lisp architecture:

- **main.lisp** - Server lifecycle, HTTP handlers, WebSocket dispatcher
- **websocket.lisp** - Real-time message handling, room management, broadcasting
- **database.lisp** - SQLite operations (users, sessions, canvas state)
- **auth.lisp** - User registration, login, session management
- **config.lisp** - Configuration parameters
- **canvas-state.lisp** - Canvas state persistence wrapper

Key pattern: WebSocket messages route through `text-message-received` method, which dispatches based on message type (cursor, object-create, object-update, object-delete).

### Frontend Structure

- **main.js** - Application entry point, initialization, auth flow
- **canvas.js** - PixiJS canvas manager (pan/zoom, object creation/manipulation)
- **websocket.js** - WebSocket client, message handling, reconnection
- **auth.js** - Authentication UI and logic

Key pattern: CanvasManager handles all PixiJS rendering and interaction. It exposes callbacks (onObjectCreated, onObjectMoved, onObjectDeleted) that WebSocketClient uses to broadcast changes.

### Communication Protocol

**Client → Server:**
- `{type: 'auth', sessionId, username, canvasId}` - Authenticate
- `{type: 'cursor', x, y}` - Cursor position
- `{type: 'object-create', object: {...}}` - Create object
- `{type: 'object-update', objectId, updates: {...}}` - Update object
- `{type: 'object-delete', objectId}` - Delete object

**Server → Client:**
- `{type: 'auth-success', userId, username}` - Auth confirmation
- `{type: 'cursor', userId, username, x, y}` - Remote cursor
- `{type: 'presence', users: [...]}` - Active users
- Object messages broadcast to all clients

### Coordinate Systems

Frontend uses two coordinate systems:
- **Screen coordinates**: Browser viewport pixels
- **World coordinates**: Canvas space (pan/zoom applied)

Conversion functions in CanvasManager:
- `screenToWorld(screenX, screenY)` - Convert mouse position to canvas space
- `worldToScreen(worldX, worldY)` - Convert canvas position to screen

This is critical for accurate object placement and cursor synchronization.

### State Management

- **Client-side**: CanvasManager maintains Map of objects (id → PIXI.Graphics)
- **Server-side**: Canvas state stored in SQLite as JSON blob
- **Sync strategy**: Last-write-wins (simple conflict resolution)

### WebSocket Room Management

Each canvas has a unique ID. Server maintains hash table of rooms (canvas-id → canvas-room). Rooms track connected clients and broadcast messages to all participants.

## Key Implementation Files

**backend-main.lisp** (7KB) - Complete working backend with HTTP API, WebSocket dispatch, CORS handling, and server lifecycle.

**frontend-canvas.js** (14KB) - Complete CanvasManager with pan/zoom, shape creation (rect/circle/text), drag-and-drop, multi-select, keyboard shortcuts, and remote cursor visualization.

These files are production-ready implementations, not templates.

## Development Workflow

1. **REPL-driven development**: Make changes in editor, reload in REPL without restarting server
2. **Hot reload frontend**: Vite automatically reloads on file changes
3. **Test incrementally**: Use two browser windows to verify real-time sync after each feature
4. **Profile performance**: Target 60 FPS, use Chrome DevTools Performance tab

## Common Tasks

**Add new WebSocket message type:**
1. Add handler in `backend/src/websocket.lisp` (text-message-received method)
2. Add case to `frontend/src/websocket.js` (handleMessage method)
3. Update protocol documentation

**Add new shape type:**
1. Add creation method to CanvasManager (e.g., createPolygon)
2. Update createToolObject and setupToolHandlers
3. Add serialization in createObjectFromData (WebSocketClient)

**Debug sync issues:**
1. Open browser DevTools → Network → WS tab
2. Inspect messages sent/received
3. Check backend logs for message processing
4. Verify JSON parsing on both sides

## Performance Considerations

- **Cursor updates**: Throttle to 30/sec to reduce bandwidth
- **Object updates**: Debounce drag events (send on mouseup, not every mousemove)
- **PixiJS optimization**: Use containers to group objects, limit draw calls
- **WebSocket backpressure**: Check readyState before sending

## Deployment

**Build and deploy to Fly.io:**
```bash
# Build frontend
cd frontend
npm run build

# Deploy
fly deploy
```

**Dockerfile** builds Roswell + SBCL, installs dependencies, and creates standalone binary.

## MVP Checklist

Core requirements:
- Pan/zoom canvas (60 FPS)
- Create shapes (rectangle, circle)
- Move objects via drag-and-drop
- Real-time sync (2+ users)
- Multiplayer cursors with usernames
- Presence awareness (who's online)
- User authentication (email/password)

## Documentation Files

- **README.md** - Project overview, quick start, API reference
- **COLLABCANVAS_ARCHITECTURE.md** - Complete technical architecture and implementation code
- **QUICKSTART_GUIDE.md** - Step-by-step 24-hour implementation guide
- **MVP_CHECKLIST.md** - Sprint plan with acceptance criteria
- **PROJECT_SUMMARY.md** - Package contents and learning path

## Important Notes

- The project uses Roswell for Lisp environment management - ensure it's installed first
- WebSocket connections require matching canvas IDs between client/server
- CORS is configured to allow all origins in development - restrict in production
- Session timeout is 24 hours (configurable in config.lisp)
- Database is SQLite, stored at `backend/data/canvas.db`

## Task Master AI Instructions
**Import Task Master's development workflow commands and guidelines, treat as if import is in the main CLAUDE.md file.**
@./.taskmaster/CLAUDE.md
