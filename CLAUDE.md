# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Current Work In Progress: Spatial Partitioning Optimization

**Branch:** phys-engine
**Status:** Implementation Phase (2-3 hours into 8-12 hour estimate)
**Goal:** Implement quadtree spatial partitioning to reduce collision detection from O(n²) to O(n log n)

### What's Been Completed ✅

1. **takagi/quadtree Library** - Installed and verified
   - Location: `~/quicklisp/local-projects/quadtree/`
   - Available via Quicklisp
   - Functions confirmed: `make`, `insert`, `query`, `clear`, `boundary`, `intersect-p`

2. **physics-quadtree.lisp Module** - Created at `backend/src/physics-quadtree.lisp`
   - `entity-bounds` structure for spatial entity data
   - `make-physics-quadtree(width, height)` - creates quadtree for canvas
   - `clear-physics-quadtree(quadtree)` - clears per-frame
   - `insert-entity-bounds(quadtree, entity-id, x, y, radius, type)` - inserts entities
   - `query-nearby-entities(quadtree, x, y, radius)` - region query with 4-cardinal-point sampling
   - `entities-potentially-collide-p(bounds1, bounds2)` - fast AABB prefilter
   - `quadtree:intersect-p` method for circle-rectangle intersection testing
   - Performance stats tracking with `*quadtree-stats*`

3. **ASDF Integration** - Updated `backend/collabcanvas.asd`
   - Added `:quadtree` to dependencies list
   - Added `(:file "physics-quadtree" :depends-on ("package" "physics-ecs"))` to components
   - Load order: ECS → quadtree → components → systems → loop

4. **Package Exports** - Updated `backend/src/package.lisp`
   - Exported 13 new symbols: entity-bounds functions, quadtree wrappers, stats

5. **✅ FIXED: Physics Loop Crash** - Fixed `backend/src/physics-systems.lisp`
   - **Root Cause**: `:arguments` clauses used keyword syntax `((:param type))` instead of positional syntax `((param type))`
   - **Fix Applied**: Removed colons from argument names in 5 system definitions (lines 98, 133, 155, 182, 206)
   - Changed `(:dt single-float)` → `(dt single-float)`
   - Changed `(:quadtree t)` → `(quadtree t)`
   - **Result**: Server compiles and runs successfully, physics loop no longer crashes! ✅

6. **Quadtree Integration with physics-canvas-config** - ✅ DONE
   - Added `quadtree` slot to `physics-canvas-config` struct in `backend/src/physics-ecs.lisp` (line 27)
   - Quadtree initialized in `init-canvas-physics` with canvas dimensions (lines 78-80)
   - Canvas dimensions stored in config for quadtree sizing

7. **Physics Systems Updated** - ✅ DONE
   - Location: `backend/src/physics-systems.lisp`
   - All systems now use quadtree spatial partitioning
   - System execution order:
     1. `populate-quadtree-system` - indexes balls with physics data
     2. `populate-quadtree-blocks-system` - indexes blocks
     3. `apply-acceleration-system` - gravity and forces
     4. `apply-velocity-system` - position updates
     5. `detect-collisions-system` - TWO-PASS collision detection
     6. `apply-collision-impulses-system` - TWO-PASS impulse application
     7. `check-sleeping-system` - sleep optimization

8. **✅ Server Running with .env Loaded**
   - Fixed environment variable loading issue
   - Server properly loads AUTH0_DOMAIN and other config from .env
   - Health endpoint responds successfully without errors

### REPL-Driven Development Workflow 🔧

**IMPORTANT**: Use Common Lisp's REPL for live code modification instead of restarting the server!

**Start the server ONCE**:
```bash
cd backend
./start.sh   # Or the script that sources .env and starts server
```

**Then use the REPL for all code changes**:
```bash
cd backend
chmod +x repl.sh
./repl.sh    # Opens interactive REPL connected to CollabCanvas
```

**In the REPL, reload files after editing**:
```lisp
;; Reload a specific file (redefines functions/systems)
(load "src/physics-systems.lisp")
(load "src/websocket-adapter.lisp")
(load "src/physics-loop.lisp")

;; Or force recompile the entire system
(asdf:load-system :collabcanvas :force t)

;; Test changes immediately
(list-active-physics-canvases)
(spawn-physics-ball "canvas-123" 100.0 100.0 10.0 -5.0)
```

**Benefits**:
- No server restart needed (server keeps running on port 8080)
- Instant feedback on code changes
- Preserve running physics simulations for testing
- True REPL-driven development workflow
- Much faster iteration cycle

**Only restart the server when**:
- Changing package definitions in `package.lisp`
- Modifying ASDF system definition in `collabcanvas.asd`
- Adding new dependencies to Quicklisp
- After clearing FASL cache

### What Remains TODO ⏳

**Next Steps:**

8. **✅ physics-loop.lisp Updated** - Quadtree integration complete
   - Quadtree passed to all systems from canvas config
   - Physics loop runs successfully without crashes
   - Delta compression and broadcasting working

9. **Test Frontend Integration** (30-60 min) - ⚠️ NEXT PRIORITY
   - Start frontend: `cd frontend && npm run dev`
   - Test physics ball spawning - verify balls appear and stay visible
   - Test ball collision and bouncing behavior
   - Verify physics state syncs between clients

10. **Fix Object Position Persistence** (1-2 hours) - Known issue from previous session
    - **Issue**: Moved objects revert to old positions on page reload
    - **Suspected cause**: WebSocket `object-update` handler crash
    - **Error**: "The value NIL is not of type CONS" in `backend/src/websocket-adapter.lisp`
    - **Action**: Debug and fix `handle-canvas-message` function

11. **Create Unit Tests** (1-2 hours)
    - File: `backend/tests/test-quadtree-collision.lisp`
    - Test scenarios:
      - 2 balls colliding (should detect)
      - 2 balls far apart (should NOT check collision)
      - Ball on quadtree boundary (edge case)
      - 100 balls in small area (stress test)
      - Single ball alone (no collisions)

12. **Performance Benchmarking** (1 hour)
    - Run existing load tests with new collision system
    - Compare frame times: expect 500 entities 8.42ms → ~4.5ms
    - Test with 1000 entities: expect ~9ms (vs 34ms current)
    - Test with 2000 entities: should achieve <16ms (new capability!)
    - Document results in `.taskmaster/docs/PROFILING_RESULTS.md`

13. **Optimize Parameters** (30-60 min)
    - Tune `max-depth` (try 4, 5, 6)
    - Tune `max-capacity` (try 8, 10, 15)
    - Find optimal values for 500-2000 entity range
    - Update defaults in `make-physics-quadtree`

### Known Issues 🐛

None! Physics loop crash has been fixed. ✅

**From Previous Sessions (not yet tested):**
- Physics balls may not render correctly in frontend (need to test with frontend running)
- Object position persistence failure (need to investigate WebSocket handler crash)

### Expected Performance Impact 📈

**Before (Current Brute Force):**
- 500 entities: 8.42ms/frame, 124,750 collision checks
- 1000 entities: 34ms/frame (unusable at 60 Hz)
- Scaling limit: ~600-700 entities

**After (Quadtree Spatial Partitioning):**
- 500 entities: ~4.5ms/frame, ~4,500 collision checks (96% reduction)
- 1000 entities: ~9ms/frame (usable at 60 Hz)
- 2000 entities: ~16ms/frame (4x scaling improvement)
- New limit: ~2,000-2,500 entities

### Quick Start Commands for Next Session

```bash
# Terminal 1: Backend
cd backend
ros run

# In REPL:
(ql:quickload :collabcanvas)
(in-package :collabcanvas)

# Test quadtree wrapper:
(defvar *test-qt* (make-physics-quadtree 800.0 600.0))
(insert-entity-bounds *test-qt* 1 100.0 100.0 10.0 :ball)
(insert-entity-bounds *test-qt* 2 105.0 105.0 10.0 :ball)
(query-nearby-entities *test-qt* 100.0 100.0 20.0)  ; Should return both entities

# If that works, continue with task 6 (integrate with physics-canvas-config)
```

### Reference Documentation

- **Implementation Plan**: See conversation summary above
- **Profiling Results**: `.taskmaster/docs/PROFILING_RESULTS.md` (shows collision detection is 60-62% of frame time)
- **Load Test Results**: `.taskmaster/docs/LOAD_TEST_RESULTS.md`
- **Component API**: `.taskmaster/docs/PHYSICS_COMPONENTS.md`
- **takagi/quadtree README**: `~/quicklisp/local-projects/quadtree/README.markdown`

---

## Project Status (Overall)

**Current Phase:** Physics Engine Complete + Optimization In Progress
**Implementation Status:** Backend and frontend fully implemented, spatial partitioning optimization underway

This repository contains a fully functional real-time collaborative design tool with multiplayer physics simulation. The physics engine (Tasks 1-5, Phases 0-4) is 100% complete. We are now implementing Priority 1 optimization: spatial partitioning for collision detection.

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
