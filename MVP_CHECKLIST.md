# CollabCanvas MVP - 24 Hour Sprint Checklist

**🎯 Task Master Integration:** This checklist corresponds to **15 main tasks with 75 detailed subtasks** in Task Master. Use the granular subtasks for precise tracking while following this hour-by-hour guide for workflow pacing.

```bash
# Track implementation with Task Master
task-master list              # View all 15 tasks + 75 subtasks
task-master next              # Get next subtask to work on
task-master show 1            # View Task 1 with all 5 subtasks
task-master status            # Overall progress dashboard

# Work through subtasks (30-60 min each)
task-master set-status --id=1.1 --status=in-progress
# ... implement subtask 1.1 ...
task-master set-status --id=1.1 --status=done
```

**Mapping:** Each hour-by-hour section below contains multiple Task Master subtasks. The checklist items represent aggregated goals, while subtasks provide granular implementation steps.

---

## Hour-by-Hour Implementation Guide

---

## Hours 0-2: Foundation Setup

### ✅ Backend Bootstrap

- [ ] Install Roswell if not already installed
- [ ] Create project structure
- [ ] Write `collabcanvas.asd` system definition
- [ ] Create all package definitions in `src/package.lisp`
- [ ] Link project to Roswell: `ln -s $(pwd) ~/.roswell/local-projects/collabcanvas`
- [ ] Register with Quicklisp: `ros -e '(ql:register-local-projects)'`
- [ ] Install dependencies: `ros -e '(ql:quickload :collabcanvas)'`
- [ ] Test REPL loads: `ros run` then `(ql:quickload :collabcanvas)`

**Test:** Can load system without errors

### ✅ Database Setup

- [ ] Create `db/schema.sql` with users, sessions, canvas_states tables
- [ ] Write `src/database.lisp` with init-db, create-user, find-user-by-email
- [ ] Test database initialization: `sqlite3 data/canvas.db < db/schema.sql`
- [ ] Verify tables: `sqlite3 data/canvas.db ".tables"`

**Test:** Can create database and query tables

---

## Hours 2-4: Authentication

### ✅ Auth Module

- [ ] Implement password hashing in `src/auth.lisp`
- [ ] Write register-user function
- [ ] Write login-user function with session creation
- [ ] Write verify-session function
- [ ] Test in REPL:
  ```lisp
  (ql:quickload :collabcanvas)
  (collabcanvas.database:init-db)
  (collabcanvas.auth:register-user "test@test.com" "password" "testuser")
  (collabcanvas.auth:login-user "test@test.com" "password")
  ```

**Test:** Can register, login, and verify session

### ✅ HTTP API

- [ ] Write `/api/register` endpoint in `src/main.lisp`
- [ ] Write `/api/login` endpoint
- [ ] Write `/api/logout` endpoint
- [ ] Add CORS headers to all endpoints
- [ ] Test with curl:
  ```bash
  curl -X POST http://localhost:8080/api/register \
    -H "Content-Type: application/json" \
    -d '{"email":"test@test.com","password":"test","username":"tester"}'
  ```

**Test:** Can register and login via HTTP

---

## Hours 4-8: WebSocket Foundation

### ✅ WebSocket Server

- [ ] Create canvas-room class in `src/websocket.lisp`
- [ ] Implement client-connected method
- [ ] Implement client-disconnected method
- [ ] Implement text-message-received method
- [ ] Add room management (get-or-create-room)
- [ ] Create WebSocket dispatch function
- [ ] Start WebSocket acceptor in main.lisp

**Test:** Can connect WebSocket from browser console:
```javascript
const ws = new WebSocket('ws://localhost:8080/ws/test-canvas');
ws.onopen = () => console.log('Connected!');
```

### ✅ Message Handlers

- [ ] Implement handle-auth for client authentication
- [ ] Implement handle-cursor for cursor updates
- [ ] Implement handle-object-create
- [ ] Implement handle-object-update
- [ ] Add broadcast-to-all function
- [ ] Add broadcast-to-others function
- [ ] Add broadcast-presence function

**Test:** Can send/receive messages

---

## Hours 8-12: Frontend Core

### ✅ Project Setup

- [ ] Initialize npm project in frontend/
- [ ] Install PixiJS: `npm install pixi.js@^7.3.0`
- [ ] Install Vite: `npm install vite@^5.0.0 --save-dev`
- [ ] Create `index.html` with canvas container
- [ ] Configure `vite.config.js` with proxy
- [ ] Add dev script to package.json

**Test:** `npm run dev` starts server

### ✅ Canvas Manager

- [ ] Create `src/canvas.js`
- [ ] Initialize PIXI Application
- [ ] Implement pan (middle-click drag)
- [ ] Implement zoom (mouse wheel)
- [ ] Create viewport container
- [ ] Test pan/zoom works smoothly

**Test:** Can pan and zoom canvas at 60 FPS

### ✅ Shape Creation

- [ ] Implement createRectangle method
- [ ] Implement createCircle method
- [ ] Implement makeDraggable for objects
- [ ] Store objects in Map (id -> PIXI object)
- [ ] Add keyboard shortcuts (R for rectangle, C for circle)

**Test:** Can create and drag shapes locally

---

## Hours 12-16: Real-Time Sync

### ✅ WebSocket Client

- [ ] Create `src/websocket.js`
- [ ] Implement connect method
- [ ] Implement send method
- [ ] Implement message handler
- [ ] Add reconnection logic
- [ ] Send cursor position on mousemove (throttled)

**Test:** Browser console shows WebSocket messages

### ✅ Sync Implementation

- [ ] Handle 'object-create' messages
- [ ] Handle 'object-update' messages
- [ ] Handle 'cursor' messages
- [ ] Send object creation to server
- [ ] Send object updates to server
- [ ] Update remote objects from messages

**Test:** Changes in one window appear in another

### ✅ Cursor Sync

- [ ] Create cursor graphics for remote users
- [ ] Store cursors in Map (userId -> cursor)
- [ ] Update cursor position on 'cursor' message
- [ ] Show username label next to cursor
- [ ] Remove cursor on user disconnect

**Test:** See multiplayer cursors with names

---

## Hours 16-20: Polish & Persistence

### ✅ State Persistence

- [ ] Implement save-canvas-state in database.lisp
- [ ] Implement load-canvas-state
- [ ] Save canvas state on object changes (debounced)
- [ ] Load canvas state on connection
- [ ] Send initial state to new clients

**Test:** Refresh page, objects remain

### ✅ Presence Awareness

- [ ] Show list of online users
- [ ] Update presence on connect/disconnect
- [ ] Handle broadcast-presence in frontend
- [ ] Display user count in UI

**Test:** See who's online

### ✅ UI Improvements

- [ ] Add toolbar for shape creation
- [ ] Add color picker
- [ ] Show current tool/mode
- [ ] Add mini-map (optional but cool)
- [ ] Style with CSS

**Test:** UI is usable and clear

---

## Hours 20-24: Testing & Deployment

### ✅ Integration Testing

- [ ] Test with 2 browser windows
- [ ] Test with 3+ browser windows
- [ ] Test rapid object creation
- [ ] Test concurrent editing
- [ ] Test page refresh during editing
- [ ] Test user disconnect/reconnect
- [ ] Test network throttling (Chrome DevTools)

**Test:** All multiplayer features work reliably

### ✅ Performance Testing

- [ ] Check FPS during pan/zoom (should be 60)
- [ ] Check sync latency (<100ms for objects)
- [ ] Check cursor latency (<50ms)
- [ ] Create 100+ objects - still smooth?
- [ ] Profile with Chrome DevTools

**Test:** Meets performance targets

### ✅ Deployment Prep

- [ ] Build frontend: `npm run build`
- [ ] Create Dockerfile
- [ ] Create fly.toml
- [ ] Test Docker build locally
- [ ] Deploy to Fly.io: `fly deploy`
- [ ] Test deployed version

**Test:** Works on public URL

### ✅ Documentation

- [ ] Update README with setup instructions
- [ ] Document API endpoints
- [ ] Document WebSocket message format
- [ ] Add architecture diagram
- [ ] Record 3-5 minute demo video

**Test:** Someone else can run your code

---

## MVP Acceptance Criteria

### Must Pass All:

1. **Authentication**
   - [ ] Can register with email/password
   - [ ] Can login and get session
   - [ ] Session persists across refreshes

2. **Canvas**
   - [ ] Infinite pan with middle-click or Alt+drag
   - [ ] Smooth zoom with mouse wheel
   - [ ] Can create at least one shape type
   - [ ] Can move shapes by dragging

3. **Real-Time Sync**
   - [ ] Two users can connect simultaneously
   - [ ] Cursor positions sync in real-time
   - [ ] Object creation syncs immediately
   - [ ] Object updates sync immediately
   - [ ] All users see same state

4. **Persistence**
   - [ ] Canvas state saves to database
   - [ ] Refresh page - objects remain
   - [ ] All users leave and rejoin - work is saved

5. **Presence**
   - [ ] Show who's online
   - [ ] Multiplayer cursors with names
   - [ ] Update presence on connect/disconnect

6. **Deployment**
   - [ ] Publicly accessible URL
   - [ ] Multiple users can connect
   - [ ] Works in different browsers

7. **Performance**
   - [ ] 60 FPS during all interactions
   - [ ] Object sync <100ms latency
   - [ ] Cursor sync <50ms latency
   - [ ] No memory leaks over 5 minutes

---

## Troubleshooting Quick Fixes

### Backend Won't Start

```bash
# Clear FASL cache
rm -rf ~/.cache/common-lisp/

# Re-register project
ros -e '(ql:register-local-projects)'

# Force recompile
ros -e '(asdf:compile-system :collabcanvas :force t)'
```

### WebSocket Won't Connect

- Check backend is running: `curl http://localhost:8080/api/login`
- Check WebSocket URL matches backend port
- Check browser console for errors
- Verify no firewall blocking WebSocket

### Objects Not Syncing

- Open browser console in both windows
- Check WebSocket messages are being sent
- Verify message format matches backend expectations
- Check backend logs for errors
- Test with just two users (easier to debug)

### Performance Issues

- Reduce cursor update frequency (throttle to 30/sec)
- Use requestAnimationFrame for rendering
- Check for memory leaks (DevTools Memory tab)
- Profile with Chrome Performance tab
- Reduce object complexity (simpler shapes)

### Deployment Fails

- Test Docker build locally first: `docker build -t test .`
- Check Dockerfile paths are correct
- Verify frontend is built before copying to Docker
- Check fly.toml configuration
- Review Fly.io logs: `fly logs`

---

## Post-MVP Enhancements

After you have a working MVP, consider:

1. **Selection** - Multi-select with shift-click
2. **Layers** - Z-index management
3. **Undo/Redo** - Command pattern
4. **Export** - PNG/JSON export
5. **AI Commands** - "Create a login form"
6. **More Shapes** - Text, lines, polygons
7. **Styling** - Colors, borders, gradients
8. **Collaboration** - Comments, threads
9. **Permissions** - Read-only users
10. **History** - Version control

---

## Success Metrics

By hour 24, you should have:

- ✅ 2+ users editing simultaneously
- ✅ Real-time cursor and object sync
- ✅ State persistence across sessions
- ✅ 60 FPS performance
- ✅ Deployed to public URL
- ✅ Clean, documented code
- ✅ Demo video recorded

**Remember:** Perfect is the enemy of done. Ship the MVP first, iterate later!

---

## Final Pre-Submission Checklist

- [ ] Code pushed to GitHub
- [ ] README has setup instructions
- [ ] Architecture documented
- [ ] Demo video shows key features
- [ ] Deployed app is accessible
- [ ] Two test accounts created
- [ ] Performance meets targets
- [ ] No critical bugs

**You got this!** 🚀
