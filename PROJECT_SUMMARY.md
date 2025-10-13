# CollabCanvas Implementation Package

## 📦 What You Have

This package contains everything you need to build a real-time collaborative canvas (Figma-like) using Common Lisp and PixiJS.

---

## 📄 Documentation Files

### 1. **README.md**
Complete project overview with:
- Architecture diagram
- Quick start guide
- API reference
- Deployment instructions
- Troubleshooting guide

### 2. **COLLABCANVAS_ARCHITECTURE.md**
Comprehensive technical architecture including:
- Full technology stack details
- Complete backend implementation (all Lisp modules)
- Frontend implementation with PixiJS
- Database schema and queries
- WebSocket protocol specification
- Deployment configuration (Docker + Fly.io)

### 3. **QUICKSTART_GUIDE.md**
Step-by-step implementation guide with:
- Hour-by-hour breakdown
- Code examples for critical sections
- Testing procedures
- Common pitfalls and solutions
- Performance optimization tips

### 4. **MVP_CHECKLIST.md**
24-hour sprint checklist with:
- Hour-by-hour tasks
- Acceptance criteria
- Testing scenarios
- Debugging quick fixes
- Success metrics

### 5. **ROSWELL_GUIDE.md** (already provided)
Complete Roswell and Common Lisp development guide

---

## 💻 Implementation Files

### Backend (Common Lisp)

**backend-main.lisp**
Complete, working main.lisp with:
- HTTP API endpoints (register, login, logout)
- WebSocket dispatcher
- CORS handling
- Server lifecycle management
- Health check endpoint
- OPTIONS handler for CORS preflight

Key features:
```lisp
(collabcanvas:start)  ; Start server
(collabcanvas:stop)   ; Stop server
(collabcanvas:main)   ; Entry point for binary
```

### Frontend (JavaScript/PixiJS)

**frontend-canvas.js**
Complete CanvasManager class with:
- Pan/zoom with smooth 60 FPS
- Shape creation (rectangles, circles, text)
- Drag-and-drop for all objects
- Multi-select with Shift
- Keyboard shortcuts (R, C, T, V, Delete)
- Remote cursor visualization
- Object state management
- Screen/world coordinate conversion

Key features:
```javascript
const canvas = new CanvasManager(app);
canvas.setTool('rectangle');
canvas.createRectangle(id, x, y, w, h, color);
canvas.updateRemoteCursor(userId, name, x, y);
```

---

## 🏗️ Project Structure

```
Your Project/
├── README.md                          ← Start here
├── COLLABCANVAS_ARCHITECTURE.md       ← Technical deep dive
├── QUICKSTART_GUIDE.md                ← Implementation walkthrough
├── MVP_CHECKLIST.md                   ← 24-hour sprint plan
├── ROSWELL_GUIDE.md                   ← Lisp development guide
├── backend-main.lisp                  ← Working server code
├── frontend-canvas.js                 ← Complete canvas implementation
│
└── Create these directories:
    ├── backend/
    │   ├── collabcanvas.asd           ← Copy from ARCHITECTURE.md
    │   ├── src/
    │   │   ├── package.lisp          ← Copy from ARCHITECTURE.md
    │   │   ├── config.lisp           ← Copy from ARCHITECTURE.md
    │   │   ├── utils.lisp            ← Copy from ARCHITECTURE.md
    │   │   ├── database.lisp         ← Implement using guide
    │   │   ├── auth.lisp             ← Implement using guide
    │   │   ├── websocket.lisp        ← Implement using guide
    │   │   ├── canvas-state.lisp     ← Simple wrapper module
    │   │   └── main.lisp             ← Use backend-main.lisp
    │   ├── db/
    │   │   └── schema.sql            ← Copy from ARCHITECTURE.md
    │   └── start.sh                  ← Copy from QUICKSTART.md
    │
    └── frontend/
        ├── package.json              ← Copy from ARCHITECTURE.md
        ├── vite.config.js            ← Copy from ARCHITECTURE.md
        ├── index.html                ← Create simple HTML
        └── src/
            ├── main.js               ← Copy pattern from ARCHITECTURE.md
            ├── canvas.js             ← Use frontend-canvas.js
            ├── websocket.js          ← Implement using guide
            ├── auth.js               ← Simple auth UI
            └── styles.css            ← Basic styling
```

---

## 🚀 Implementation Steps

**🎯 Task Master Integration:** Implementation is organized into **15 main tasks with 75 detailed subtasks** (5 per task). Each subtask takes 30-60 minutes, totaling 40-75 hours of implementation time.

```bash
# View task structure
task-master list              # Shows all 15 tasks + 75 subtasks
task-master show 1            # View Task 1 with its 5 subtasks
task-master status            # Overall progress tracking

# Work through implementation
task-master next              # Get next available subtask
task-master set-status --id=1.1 --status=in-progress
# ... implement ...
task-master set-status --id=1.1 --status=done
```

**Task Breakdown:**
- **Tasks 1-3:** Foundation (project structure, database, auth)
- **Tasks 4-7:** Core backend (WebSocket, state management)
- **Tasks 8-10:** Frontend implementation (canvas, sync)
- **Tasks 11-13:** Features & polish (persistence, presence, UI)
- **Tasks 14-15:** Testing & deployment

---

### Phase 1: Setup (30 minutes)

1. **Create project structure:**
   ```bash
   mkdir -p collabcanvas/{backend/src,backend/db,frontend/src}
   cd collabcanvas
   ```

2. **Copy ASDF system definition** from ARCHITECTURE.md to `backend/collabcanvas.asd`

3. **Copy all package definitions** to `backend/src/package.lisp`

4. **Copy database schema** to `backend/db/schema.sql`

5. **Copy backend-main.lisp** to `backend/src/main.lisp`

6. **Link to Roswell:**
   ```bash
   cd backend
   ln -s $(pwd) ~/.roswell/local-projects/collabcanvas
   ros -e '(ql:register-local-projects)'
   ros -e '(ql:quickload :collabcanvas)'
   ```

### Phase 2: Backend Core (2-4 hours)

Implement these files using code from ARCHITECTURE.md:

1. `src/config.lisp` ✅ (provided in docs)
2. `src/utils.lisp` ✅ (provided in docs)
3. `src/database.lisp` (implement all functions from ARCHITECTURE.md)
4. `src/auth.lisp` (implement authentication logic)
5. `src/websocket.lisp` (implement WebSocket handlers)
6. `src/canvas-state.lisp` (simple in-memory state wrapper)

**Test each module:**
```lisp
(ql:quickload :collabcanvas)
(collabcanvas:start)
```

### Phase 3: Frontend Core (2-4 hours)

1. **Initialize npm project:**
   ```bash
   cd frontend
   npm init -y
   npm install pixi.js@^7.3.0 vite@^5.0.0
   ```

2. **Copy frontend-canvas.js** to `frontend/src/canvas.js`

3. **Implement WebSocket client** using pattern from ARCHITECTURE.md

4. **Create simple HTML** with canvas container and auth modal

5. **Wire everything together** in `main.js`

### Phase 4: Integration & Testing (2-4 hours)

1. **Start both servers**
2. **Test with 2 browser windows**
3. **Verify all MVP requirements**
4. **Fix any sync issues**
5. **Performance profiling**

### Phase 5: Deployment (1-2 hours)

1. **Build frontend:** `npm run build`
2. **Create Dockerfile** (from ARCHITECTURE.md)
3. **Deploy to Fly.io:** `fly deploy`
4. **Test production**

---

## 🎯 MVP Requirements Checklist

Your project must have:

- [ ] **Pan/zoom canvas** (middle-click/Alt+drag, mouse wheel)
- [ ] **Create shapes** (at least rectangle or circle)
- [ ] **Move objects** (drag and drop)
- [ ] **Real-time sync** (2+ users see same state)
- [ ] **Multiplayer cursors** (with names)
- [ ] **Presence awareness** (who's online)
- [ ] **User authentication** (email/password)
- [ ] **Deployed** (publicly accessible URL)

Performance:
- [ ] **60 FPS** during pan/zoom/interactions
- [ ] **<100ms** object sync latency
- [ ] **<50ms** cursor sync latency
- [ ] **500+ objects** without FPS drop
- [ ] **5+ concurrent users** without degradation

---

## 📋 Key Implementation Notes

### Critical Backend Patterns

1. **WebSocket Message Handling:**
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
     (dolist (client-pair (room-clients room))
       (send-text-message (car client-pair) message)))
   ```

3. **State Persistence:**
   ```lisp
   (save-canvas-state canvas-id 
     (jonathan:to-json (get-all-objects)))
   ```

### Critical Frontend Patterns

1. **Coordinate Conversion:**
   ```javascript
   screenToWorld(screenX, screenY) {
     return {
       x: (screenX - this.viewport.x) / this.viewport.scale.x,
       y: (screenY - this.viewport.y) / this.viewport.scale.y
     };
   }
   ```

2. **WebSocket Message Sending:**
   ```javascript
   send(data) {
     if (this.ws.readyState === WebSocket.OPEN) {
       this.ws.send(JSON.stringify(data));
     }
   }
   ```

3. **Object Creation:**
   ```javascript
   const rect = new PIXI.Graphics();
   rect.beginFill(color);
   rect.drawRect(0, 0, width, height);
   rect.endFill();
   this.viewport.addChild(rect);
   ```

---

## 🐛 Common Issues & Solutions

### "Package COLLABCANVAS does not exist"
```bash
ros -e '(ql:register-local-projects)'
ros -e '(asdf:load-system :collabcanvas :force t)'
```

### WebSocket connection refused
- Check backend is running: `curl http://localhost:8080/health`
- Verify WebSocket URL matches backend port
- Check CORS settings

### Objects not syncing
- Open browser DevTools → Network → WS tab
- Verify messages are being sent/received
- Check message format matches backend expectations

### Poor performance
- Throttle cursor updates (max 30/second)
- Use `requestAnimationFrame` for rendering
- Profile with Chrome DevTools

---

## 📚 Learning Path

### Day 1: Foundation
- Read QUICKSTART_GUIDE.md
- Set up backend skeleton
- Get HTTP server running
- Test with curl

### Day 2: Core Features
- Implement WebSocket handlers
- Build PixiJS canvas
- Get cursor sync working
- Test with 2 windows

### Day 3: Polish & Deploy
- Add shape creation
- Implement persistence
- Performance optimization
- Deploy to Fly.io

---

## 🎓 After MVP

Once you have a working MVP, enhance with:

1. **More shapes:** Lines, polygons, text
2. **Transformations:** Resize, rotate, scale
3. **Layers:** Z-index, grouping
4. **Undo/redo:** Command pattern
5. **AI agent:** Function calling for natural language
6. **Export:** PNG, JSON, SVG
7. **Collaborative features:** Comments, version history

---

## 🤝 Getting Help

If you get stuck:

1. **Check MVP_CHECKLIST.md** for debugging steps
2. **Review ARCHITECTURE.md** for implementation details
3. **Consult ROSWELL_GUIDE.md** for Lisp-specific issues
4. **Use browser DevTools** to inspect WebSocket messages
5. **Test incrementally** - don't build everything at once

---

## 🎉 Success Criteria

You've succeeded when:

- ✅ Two users can edit simultaneously
- ✅ Changes sync in real-time
- ✅ State persists across page refreshes
- ✅ Performance is smooth (60 FPS)
- ✅ App is deployed and accessible
- ✅ Code is clean and documented

**Remember:** A simple, working multiplayer canvas beats a complex, broken one. Ship incrementally!

---

## 📦 File Summary

You have:
- ✅ 5 comprehensive documentation files
- ✅ 1 complete backend implementation (main.lisp)
- ✅ 1 complete frontend implementation (canvas.js)
- ✅ Database schema and API specifications
- ✅ Deployment configuration
- ✅ Testing and debugging guides

**Everything you need to build CollabCanvas!**

Start with QUICKSTART_GUIDE.md and follow the hour-by-hour plan. You got this! 🚀
