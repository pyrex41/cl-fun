# CollabCanvas
### Real-Time Collaborative Design Tool with AI
*Built with Common Lisp + PixiJS*

A Figma-like collaborative canvas where multiple designers can work together in real-time, with an AI agent that can create and manipulate designs through natural language.

---

## 📋 What You're Building

This project implements:

- **Real-time multiplayer canvas** with WebSocket sync
- **60 FPS pan and zoom** using PixiJS
- **Live cursor tracking** showing all connected users
- **Object creation and manipulation** (rectangles, circles, text)
- **State persistence** with SQLite
- **Email/password authentication** with sessions
- **AI agent integration** for natural language design commands

---

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────┐
│                   Frontend                       │
│  ┌──────────────────────────────────────────┐  │
│  │         PixiJS Canvas (WebGL)             │  │
│  │  - Pan/Zoom - Shapes - Transformations    │  │
│  └──────────────────────────────────────────┘  │
│               ↕ WebSocket                       │
└─────────────────────────────────────────────────┘
                      ↕
┌─────────────────────────────────────────────────┐
│                Backend (Common Lisp)             │
│  ┌──────────────┐    ┌───────────────────────┐ │
│  │ Hunchentoot  │    │   Hunchensocket       │ │
│  │   HTTP API   │    │  Real-time Sync       │ │
│  └──────────────┘    └───────────────────────┘ │
│          ↓                     ↓                │
│  ┌────────────────────────────────────────────┐│
│  │          SQLite Database                    ││
│  │   Users | Sessions | Canvas States         ││
│  └────────────────────────────────────────────┘│
└─────────────────────────────────────────────────┘
```

---

## 🚀 Quick Start

### Prerequisites

```bash
# Install Roswell (Common Lisp environment manager)
brew install roswell  # macOS
# or see ROSWELL_GUIDE.md for other platforms

# Install Node.js 18+
brew install node  # or from nodejs.org

# Install pnpm (optional, can use npm)
npm install -g pnpm
```

### Setup (5 minutes)

```bash
# Clone the repository
git clone <your-repo>
cd collabcanvas

# Backend setup
cd backend
ln -s $(pwd) ~/.roswell/local-projects/collabcanvas
ros -e '(ql:register-local-projects)'
ros -e '(ql:quickload :collabcanvas)'

# Frontend setup
cd ../frontend
pnpm install  # or npm install

# Initialize database
mkdir -p backend/data
sqlite3 backend/data/canvas.db < backend/db/schema.sql
```

### Run Development Servers

**Terminal 1 - Backend:**
```bash
cd backend
./start.sh
# Server starts on http://localhost:8080
```

**Terminal 2 - Frontend:**
```bash
cd frontend
pnpm dev  # or npm run dev
# Dev server starts on http://localhost:5173
```

**Open your browser:**
- Go to http://localhost:5173
- Register an account
- Open a second browser window (different account)
- See real-time collaboration!

---

## 📁 Project Structure

```
collabcanvas/
├── backend/                      # Common Lisp backend
│   ├── collabcanvas.asd         # ASDF system definition
│   ├── src/
│   │   ├── package.lisp         # Package definitions
│   │   ├── config.lisp          # Configuration
│   │   ├── utils.lisp           # Utilities
│   │   ├── database.lisp        # SQLite operations
│   │   ├── auth.lisp            # Authentication
│   │   ├── websocket.lisp       # Real-time sync
│   │   ├── canvas-state.lisp    # State management
│   │   ├── ai-agent.lisp        # AI integration
│   │   └── main.lisp            # Server entry point
│   ├── db/
│   │   └── schema.sql           # Database schema
│   ├── data/                    # Runtime data (git-ignored)
│   └── start.sh                 # Development startup
├── frontend/                     # PixiJS frontend
│   ├── package.json
│   ├── vite.config.js
│   ├── index.html
│   └── src/
│       ├── main.js              # App entry point
│       ├── canvas.js            # PixiJS canvas logic
│       ├── websocket.js         # WebSocket client
│       ├── auth.js              # Auth UI
│       └── styles.css
├── Dockerfile                    # Container definition
├── fly.toml                      # Fly.io config
└── README.md                     # This file
```

---

## 🎯 MVP Requirements (24 Hours)

### Must Have

- [x] Pan/zoom canvas
- [x] Create shapes (rectangle, circle)
- [x] Move objects
- [x] Real-time sync (2+ users)
- [x] Multiplayer cursors with names
- [x] Presence awareness
- [x] User authentication
- [x] Deployed and accessible

### Performance Targets

- **FPS:** 60 during all interactions
- **Sync Latency:** <100ms for objects, <50ms for cursors
- **Scale:** 500+ objects, 5+ concurrent users

---

## 🔧 Development

### Backend REPL Workflow

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

### Frontend Hot Reload

Vite automatically reloads on file changes. Just edit and save!

### Testing Multiplayer

1. Start backend and frontend
2. Open http://localhost:5173 in two browser windows
3. Register different accounts in each
4. Create objects in one - see them appear in the other
5. Move cursor - see it in the other window

---

## 🌐 Deployment

### Build Frontend

```bash
cd frontend
pnpm build
# Creates frontend/dist/
```

### Deploy to Fly.io

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

# Open app
fly open
```

Your app will be live at `https://collabcanvas-yourname.fly.dev`

---

## 📡 API Reference

### HTTP Endpoints

**POST /api/register**
```json
{
  "email": "user@example.com",
  "password": "securepassword",
  "username": "johndoe"
}
```

**POST /api/login**
```json
{
  "email": "user@example.com",
  "password": "securepassword"
}
```
Response:
```json
{
  "success": true,
  "sessionId": "uuid-here",
  "userId": 1,
  "username": "johndoe"
}
```

**POST /api/logout**
```json
{
  "sessionId": "uuid-here"
}
```

**GET /api/canvas/state?canvasId=xxx**

Returns current canvas state as JSON.

### WebSocket Messages

**Connect:** `ws://localhost:8080/ws/<canvas-id>`

**Client → Server:**

```javascript
// Authenticate
{
  type: 'auth',
  sessionId: 'uuid',
  username: 'johndoe',
  canvasId: 'canvas-123'
}

// Cursor update
{
  type: 'cursor',
  x: 100,
  y: 200
}

// Create object
{
  type: 'object-create',
  object: {
    id: 'obj-123',
    type: 'rectangle',
    x: 100,
    y: 100,
    width: 200,
    height: 100,
    color: 0xFF0000
  }
}

// Update object
{
  type: 'object-update',
  objectId: 'obj-123',
  updates: {
    x: 150,
    y: 150
  }
}

// Delete object
{
  type: 'object-delete',
  objectId: 'obj-123'
}
```

**Server → Client:**

```javascript
// Auth success
{
  type: 'auth-success',
  userId: 1,
  username: 'johndoe'
}

// Remote cursor
{
  type: 'cursor',
  userId: 2,
  username: 'jane',
  x: 250,
  y: 300
}

// Presence update
{
  type: 'presence',
  users: [
    { userId: 1, username: 'john' },
    { userId: 2, username: 'jane' }
  ]
}

// Broadcast messages (same format as client sends)
```

---

## 🤖 AI Agent Integration

### Phase 1: Function Calling Setup

The AI agent uses OpenAI function calling to interpret natural language and execute canvas operations.

**Example Commands:**
- "Create a blue rectangle at position 200, 300"
- "Move the circle to the center"
- "Create a login form"
- "Arrange these shapes in a grid"

**Implementation** (in `src/ai-agent.lisp`):

```lisp
(defun process-ai-command (command canvas-state)
  "Process natural language command and return canvas operations"
  ;; Call OpenAI API with function definitions
  ;; Parse response and return list of operations
  ;; Operations are then broadcast to all clients
  )
```

See `ARCHITECTURE.md` for detailed AI integration guide.

---

## 🐛 Troubleshooting

### Backend won't start

```bash
# Clear FASL cache
rm -rf ~/.cache/common-lisp/

# Re-register and reload
ros -e '(ql:register-local-projects)'
ros -e '(asdf:load-system :collabcanvas :force t)'
```

### WebSocket connection fails

- Check backend is running: `curl http://localhost:8080/health`
- Verify WebSocket URL in frontend matches backend port
- Check browser console for connection errors
- Ensure no firewall blocking port 8080

### Objects not syncing

- Open browser console in both windows
- Check WebSocket status (should be "open")
- Verify messages are being sent/received
- Check backend logs for errors
- Test with simplified scenario (just two users)

### Performance issues

- Reduce cursor update frequency (throttle to 30/sec)
- Check FPS with Chrome DevTools (Performance tab)
- Profile for memory leaks
- Reduce object complexity
- Use browser dev tools to identify bottlenecks

---

## 📚 Resources

### Project Documentation

- **Architecture Guide:** `COLLABCANVAS_ARCHITECTURE.md`
- **Quick Start:** `QUICKSTART_GUIDE.md`
- **MVP Checklist:** `MVP_CHECKLIST.md`
- **Roswell Guide:** `ROSWELL_GUIDE.md`
- **Claude Code Guide:** `CLAUDE.md`

### Task Master Integration

**Implementation is structured using Task Master with 75 detailed subtasks:**

- **15 main tasks** (project structure → deployment)
- **Each task has 5 subtasks** (30-60 min each)
- **Total implementation time:** ~40-75 hours
- **Track progress:** `task-master list` and `task-master status`
- **PRD Document:** `.taskmaster/docs/prd.md`

```bash
# View all tasks
task-master list

# Start next subtask
task-master next
task-master show 1.1

# Track progress
task-master status
```

### External Resources

- [PixiJS Documentation](https://pixijs.download/release/docs/index.html)
- [Hunchensocket Guide](https://github.com/joaotavora/hunchensocket)
- [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/)
- [Fly.io Documentation](https://fly.io/docs/)

---

## 🎨 Features Roadmap

### Phase 1: MVP ✅
- Real-time collaboration
- Basic shapes
- Auth & persistence

### Phase 2: Enhanced Canvas
- [ ] Multi-select
- [ ] Layers & z-index
- [ ] Text tool
- [ ] Lines & paths
- [ ] Undo/redo

### Phase 3: AI Integration
- [ ] Function calling setup
- [ ] Basic commands (create, move, resize)
- [ ] Layout commands (grid, align, distribute)
- [ ] Complex commands (create forms, components)
- [ ] Natural language query

### Phase 4: Polish
- [ ] Export to PNG/JSON
- [ ] Import images
- [ ] Keyboard shortcuts
- [ ] Templates
- [ ] Comments & annotations

---

## 🤝 Contributing

This is a learning project! Feel free to:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

### Code Style

- **Lisp:** Follow standard Common Lisp conventions
- **JavaScript:** ES6+, functional style where possible
- **Comments:** Explain *why*, not *what*
- **Testing:** Add tests for new features

---

## 📄 License

MIT License - see LICENSE file for details

---

## 🙏 Acknowledgments

- Inspired by Figma's collaborative infrastructure
- Built on the shoulders of Common Lisp giants
- PixiJS for incredible 2D rendering performance
- The Roswell project for making CL development accessible

---

## 📧 Contact

Questions? Issues? Reach out or open an issue!

**Remember:** A simple, working multiplayer canvas beats a complex, broken one. Ship the MVP! 🚀
