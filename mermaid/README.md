# CollabCanvas Architecture Diagrams

This folder contains Mermaid diagrams documenting the complete CollabCanvas architecture including the physics engine, authentication, and real-time collaboration features.

## Viewing Diagrams

These diagrams use Mermaid syntax. To view them:

1. **GitHub** - Diagrams render automatically in `.mermaid` files on GitHub
2. **VS Code** - Install "Markdown Preview Mermaid Support" extension
3. **Online** - Copy diagram content to https://mermaid.live

## Diagram Index

### 1. System Architecture (`system_architecture.mermaid`)
**Type:** Class Diagram  
**Purpose:** High-level overview of the entire system

Shows all major components and their relationships:
- **Backend Core**: Server, config, database
- **Authentication**: Traditional auth + Auth0 OAuth2
- **WebSocket Infrastructure**: Real-time communication
- **Physics Engine**: ECS-based simulation with spatial partitioning
- **Frontend**: PixiJS canvas, WebSocket client, physics visualization
- **External Libraries**: cl-fast-ecs, quadtree, PixiJS

**Key Insights:**
- Physics engine built on cl-fast-ecs with quadtree optimization
- Dual authentication (traditional + Auth0)
- AI agent integration for natural language commands
- Client-side prediction with server-authoritative physics

---

### 2. Database Schema (`schema.mermaid`)
**Type:** Entity-Relationship Diagram  
**Purpose:** Complete database structure

Tables:
- **Core**: users, sessions, canvas_states, canvas_history, collaborators
- **Physics**: physics_canvas_settings, physics_components, physics_bodies, physics_component_types

**Key Features:**
- Auth0 integration via `auth0_sub` field
- User color preferences
- Physics component types: ball, fan, block, emitter, magnet
- Runtime physics state persistence
- Canvas version control

---

### 3. Backend Modules (`backend_modules.mermaid`)
**Type:** Dependency Graph  
**Purpose:** Backend module loading order and dependencies

**Module Categories:**
- **Core** (blue): package, config, utils, database
- **Auth** (orange): auth, auth0-config, auth0-oauth, auth-metrics
- **Canvas** (purple): canvas-state, components, ai-agent
- **Physics** (green): 6 physics modules in strict load order
- **Server** (pink): websocket-adapter, app, server, main

**Critical Load Order:**
1. `physics-ecs` → Core ECS setup
2. `physics-quadtree` → Spatial partitioning
3. `physics-components` → Ball, fan, block definitions
4. `physics-systems` → Force, collision, boundary logic
5. `physics-loop` → 60Hz simulation loop

---

### 4. Physics Engine ECS (`physics_ecs.mermaid`)
**Type:** Flow Diagram  
**Purpose:** Physics engine internal architecture

**Components (Data):**
- position (x, y)
- velocity (vx, vy)
- ball (radius, mass)
- fan (force, direction, radius, angle)
- block (width, height, is-static)
- emitter (spawn-rate, max-balls)
- magnet (force, radius, repel/attract)

**Systems (Logic):**
1. Apply Forces (gravity + custom)
2. Fan System (directional forces)
3. Magnet System (attraction/repulsion)
4. Collision System (ball-ball, ball-block with quadtree)
5. Boundary System (canvas edges)
6. Emitter System (spawn balls)

**Physics Loop (60 Hz):**
```
Clear Quadtree → Run Systems → Sync Clients → Track Metrics → Sleep
```

**Optimization:**
- Quadtree spatial partitioning (800x600, depth=5, capacity=10)
- O(n log n) collision detection instead of O(n²)

---

### 5. WebSocket Lifecycle (`websocket_lifecycle.mermaid`)
**Type:** Sequence Diagram  
**Purpose:** Real-time communication protocol

**Message Types:**

**Auth:**
- `auth` → `auth-success`
- Loads canvas + physics state from database

**Cursor Tracking:**
- `cursor` (30 Hz throttled)
- Broadcast to all other clients

**Regular Objects:**
- `object-create`, `object-update`, `object-delete`
- Debounced database saves

**Physics Objects:**
- `physics-spawn-ball`, `physics-spawn-fan`
- Creates ECS entities
- Returns entity IDs

**Physics Sync (60 Hz):**
- `physics-sync` with all entity states
- Clients interpolate for smooth rendering

**AI Commands:**
- `ai-command` → Claude API → `ai-operations`
- Broadcast operations to all clients

---

### 6. Frontend Architecture (`frontend_architecture.mermaid`)
**Type:** Component Diagram  
**Purpose:** Frontend module structure

**Core Modules:**
- `main.js` - Entry point
- `canvas.js` - PixiJS canvas manager (pan/zoom, tools)
- `websocket.js` - WebSocket client with auto-reconnect
- `auth.js` - Authentication UI + Auth0 flow

**Physics Modules:**
- `physics-predictor.js` - Client-side prediction (~100ms)
- `physics-renderer.js` - Visual rendering, particle effects
- `physics-ui.js` - Control panel, tool selection

**Key Features:**
- Ghost ball system for prediction
- State interpolation for smooth transitions
- Conflict resolution (last-write-wins)
- Presence indicators

---

### 7. Data Flow (`data_flow.mermaid`)
**Type:** Flowchart  
**Purpose:** Data flow through the system

**Four Main Flows:**

1. **Regular Canvas Objects**
   - User action → Local update → Callback → WebSocket send
   - Server broadcast → Other clients update
   - Debounced database save

2. **Physics Ball Spawn**
   - Click → Get coords → WebSocket send
   - Server creates ECS entity → Broadcast
   - Clients create ghost ball → Wait for server state

3. **Physics Sync Loop (60 Hz)**
   - Run systems (forces, collision, boundary)
   - Collect entity states → Broadcast
   - Clients interpolate & render → Replace ghosts

4. **AI Commands**
   - User input → WebSocket send → Claude API
   - Generate operations → Broadcast → Execute

**Persistence:**
- Canvas state → `canvas_states`
- Physics settings → `physics_canvas_settings`
- Components → `physics_components`
- Runtime state → `physics_bodies`

---

## Architecture Highlights

### Performance Optimizations
1. **Quadtree Spatial Partitioning** - Reduces collision checks from O(n²) to O(n log n)
2. **Client-Side Prediction** - Instant feedback with ghost balls
3. **State Interpolation** - Smooth rendering at 60 FPS
4. **Debounced Database Saves** - Reduces write load
5. **Throttled Cursor Updates** - 30 Hz instead of every mousemove

### Scalability Features
1. **ECS Architecture** - Data-oriented design for performance
2. **Spatial Partitioning** - Supports 2000+ physics objects
3. **Server-Authoritative** - Prevents client-side cheating
4. **WebSocket Broadcasting** - Efficient multiplayer sync
5. **Database Persistence** - State survives server restarts

### Development Features
1. **Modular Architecture** - Clear separation of concerns
2. **Type Safety** - Strict component definitions
3. **Metrics Tracking** - Frame time, collision counts
4. **AI Integration** - Natural language commands
5. **Dual Auth** - Traditional + OAuth2

## Last Updated
2024-10-18 - Complete physics engine with quadtree optimization
