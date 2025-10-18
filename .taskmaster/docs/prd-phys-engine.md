# Product Requirements Document (PRD): Multiplayer Physics Simulation Feature

## Document Information

- **Title**: Feature PRD: Multiplayer Physics Simulation for CollabCanvas
- **Version**: 3.0 (ECS Architecture Revision)
- **Date**: October 17, 2025
- **Status**: Approved for Implementation
- **Architecture**: cl-fast-ecs (backend ECS + custom physics) + Simple prediction (frontend)

---

## 1. Overview

### 1.1 Purpose

This PRD outlines the requirements for integrating a 2D physics simulation into the existing **CollabCanvas** application using an Entity-Component-System architecture. The new feature will allow multiple users to interact in real-time with a shared physics-based environment containing bouncing balls, force-emitting fans, static obstacles, and eventually emitters, magnets, and gravity wells.

This document details an extension to the current CollabCanvas foundation—its Common Lisp backend (Clack/Woo), PixiJS v8 frontend, and existing WebSocket synchronization layer (`websocket-adapter.lisp`)—to introduce an **authoritative server-side physics simulation with limited client-side ghost prediction**.

### 1.2 Background

The CollabCanvas application already provides a robust platform for real-time, multiplayer visual collaboration with:
- Clack/Woo HTTP server + websocket-driver WebSocket implementation
- PixiJS v8 canvas rendering with viewport culling and performance monitoring
- Auth0 authentication and SQLite persistence
- Room-based WebSocket broadcasting

The core challenge is to extend this platform to support a high-performance physics simulation that feels instantaneous to users (especially for their own actions) while maintaining server authority.

### 1.3 Technical Approach

**Hybrid Prediction + Server Streaming Model with Pure ECS Architecture:**

- **Backend**: cl-fast-ecs framework + custom physics implementation
  - Pure Entity-Component-System architecture
  - Custom Newtonian physics (position, velocity, acceleration, forces)
  - Authoritative 60 Hz simulation
  - Delta-based state broadcasting at 20 Hz
  - Manual collision detection (circle-circle, circle-rectangle)
  - Component-based extensibility for force fields, emitters, magnets

- **Frontend**: Simple custom prediction (no physics engine needed)
  - Lightweight prediction for user-spawned "ghost" objects (~100ms window)
  - Simple position + velocity interpolation
  - Server objects rendered with interpolation (20 Hz → 60 FPS smoothing)
  - Optional: Matter.js for more complex ghost collision prediction

**Key Insight**: We do NOT use a traditional physics engine. Instead, we implement simple custom physics using the ECS pattern, giving us full control and excellent performance (tutorial demonstrates 5000 objects at 60 FPS).

**Why cl-fast-ecs over cl-bodge?**
- **Proven performance**: Tutorial shows 5000 objects at 60 FPS (exceeds our 2000 stretch goal)
- **Perfect architectural fit**: Pure ECS framework, not a physics engine
- **Simplicity**: Implement exactly what we need (position, velocity, forces, basic collisions)
- **Full control**: Custom physics tuned for our multiplayer use case
- **No dependency overhead**: Lightweight, no complex rigid body engine
- **Extensibility**: Adding components (fans, emitters, magnets) is trivial with ECS

### 1.4 Goals and Objectives

**Primary Goal**: Deliver a responsive, synchronized physics simulation within CollabCanvas that feels instantaneous for user actions (<50ms perceived latency) while maintaining server authority.

**Objectives**:
- Support 500 active dynamic bodies at 60 FPS (server and client)
- Stretch goal: 2,000 total bodies (500 active, 1,500 sleeping)
- Leverage existing WebSocket infrastructure (`websocket-adapter.lisp`, `websocket.js`)
- Preserve existing performance optimizations (viewport culling, centralized drag, FPS monitoring)
- Design component-based architecture for easy addition of emitters, magnets, gravity wells
- Maintain <50ms perceived latency for user actions via ghost prediction
- Keep server-reconciled latency under 150ms

### 1.5 Leveraged Features (In Scope)

- **User Authentication**: Existing Auth0 session management identifies users in physics simulations
- **Database**: SQLite extended with physics canvas settings and component storage
- **WebSocket Layer**: Existing `websocket-adapter.lisp` message dispatch system
- **Canvas State**: Existing `canvas-state.lisp` caching integrated with physics state
- **Rendering**: PixiJS v8 `CanvasManager` with viewport culling and performance monitoring

### 1.6 Out of Scope

- Complete replacement of existing drawing functionality (physics is a new mode)
- Advanced user permissions beyond identifying the actor
- 3D physics, soft-body dynamics, or fluid simulations
- Full client-side parallel physics simulation (only ghost prediction)
- Complex rigid body physics (rotation, torque, friction - we use simple physics)

---

## 2. Target Audience and User Personas

### User Persona 1: Simulator Enthusiast
- **Name**: Alex, software engineer using CollabCanvas for diagrams
- **Needs**: Switch canvas to "Physics Mode" to prototype physics interactions
- **Pain Points**: Static diagramming tools; need separate app for dynamic simulations

### User Persona 2: Collaborative Educator
- **Name**: Jordan, physics teacher
- **Needs**: Real-time physics demonstrations where teacher and students can interact simultaneously
- **Pain Points**: Single-player tools or poor synchronization in existing platforms

---

## 3. Features and Requirements

### 3.1 Functional Requirements

#### 3.1.1 Physics Simulation Core (Component-Based ECS)

**Built-in Components (MVP)**:

- **Position Component**
  - `x, y` coordinates (single-float)
  - World space position in pixels

- **Velocity Component** (formerly "speed")
  - `vx, vy` velocity vector (single-float)
  - Units: pixels/second

- **Acceleration Component**
  - `ax, ay` acceleration vector (single-float)
  - Units: pixels/second²
  - Used for gravity, force fields

- **Ball Component** (tag + metadata)
  - `radius` (single-float, default: 10.0)
  - `mass` (single-float, default: 1.0)
  - `restitution` (single-float, default: 0.8 for bounciness)
  - Marks entity as a dynamic ball

- **ForceField Component** (fan/gravity well)
  - `type` (:fan, :gravity-well, :magnet)
  - `strength` (single-float)
  - `radius` (single-float, area of effect)
  - `direction` (optional, for directional fans)
  - Applies forces to balls within radius

- **Block Component** (static obstacle)
  - `width, height` (single-float)
  - Marks entity as static rectangular obstacle

**Post-MVP Components** (extensibility demonstration):

- **Emitter Component**
  - `emit-rate` (balls per second)
  - `emit-direction` (angle in radians)
  - `emit-velocity` (initial velocity magnitude)

- **Magnet Component**
  - `polarity` (:attract or :repel)
  - `strength` (force magnitude)
  - `radius` (area of effect)

**Core ECS Systems**:

1. **apply-forces-system**
   - Processes entities with `force-field` component
   - Applies forces to nearby balls (updates `acceleration`)

2. **apply-acceleration-system**
   - Updates `velocity` based on `acceleration`
   - Formula: `v(t+dt) = v(t) + a(t) * dt`

3. **apply-velocity-system**
   - Updates `position` based on `velocity`
   - Formula: `p(t+dt) = p(t) + v(t) * dt`

4. **collision-system**
   - Detects collisions (circle-circle, circle-rectangle)
   - Resolves collisions with simple elastic response
   - Uses basic distance checks (no spatial partitioning for MVP)

5. **boundary-system**
   - Wraps or bounces entities at world edges
   - Configurable behavior per canvas

#### 3.1.2 Server-Side Physics (Authoritative Simulation)

**Requirements**:
- 60 Hz fixed timestep simulation (16.67ms per frame)
- Independent physics thread per canvas (using `bordeaux-threads`)
- Delta broadcasting at 20 Hz (every 3rd tick)
- Body sleeping: objects with `|velocity| < 0.01` marked as sleeping
- Collision detection: brute-force O(n²) for MVP (spatial partitioning for optimization)

**Physics Formulas** (Newtonian):

Gravity (constant acceleration):
```
a = (0, g)  ; where g = 9.8 pixels/s² (configurable)
```

Force fields (inverse-square or linear):
```
distance = sqrt((x2 - x1)² + (y2 - y1)²)
force_magnitude = strength / distance²  ; or strength / distance for linear
angle = atan2(y2 - y1, x2 - x1)
acceleration = (force_magnitude / mass) * (cos(angle), sin(angle))
```

Elastic collision response (simplified):
```
; Balls collide when distance < r1 + r2
; Separate balls: move along collision normal
; Reverse velocity components along collision normal with restitution
```

#### 3.1.3 Frontend Ghost Prediction (Client-Side)

**Requirements**:
- Ghost objects: user-spawned balls during ~100ms confirmation window
- Simple prediction: `position += velocity * dt`
- No complex collision (optional: use Matter.js for ghost-ghost collision)
- Ghost confirmation: on server response, transition ghost → server object
- Interpolation: server objects smoothed from 20 Hz deltas to 60 FPS rendering

**Ghost Lifecycle**:
1. User clicks to spawn ball → create ghost immediately
2. Send `physics-spawn-ball` message to server
3. Ghost simulates locally (simple velocity-based movement)
4. Server responds with `physics-ball-created` + server ID
5. Remove ghost, start rendering server object with interpolation

#### 3.1.4 Component System Extensibility

**Design Requirements**:
- Base `physics-component` class (if using CLOS) or simple component definitions
- Generic `apply-component-effect` method for force fields
- Easy addition of new component types via `ecs:define-component`
- New systems added via `ecs:define-system`

**Example: Adding a Magnet**:
```lisp
(ecs:define-component magnet
  "Attracts or repels balls within radius."
  (strength 100.0 :type single-float)
  (radius 200.0 :type single-float)
  (polarity :attract :type keyword))  ; :attract or :repel

(ecs:define-system apply-magnet-forces
  (:components-ro (position magnet))
  ; Find all balls within radius, apply force
  ; Based on magnet polarity and strength
  ...)
```

---

## 4. Technical Architecture

### 4.1 Backend Architecture (Common Lisp)

#### 4.1.1 Core Modules

**New Files**:
- `backend/src/physics-ecs.lisp` - ECS setup and entity creation helpers
- `backend/src/physics-components.lisp` - Component definitions
- `backend/src/physics-systems.lisp` - System definitions (forces, velocity, collision)
- `backend/src/physics-loop.lisp` - 60 Hz simulation thread
- `backend/src/physics-delta.lisp` - Delta compression and broadcasting

**Modified Files**:
- `backend/collabcanvas.asd` - Add `:cl-fast-ecs` dependency
- `backend/src/websocket-adapter.lisp` - Add physics message handlers
- `backend/src/canvas-state.lisp` - Optional integration for persistence

#### 4.1.2 ECS Setup and Component Definitions

**Installation** (cl-fast-ecs):
```bash
# Download cl-fast-ecs zip, extract to:
~/quicklisp/local-projects/cl-fast-ecs/
```

**Component Definitions** (`physics-components.lisp`):
```lisp
(ecs:define-component position
  "World space position in pixels."
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))

(ecs:define-component velocity
  "Velocity vector in pixels/second."
  (vx 0.0 :type single-float)
  (vy 0.0 :type single-float))

(ecs:define-component acceleration
  "Acceleration vector in pixels/second²."
  (ax 0.0 :type single-float)
  (ay 0.0 :type single-float))

(ecs:define-component ball
  "Dynamic circular physics body."
  (radius 10.0 :type single-float)
  (mass 1.0 :type single-float)
  (restitution 0.8 :type single-float))

(ecs:define-component force-field
  "Applies forces to nearby balls (fan, gravity well, etc.)."
  (field-type :fan :type keyword)  ; :fan, :gravity-well, :magnet
  (strength 50.0 :type single-float)
  (radius 150.0 :type single-float)
  (direction 0.0 :type single-float))  ; radians, for directional fans

(ecs:define-component block
  "Static rectangular obstacle."
  (width 50.0 :type single-float)
  (height 50.0 :type single-float))

(ecs:define-component sleeping
  "Tag component indicating inactive body (velocity < threshold).")
```

#### 4.1.3 Physics Systems

**apply-forces-system** (`physics-systems.lisp`):
```lisp
(ecs:define-system apply-forces-system
  (:components-ro (position force-field)
   :arguments ((:gravity-y single-float)))
  ;; For each force field, find all balls within radius
  ;; Apply force based on field type and distance
  ;; Update ball's acceleration component
  (ecs:make-system :process-entities
    (lambda (entity)
      (when (has-component-p entity 'ball)
        ;; Calculate distance to force field
        ;; Apply force if within radius
        ;; Update acceleration
        ...))))

(ecs:define-system apply-acceleration-system
  (:components-ro (acceleration)
   :components-rw (velocity)
   :arguments ((:dt single-float)))
  (incf velocity-vx (* dt acceleration-ax))
  (incf velocity-vy (* dt acceleration-ay)))

(ecs:define-system apply-velocity-system
  (:components-ro (velocity)
   :components-rw (position)
   :arguments ((:dt single-float)))
  (incf position-x (* dt velocity-vx))
  (incf position-y (* dt velocity-vy)))

(ecs:define-system collision-system
  (:components-rw (position velocity)
   :components-ro (ball))
  ;; Detect ball-ball collisions (circle-circle)
  ;; Detect ball-block collisions (circle-rectangle)
  ;; Resolve with simple elastic response
  ;; Update positions and velocities
  ...)

(ecs:define-system check-sleeping-system
  (:components-ro (velocity)
   :with ((velocity-threshold)
          :of-type (single-float)
          := 0.01))
  (let ((speed (sqrt (+ (expt velocity-vx 2)
                        (expt velocity-vy 2)))))
    (if (< speed velocity-threshold)
        (unless (has-component-p entity 'sleeping)
          (add-component entity 'sleeping))
        (when (has-component-p entity 'sleeping)
          (remove-component entity 'sleeping)))))
```

#### 4.1.4 Physics Loop (60 Hz Fixed Timestep)

**physics-loop.lisp**:
```lisp
(defvar *physics-canvases* (make-hash-table :test 'equal)
  "Hash table: canvas-id → (thread ecs-storage last-broadcast-tick)")

(defun start-physics-for-canvas (canvas-id gravity-y)
  "Starts 60 Hz physics simulation for canvas."
  (let ((ecs-storage (ecs:make-storage))
        (tick-counter 0)
        (running t))
    (setf (gethash canvas-id *physics-canvases*)
          (list
           (bt:make-thread
            (lambda ()
              (loop while running do
                (let ((dt (/ 1.0 60.0)))  ; 16.67ms fixed timestep
                  ;; Run physics systems
                  (ecs:with-storage ecs-storage
                    (ecs:run-systems :dt dt :gravity-y gravity-y))

                  ;; Broadcast deltas every 3rd tick (20 Hz)
                  (when (zerop (mod tick-counter 3))
                    (broadcast-physics-delta canvas-id ecs-storage))

                  (incf tick-counter)
                  (sleep dt))))
            :name (format nil "Physics-~A" canvas-id))
           ecs-storage
           tick-counter))))

(defun broadcast-physics-delta (canvas-id ecs-storage)
  "Sends delta update to all clients in canvas room."
  (let ((deltas (collect-deltas ecs-storage)))
    (broadcast-to-canvas-room
     canvas-id
     `(:type "physics-update"
       :deltas ,deltas))))
```

#### 4.1.5 WebSocket Message Handlers

**websocket-adapter.lisp** (additions):
```lisp
(defun handle-ws-message (conn-id message)
  (let* ((data (jonathan:parse message))
         (msg-type (gethash "type" data)))
    (cond
      ;; Existing handlers...
      ((string= msg-type "physics-spawn-ball")
       (handle-physics-spawn-ball conn-id data))
      ((string= msg-type "physics-toggle-fan")
       (handle-physics-toggle-fan conn-id data))
      ((string= msg-type "physics-adjust-gravity")
       (handle-physics-adjust-gravity conn-id data))
      ...)))

(defun handle-physics-spawn-ball (conn-id data)
  "Creates a new ball entity in canvas physics simulation."
  (let* ((canvas-id (get-canvas-for-connection conn-id))
         (x (gethash "x" data))
         (y (gethash "y" data))
         (ghost-id (gethash "ghostId" data))
         (ecs-storage (get-canvas-ecs-storage canvas-id)))

    ;; Create ball entity
    (ecs:with-storage ecs-storage
      (let ((entity-id (ecs:make-object
                        `((:position :x ,x :y ,y)
                          (:velocity :vx 0.0 :vy 0.0)
                          (:acceleration :ax 0.0 :ay 0.0)
                          (:ball :radius 10.0 :mass 1.0)))))

        ;; Confirm to client
        (send-to-connection
         conn-id
         `(:type "physics-ball-created"
           :ghostId ,ghost-id
           :serverId ,entity-id
           :x ,x :y ,y))))))
```

### 4.2 Frontend Architecture (JavaScript/PixiJS)

#### 4.2.1 Simple Ghost Prediction (No Physics Engine)

**frontend/src/physics-predictor.js**:
```javascript
export class SimplePhysicsPredictor {
  constructor() {
    this.ghosts = new Map();  // ghostId → {x, y, vx, vy, radius}
  }

  spawnGhost(ghostId, x, y, vx = 0, vy = 0, radius = 10) {
    this.ghosts.set(ghostId, {x, y, vx, vy, radius});
    return {x, y, vx, vy, radius};
  }

  step(dt) {
    // Simple velocity-based prediction
    for (const [id, ghost] of this.ghosts.entries()) {
      ghost.x += ghost.vx * dt;
      ghost.y += ghost.vy * dt;

      // Optional: simple gravity
      ghost.vy += 9.8 * dt;

      // Optional: boundary bounce
      if (ghost.y > 600) {
        ghost.y = 600;
        ghost.vy *= -0.8;  // restitution
      }
    }
  }

  removeGhost(ghostId) {
    this.ghosts.delete(ghostId);
  }

  getGhosts() {
    return this.ghosts;
  }
}
```

#### 4.2.2 Server Object Interpolation

**frontend/src/physics-renderer.js**:
```javascript
export class PhysicsRenderer {
  constructor(pixiApp) {
    this.app = pixiApp;
    this.serverBalls = new Map();  // serverId → {sprite, serverX, serverY, currentX, currentY}
    this.predictor = new SimplePhysicsPredictor();
  }

  onPhysicsUpdate(deltas) {
    // Update server ball states
    for (const delta of deltas) {
      if (!this.serverBalls.has(delta.id)) {
        this.createServerBall(delta.id, delta.x, delta.y);
      }

      const ball = this.serverBalls.get(delta.id);
      ball.serverX = delta.x;
      ball.serverY = delta.y;
    }
  }

  render(dt) {
    // Interpolate server balls (20 Hz → 60 FPS)
    for (const [id, ball] of this.serverBalls.entries()) {
      const alpha = 0.3;  // interpolation factor
      ball.currentX += (ball.serverX - ball.currentX) * alpha;
      ball.currentY += (ball.serverY - ball.currentY) * alpha;
      ball.sprite.position.set(ball.currentX, ball.currentY);
    }

    // Render ghost balls
    for (const [id, ghost] of this.predictor.getGhosts().entries()) {
      // Draw semi-transparent ghost
      this.renderGhost(ghost);
    }
  }
}
```

### 4.3 Database Schema

**backend/db/physics-schema.sql**:
```sql
CREATE TABLE IF NOT EXISTS physics_canvas_settings (
  canvas_id TEXT PRIMARY KEY,
  gravity_x REAL NOT NULL DEFAULT 0.0,
  gravity_y REAL NOT NULL DEFAULT 9.8,
  simulation_rate INTEGER NOT NULL DEFAULT 60,
  max_objects INTEGER NOT NULL DEFAULT 2000,
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL,
  FOREIGN KEY (canvas_id) REFERENCES canvases(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS physics_components (
  id TEXT PRIMARY KEY,
  canvas_id TEXT NOT NULL,
  component_type TEXT NOT NULL, -- 'ball', 'force-field', 'block', 'emitter', 'magnet'
  properties_json TEXT NOT NULL,  -- JSON blob with component-specific properties
  created_by INTEGER,
  created_at INTEGER NOT NULL,
  FOREIGN KEY (canvas_id) REFERENCES canvases(id) ON DELETE CASCADE,
  FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE SET NULL
);

CREATE INDEX idx_physics_components_canvas ON physics_components(canvas_id);
CREATE INDEX idx_physics_components_type ON physics_components(component_type);
CREATE INDEX idx_physics_components_created ON physics_components(created_at);

-- Component type validation lookup table
CREATE TABLE IF NOT EXISTS physics_component_types (
  type_name TEXT PRIMARY KEY,
  description TEXT
);

INSERT OR IGNORE INTO physics_component_types (type_name, description) VALUES
  ('ball', 'Dynamic circular body'),
  ('force-field', 'Applies forces (fan, gravity well, magnet)'),
  ('block', 'Static rectangular obstacle'),
  ('emitter', 'Spawns balls at intervals'),
  ('magnet', 'Attracts or repels nearby balls');
```

---

## 5. Performance Targets

### 5.1 Server Performance (60 Hz Simulation)

**Tier 1 (MVP):**
- 500 active balls at 60 Hz (< 16ms per frame)
- Simple O(n²) collision detection
- 20 Hz delta broadcasting

**Tier 2 (Stretch):**
- 2,000 total bodies (500 active, 1,500 sleeping)
- Spatial partitioning for collision (quadtree or grid)
- Delta compression (<280 KB/sec per client)

**Proven Benchmark** (from cl-fast-ecs tutorial):
- 5,000 objects at 60 FPS demonstrated
- Trivial machine code for movement system (210 bytes, 17 instructions)

### 5.2 Client Performance (60 FPS Rendering)

- Viewport culling (existing CanvasManager feature)
- Interpolation from 20 Hz deltas to 60 FPS rendering
- Ghost prediction: <5ms per frame
- Total frame budget: <16ms (including rendering)

### 5.3 Network Performance

- Delta broadcasting: 20 Hz (every 3rd physics tick)
- Only send changed properties (position, velocity)
- Skip sleeping objects (velocity < 0.01)
- Target: <280 KB/sec per client with 500 balls

---

## 6. Implementation Phases

### Phase 0: Library Validation & Architecture Proof
**Goal**: Validate cl-fast-ecs works as expected

**Tasks**:
1. Install cl-fast-ecs from zip to `~/quicklisp/local-projects/`
2. Create standalone test: spawn 500 balls with gravity
3. Benchmark performance (target: <16ms per frame)
4. Verify ECS component/system patterns work
5. Create database schema SQL files

**Success Criteria**:
- cl-fast-ecs loads successfully
- 500 balls simulate at 60 Hz with <16ms frame time
- Database schema validated

### Phase 1: Backend Physics Core
**Goal**: Implement authoritative server-side physics

**Tasks**:
1. Create ECS storage per canvas
2. Define components (position, velocity, acceleration, ball, force-field, block)
3. Implement systems (forces, acceleration, velocity, collision)
4. Create 60 Hz background thread
5. Add WebSocket message handlers
6. Execute database schema

**Success Criteria**:
- Backend simulates 500 balls at 60 Hz
- Force fields apply forces correctly
- Collisions resolve properly
- State persists across restarts

### Phase 2: Frontend Integration
**Goal**: Create responsive client experience

**Tasks**:
1. Simple ghost predictor (no physics engine)
2. Extend CanvasManager for physics rendering
3. WebSocket message handling
4. UI controls (spawn ball, toggle fan, gravity slider)

**Success Criteria**:
- Ghost balls appear instantly (<5ms)
- Server objects interpolate smoothly
- 60 FPS maintained with 500 balls
- UI responsive

### Phase 3: Multiplayer Sync & Polish
**Goal**: Optimize and test

**Tasks**:
1. Delta compression
2. Latency/bandwidth monitoring
3. Determinism testing
4. Load testing (2 clients, 500 balls)
5. Error handling (disconnections)

**Success Criteria**:
- Bandwidth <280 KB/sec per client
- Deterministic simulation
- Smooth reconnection

### Phase 4: Component System & Extensibility
**Goal**: Prove extensibility

**Tasks**:
1. Gravity adjustment UI
2. Implement emitter OR magnet
3. Developer documentation
4. Performance profiling

**Success Criteria**:
- Gravity slider works
- Post-MVP component functional
- Documentation enables <1 day component addition

---

## 7. Risks and Mitigation

### Risk 1: Custom Physics Complexity
**Mitigation**: Start with simple physics (position + velocity). Tutorial proves this works for 5000 objects.

### Risk 2: Collision Detection Performance
**Mitigation**: O(n²) acceptable for 500 balls. Add spatial partitioning if needed.

### Risk 3: ECS Learning Curve
**Mitigation**: Tutorial provides clear examples. Framework is simple (3 main macros).

---

## 8. Success Metrics

- 500 active balls at 60 Hz (server and client)
- <50ms perceived latency for user actions
- <150ms server-reconciled latency
- >90% client satisfaction (from testing)
- Developer can add new component in <4 hours

---

## 9. Appendix

### 9.1 References

- **cl-fast-ecs Tutorial**: Gamedev in Lisp, Part 1
- **ECS Pattern**: https://github.com/SanderMertens/ecs-faq
- **CollabCanvas Existing Code**: backend/src/websocket-adapter.lisp, frontend/src/canvas.js

### 9.2 Glossary

- **ECS**: Entity-Component-System architecture pattern
- **Ghost**: Client-predicted object during server confirmation window
- **Delta**: Incremental state update (only changed properties)
- **Sleeping**: Inactive object with velocity below threshold
