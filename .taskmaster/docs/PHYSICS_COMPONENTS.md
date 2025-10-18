# Physics Components Developer Guide

**CollabCanvas Physics Engine - Component API Documentation**

This guide explains how to add new physics component types to the CollabCanvas physics engine. After reading this document, you should be able to implement a new component (like magnets, springs, or custom force fields) in under one day.

---

## Table of Contents

1. [Introduction to the ECS Architecture](#1-introduction-to-the-ecs-architecture)
2. [How to Add New Component Types](#2-how-to-add-new-component-types)
3. [Component Lifecycle](#3-component-lifecycle)
4. [Frontend Rendering Guide](#4-frontend-rendering-guide)
5. [Complete Example: Emitter Component](#5-complete-example-emitter-component)
6. [Best Practices](#6-best-practices)
7. [API Reference](#7-api-reference)

---

## 1. Introduction to the ECS Architecture

### What is ECS?

CollabCanvas uses **Entity-Component-System (ECS)** architecture for physics simulation:

- **Entity**: A unique ID (integer) representing a physics object
- **Component**: Data attached to an entity (position, velocity, ball, force-field, etc.)
- **System**: Logic that processes entities with specific component combinations

### Why ECS?

ECS provides several benefits for physics engines:

1. **Performance**: Components are stored in contiguous arrays (cache-friendly)
2. **Extensibility**: New components can be added without modifying existing code
3. **Flexibility**: Any combination of components defines object behavior
4. **Scalability**: Optimized for 500+ entities at 60 Hz

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│ Per-Canvas Physics World (*physics-canvases* hash table)    │
│                                                              │
│  Canvas "abc123"                                             │
│  ├─ ECS Storage (cl-fast-ecs:storage)                       │
│  │  ├─ Entity 1: [position, velocity, acceleration, ball]   │
│  │  ├─ Entity 2: [position, block]                          │
│  │  └─ Entity 3: [position, force-field]                    │
│  ├─ Gravity: (0.0, 9.8)                                      │
│  └─ Simulation Rate: 60 Hz                                   │
│                                                              │
│  Canvas "xyz789"                                             │
│  └─ ... (separate physics world)                            │
└─────────────────────────────────────────────────────────────┘

Systems run every frame (60 Hz):
  1. apply-forces-system       - Force fields affect balls
  2. apply-acceleration-system - Update velocity from acceleration
  3. apply-velocity-system     - Update position from velocity
  4. collision-system          - Detect and resolve collisions
  5. check-sleeping-system     - Optimize inactive entities
```

### Component vs System vs Entity

**Entity** (created with `make-object`):
```lisp
;; Entity is just an integer ID returned by make-object
(defparameter *ball-entity*
  (make-object
   '((:position :x 100.0 :y 100.0)
     (:velocity :vx 50.0 :vy -30.0)
     (:ball :radius 15.0 :mass 1.0))))
;; => Returns entity ID (e.g., 42)
```

**Component** (defined with `define-component`):
```lisp
;; Component is data attached to entities
(define-component ball
  "Dynamic circular physics body"
  (radius 10.0 :type single-float)
  (mass 1.0 :type single-float)
  (restitution 0.8 :type single-float))
```

**System** (defined with `define-system`):
```lisp
;; System is logic that processes entities with specific components
(define-system move-balls-system
  (:components-ro (velocity)
   :components-rw (position)
   :arguments ((:dt single-float)))

  ;; Update position from velocity
  (incf position-x (* velocity-vx dt))
  (incf position-y (* velocity-vy dt)))
```

---

## 2. How to Add New Component Types

### Step-by-Step Guide

#### Step 1: Define the Component

Add your component definition to `backend/src/physics-components.lisp`:

```lisp
(define-component magnet
  "Magnetic force component that attracts or repels balls.

   Slots:
   - polarity: :attract or :repel
   - strength: Force magnitude (positive float)
   - radius: Area of influence in pixels"
  (polarity :attract :type keyword)
  (strength 200.0 :type single-float)
  (radius 150.0 :type single-float))
```

**Component Naming Conventions:**
- Use lowercase names: `magnet`, `force-field`, `spring`
- Use descriptive slot names: `strength`, `radius`, `polarity`
- Always add type declarations: `:type single-float`, `:type keyword`
- Include comprehensive docstrings

#### Step 2: Create or Modify a System

Add system logic to `backend/src/physics-systems.lisp`:

```lisp
(define-system apply-magnet-system
  (:components-ro (magnet position-magnet)
   :with (position-ball velocity ball)
   :arguments ((:dt single-float)))

  "Apply magnetic forces to nearby balls.

   Algorithm:
   1. Iterate through all magnet entities (outer loop)
   2. For each magnet, find balls within radius (inner loop)
   3. Calculate magnetic force based on polarity
   4. Apply force to ball acceleration"

  ;; Skip sleeping balls
  (unless (cl-fast-ecs:has-component (current-entity) 'sleeping)

    ;; Calculate distance from magnet to ball
    (let* ((dx (- position-ball-x position-magnet-x))
           (dy (- position-ball-y position-magnet-y))
           (dist-sq (+ (* dx dx) (* dy dy)))
           (dist (sqrt dist-sq))
           (radius magnet-radius))

      ;; Check if ball is within magnet radius
      (when (< dist radius)

        ;; Calculate force with falloff
        (let* ((falloff (- 1.0 (/ dist radius)))
               (force-magnitude (* magnet-strength falloff))
               (mass ball-mass))

          ;; Avoid division by zero
          (when (> dist 0.01)

            ;; Calculate force direction based on polarity
            (let* ((direction (if (eq magnet-polarity :attract) -1.0 1.0))
                   (nx (/ (* direction dx) dist))
                   (ny (/ (* direction dy) dist))
                   (fx (* force-magnitude nx))
                   (fy (* force-magnitude ny)))

              ;; Apply force to ball: F = ma => a = F/m
              (incf (cl-fast-ecs:get-component-value
                     (current-entity) 'acceleration 'ax)
                    (/ fx mass))
              (incf (cl-fast-ecs:get-component-value
                     (current-entity) 'acceleration 'ay)
                    (/ fy mass)))))))))
```

**System Tips:**
- Use `:components-ro` for read-only components (optimization)
- Use `:components-rw` for components you'll modify
- Use `:with` for inner loop queries (find other entities)
- Always check `sleeping` component to skip inactive entities
- Add `:arguments` for system parameters (dt, gravity, etc.)

#### Step 3: Integrate System into Physics Loop

Add your system to the execution order in `backend/src/physics-loop.lisp`:

```lisp
(defun run-physics-step (storage dt gravity-x gravity-y)
  "Run one physics simulation step."
  (with-storage (storage)
    ;; 1. Apply all force systems
    (run-system 'apply-forces-system :dt dt)
    (run-system 'apply-magnet-system :dt dt)  ; <-- ADD HERE

    ;; 2. Update velocity from acceleration
    (run-system 'apply-acceleration-system
                :dt dt :gravity-x gravity-x :gravity-y gravity-y)

    ;; 3. Update position from velocity
    (run-system 'apply-velocity-system :dt dt)

    ;; 4. Resolve collisions
    (run-system 'collision-system :dt dt)

    ;; 5. Check sleeping state
    (run-system 'check-sleeping-system)))
```

**System Execution Order Matters:**
1. **Forces first**: All force systems (magnets, force-fields, springs)
2. **Acceleration**: Update velocity from acceleration + gravity
3. **Velocity**: Update position from velocity
4. **Collisions**: Detect and resolve overlaps
5. **Sleep**: Mark inactive entities for optimization

#### Step 4: Add WebSocket Message Handler

Add message handling to `backend/src/websocket-adapter.lisp`:

```lisp
;; In handle-physics-message function, add new case:

((string= msg-type "physics-create-magnet")
 (let* ((x (gethash "x" data))
        (y (gethash "y" data))
        (polarity (intern (string-upcase (gethash "polarity" data "attract"))
                          :keyword))
        (strength (coerce (gethash "strength" data 200.0) 'single-float))
        (radius (coerce (gethash "radius" data 150.0) 'single-float))
        (storage (get-canvas-ecs-storage canvas-id)))

   (when storage
     (with-storage (storage)
       ;; Create magnet entity
       (let ((magnet-id (make-object
                         `((:position :x ,(coerce x 'single-float)
                                      :y ,(coerce y 'single-float))
                           (:magnet :polarity ,polarity
                                   :strength ,strength
                                   :radius ,radius)))))

         ;; Save to database
         (save-physics-component canvas-id magnet-id "magnet"
                                (jonathan:to-json data))

         ;; Broadcast to all clients
         (broadcast-to-canvas canvas-id
           (jonathan:to-json
            (list :type "physics-magnet-created"
                  :magnet-id magnet-id
                  :x x :y y
                  :polarity (string-downcase (symbol-name polarity))
                  :strength strength
                  :radius radius))))))))
```

#### Step 5: Update Package Exports (if needed)

If adding new public functions, export them in `backend/src/package.lisp`:

```lisp
(:export
 ;; ... existing exports ...

 ;; Magnet operations
 #:create-magnet
 #:update-magnet-strength
 #:toggle-magnet-polarity)
```

### Type Declarations for Performance

**Always use type declarations for numeric slots:**

```lisp
;; GOOD: Type-declared (enables compiler optimizations)
(define-component particle
  "Fast particle with optimized storage"
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (mass 1.0 :type single-float))

;; BAD: No type declarations (slow, boxing/unboxing)
(define-component particle
  "Slow particle with generic storage"
  (x 0.0)
  (y 0.0)
  (mass 1.0))
```

**Why single-float vs double-float?**
- `single-float`: 4 bytes, sufficient precision for pixel coordinates (< 0.01px error)
- `double-float`: 8 bytes, overkill for games/graphics
- SIMD optimizations work better with single-float arrays

---

## 3. Component Lifecycle

### Creation: Entity Creation with Components

Entities are created using `make-object` with component specifications:

```lisp
;; Create entity with multiple components
(with-storage (storage)
  (let ((entity-id (make-object
                    '((:position :x 100.0 :y 200.0)
                      (:velocity :vx 50.0 :vy -30.0)
                      (:acceleration :ax 0.0 :ay 9.8)
                      (:ball :radius 20.0 :mass 1.5 :restitution 0.9)))))

    ;; entity-id is an integer (e.g., 42)
    (format t "Created entity: ~A~%" entity-id)))
```

**Component Spec Format:**
```lisp
'((:component-name :slot1 value1 :slot2 value2 ...)
  (:another-component :slot-a val-a :slot-b val-b))
```

### Force Application: How Systems Interact with Components

Systems read and modify component data using generated accessor variables:

```lisp
(define-system example-system
  (:components-ro (velocity)      ; Read-only: velocity-vx, velocity-vy
   :components-rw (position)      ; Read-write: position-x, position-y
   :arguments ((:dt single-float)))

  ;; Accessor variables are auto-generated:
  ;; - velocity-vx (read-only)
  ;; - velocity-vy (read-only)
  ;; - position-x (read-write)
  ;; - position-y (read-write)

  ;; Read velocity, update position
  (incf position-x (* velocity-vx dt))
  (incf position-y (* velocity-vy dt)))
```

**Accessor Naming Convention:**
- Format: `<component-name>-<slot-name>`
- Example: `ball-radius`, `force-field-strength`, `magnet-polarity`
- Read-only components can only be read
- Read-write components can be modified with `setf` or `incf`

### Destruction: Entity Cleanup

Entities can be destroyed using `delete-entity`:

```lisp
(with-storage (storage)
  ;; Delete entity and all its components
  (cl-fast-ecs:delete-entity entity-id)

  ;; Or delete specific component from entity
  (cl-fast-ecs:delete-component entity-id 'velocity))
```

**When to Delete Entities:**
- Ball goes off-screen (boundary checking)
- User removes object via UI
- Canvas is destroyed (cleanup all entities)
- Component lifetime expires (e.g., temporary particles)

**Cleanup on Canvas Destruction:**
```lisp
;; In destroy-canvas-physics:
(destroy-canvas-physics canvas-id)
;; This removes entire ECS storage (all entities auto-cleaned)
```

---

## 4. Frontend Rendering Guide

### How to Visualize New Components

#### Step 1: Add Rendering Logic to PhysicsRenderer

Edit `frontend/src/physics-renderer.js`:

```javascript
class PhysicsRenderer extends CanvasManager {
  constructor(app) {
    super(app);

    // Add storage for your component
    this.magnets = new Map(); // magnetId → PIXI.Graphics
  }

  /**
   * Create magnet visualization
   * @param {number} magnetId - Entity ID from server
   * @param {number} x - Position X
   * @param {number} y - Position Y
   * @param {string} polarity - 'attract' or 'repel'
   * @param {number} strength - Force magnitude
   * @param {number} radius - Area of influence
   */
  createMagnet(magnetId, x, y, polarity, strength, radius) {
    // Remove existing if duplicate
    if (this.magnets.has(magnetId)) {
      this.removeMagnet(magnetId);
    }

    const container = new PIXI.Container();
    container.x = x;
    container.y = y;

    // Draw influence radius (semi-transparent circle)
    const radiusCircle = new PIXI.Graphics();
    const color = polarity === 'attract' ? 0x00FF00 : 0xFF0000;
    radiusCircle.circle(0, 0, radius).fill({
      color: color,
      alpha: 0.1
    });
    radiusCircle.circle(0, 0, radius).stroke({
      width: 2,
      color: color,
      alpha: 0.5
    });
    container.addChild(radiusCircle);

    // Draw magnet icon (horseshoe)
    const icon = new PIXI.Graphics();
    icon.circle(0, 0, 20).fill({ color: color, alpha: 0.8 });

    // Add polarity label
    const label = new PIXI.Text(polarity === 'attract' ? 'N' : 'S', {
      fontSize: 16,
      fill: 0xFFFFFF,
      fontWeight: 'bold'
    });
    label.anchor.set(0.5);
    icon.addChild(label);

    container.addChild(icon);

    // Make interactive (for dragging/editing)
    container.interactive = true;
    container.cursor = 'pointer';

    // Store component data
    container.userData = {
      type: 'magnet',
      magnetId,
      polarity,
      strength,
      radius
    };

    // Add to stage
    this.viewport.addChild(container);
    this.magnets.set(magnetId, container);

    return container;
  }

  removeMagnet(magnetId) {
    const magnet = this.magnets.get(magnetId);
    if (magnet) {
      this.viewport.removeChild(magnet);
      magnet.destroy({ children: true });
      this.magnets.delete(magnetId);
    }
  }

  updateMagnetPosition(magnetId, x, y) {
    const magnet = this.magnets.get(magnetId);
    if (magnet) {
      magnet.x = x;
      magnet.y = y;
    }
  }
}
```

#### Step 2: Add WebSocket Message Handlers

Edit `frontend/src/websocket.js`:

```javascript
class WebSocketClient {
  handleMessage(message) {
    const data = JSON.parse(message.data);

    switch (data.type) {
      case 'physics-magnet-created':
        this.physicsRenderer.createMagnet(
          data.magnetId,
          data.x,
          data.y,
          data.polarity,
          data.strength,
          data.radius
        );
        break;

      case 'physics-magnet-updated':
        this.physicsRenderer.updateMagnetPosition(
          data.magnetId,
          data.x,
          data.y
        );
        break;

      case 'physics-magnet-deleted':
        this.physicsRenderer.removeMagnet(data.magnetId);
        break;
    }
  }
}
```

#### Step 3: Add UI Controls

Edit `frontend/src/physics-ui.js`:

```javascript
class PhysicsUI {
  createControlPanel() {
    const panel = document.createElement('div');
    panel.innerHTML = `
      <!-- Magnet Controls -->
      <div class="physics-section">
        <h4>Magnet Tool</h4>
        <button id="create-magnet-btn" class="physics-btn">
          Create Magnet
        </button>
        <div class="magnet-controls">
          <label>
            Polarity:
            <select id="magnet-polarity">
              <option value="attract">Attract (North)</option>
              <option value="repel">Repel (South)</option>
            </select>
          </label>
          <label>
            Strength:
            <input type="range" id="magnet-strength"
                   min="0" max="500" value="200">
            <span id="magnet-strength-value">200</span>
          </label>
          <label>
            Radius:
            <input type="range" id="magnet-radius"
                   min="50" max="300" value="150">
            <span id="magnet-radius-value">150</span>
          </label>
        </div>
      </div>
    `;

    document.body.appendChild(panel);
    this.setupMagnetControls();
  }

  setupMagnetControls() {
    const createBtn = document.getElementById('create-magnet-btn');
    const polaritySelect = document.getElementById('magnet-polarity');
    const strengthSlider = document.getElementById('magnet-strength');
    const radiusSlider = document.getElementById('magnet-radius');

    // Create magnet on canvas click
    createBtn.addEventListener('click', () => {
      this.magnetCreateMode = true;
      createBtn.textContent = 'Click to Place Magnet';
      createBtn.classList.add('active');
    });

    // Update strength value display
    strengthSlider.addEventListener('input', (e) => {
      document.getElementById('magnet-strength-value').textContent =
        e.target.value;
    });

    // Update radius value display
    radiusSlider.addEventListener('input', (e) => {
      document.getElementById('magnet-radius-value').textContent =
        e.target.value;
    });

    // Handle canvas click to create magnet
    this.renderer.app.view.addEventListener('click', (e) => {
      if (!this.magnetCreateMode) return;

      const rect = this.renderer.app.view.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;

      // Convert to world coordinates
      const worldPos = this.renderer.viewport.toWorld(x, y);

      // Send create message to server
      this.ws.send(JSON.stringify({
        type: 'physics-create-magnet',
        canvasId: this.canvasId,
        x: worldPos.x,
        y: worldPos.y,
        polarity: polaritySelect.value,
        strength: parseFloat(strengthSlider.value),
        radius: parseFloat(radiusSlider.value)
      }));

      // Reset mode
      this.magnetCreateMode = false;
      createBtn.textContent = 'Create Magnet';
      createBtn.classList.remove('active');
    });
  }
}
```

### Performance Considerations

**Rendering Optimization Tips:**

1. **Use Object Pooling**: Reuse PIXI.Graphics objects instead of creating new ones
2. **Minimize Draw Calls**: Batch similar objects into containers
3. **Cull Off-Screen Objects**: Don't render objects outside viewport
4. **Use Lower Precision**: 60 FPS is enough, don't render at higher rates
5. **Throttle Updates**: Interpolate server updates (20 Hz → 60 FPS)

**Example: Object Pooling**
```javascript
class MagnetPool {
  constructor(initialSize = 10) {
    this.pool = [];
    for (let i = 0; i < initialSize; i++) {
      this.pool.push(this.createMagnetGraphics());
    }
  }

  acquire() {
    return this.pool.pop() || this.createMagnetGraphics();
  }

  release(magnet) {
    magnet.visible = false;
    this.pool.push(magnet);
  }

  createMagnetGraphics() {
    const graphics = new PIXI.Graphics();
    // ... setup graphics ...
    return graphics;
  }
}
```

### Animation Patterns

**Pulsating Effect** (for force fields):
```javascript
updateMagnet(magnetId, deltaTime) {
  const magnet = this.magnets.get(magnetId);
  if (!magnet) return;

  // Pulsate influence radius
  const time = performance.now() / 1000;
  const pulse = Math.sin(time * 2) * 0.1 + 1.0; // 0.9 to 1.1

  const radius = magnet.userData.radius * pulse;
  magnet.children[0].clear();
  magnet.children[0].circle(0, 0, radius).fill({
    color: 0x00FF00,
    alpha: 0.1
  });
}
```

**Fade In/Out** (for temporary particles):
```javascript
fadeOutParticle(particleId, duration = 1000) {
  const particle = this.particles.get(particleId);
  if (!particle) return;

  const startAlpha = particle.alpha;
  const startTime = performance.now();

  const fadeLoop = () => {
    const elapsed = performance.now() - startTime;
    const progress = Math.min(elapsed / duration, 1.0);

    particle.alpha = startAlpha * (1.0 - progress);

    if (progress < 1.0) {
      requestAnimationFrame(fadeLoop);
    } else {
      this.removeParticle(particleId);
    }
  };

  fadeLoop();
}
```

---

## 5. Complete Example: Emitter Component

This section provides a complete walkthrough of implementing an **emitter** component that spawns balls at regular intervals.

### 5.1 Backend: Component Definition

**File:** `backend/src/physics-components.lisp`

```lisp
(define-component emitter
  "Particle emitter that spawns balls at regular intervals.

   Slots:
   - rate: Balls per second (emission rate)
   - direction: Emission angle in radians (0 = right, π/2 = down)
   - velocity: Initial ball velocity magnitude in pixels/second
   - ball-radius: Radius of spawned balls
   - last-emit-time: Internal timestamp for rate limiting
   - enabled: Whether emitter is active"
  (rate 2.0 :type single-float)              ; 2 balls/second
  (direction 0.0 :type single-float)         ; 0 radians = right
  (velocity 100.0 :type single-float)        ; 100 px/s
  (ball-radius 10.0 :type single-float)      ; 10px radius balls
  (last-emit-time 0.0 :type single-float)    ; Internal state
  (enabled t :type boolean))                  ; Active by default
```

### 5.2 Backend: System Implementation

**File:** `backend/src/physics-systems.lisp`

```lisp
(define-system emitter-system
  (:components-rw (emitter position)
   :arguments ((:current-time single-float)))

  "Spawn balls from emitter entities at specified rate.

   Algorithm:
   1. Check if enough time has elapsed since last emission
   2. If yes, spawn a new ball entity with calculated velocity
   3. Update last-emit-time to current time"

  ;; Only emit if enabled
  (when emitter-enabled

    ;; Calculate time since last emission
    (let* ((time-since-emit (- current-time emitter-last-emit-time))
           (emit-interval (/ 1.0 emitter-rate)))  ; seconds per ball

      ;; Check if it's time to emit
      (when (>= time-since-emit emit-interval)

        ;; Calculate emission velocity components
        (let* ((dir emitter-direction)
               (vel emitter-velocity)
               (vx (* vel (cos dir)))
               (vy (* vel (sin dir))))

          ;; Create new ball entity
          (make-object
           `((:position :x ,position-x :y ,position-y)
             (:velocity :vx ,vx :vy ,vy)
             (:acceleration :ax 0.0 :ay 0.0)
             (:ball :radius ,emitter-ball-radius
                   :mass 1.0
                   :restitution 0.8)))

          ;; Update last emit time
          (setf emitter-last-emit-time current-time))))))
```

**Integration into physics loop** (`backend/src/physics-loop.lisp`):

```lisp
(defun run-physics-step (storage dt gravity-x gravity-y)
  "Run one physics simulation step."
  (with-storage (storage)
    ;; 0. Emitters spawn balls BEFORE forces
    (run-system 'emitter-system :current-time (get-internal-real-time))

    ;; 1. Apply forces
    (run-system 'apply-forces-system :dt dt)

    ;; 2-5. ... rest of systems ...
    ))
```

### 5.3 Backend: WebSocket Message Handlers

**File:** `backend/src/websocket-adapter.lisp`

```lisp
;; CREATE EMITTER
((string= msg-type "physics-create-emitter")
 (let* ((x (gethash "x" data))
        (y (gethash "y" data))
        (rate (coerce (gethash "rate" data 2.0) 'single-float))
        (direction (coerce (gethash "direction" data 0.0) 'single-float))
        (velocity (coerce (gethash "velocity" data 100.0) 'single-float))
        (ball-radius (coerce (gethash "ballRadius" data 10.0) 'single-float))
        (storage (get-canvas-ecs-storage canvas-id)))

   (when storage
     (with-storage (storage)
       ;; Create emitter entity
       (let ((emitter-id (make-object
                          `((:position :x ,(coerce x 'single-float)
                                       :y ,(coerce y 'single-float))
                            (:emitter :rate ,rate
                                     :direction ,direction
                                     :velocity ,velocity
                                     :ball-radius ,ball-radius
                                     :last-emit-time 0.0
                                     :enabled t)))))

         ;; Save to database
         (save-physics-component canvas-id emitter-id "emitter"
                                (jonathan:to-json data))

         ;; Broadcast creation to all clients
         (broadcast-to-canvas canvas-id
           (jonathan:to-json
            (list :type "physics-emitter-created"
                  :emitter-id emitter-id
                  :x x :y y
                  :rate rate
                  :direction direction
                  :velocity velocity
                  :ball-radius ball-radius))))))))

;; UPDATE EMITTER
((string= msg-type "physics-update-emitter")
 (let* ((emitter-id (gethash "emitterId" data))
        (rate (gethash "rate" data))
        (direction (gethash "direction" data))
        (velocity (gethash "velocity" data))
        (enabled (gethash "enabled" data))
        (storage (get-canvas-ecs-storage canvas-id)))

   (when storage
     (with-storage (storage)
       ;; Update emitter component values
       (when rate
         (setf (cl-fast-ecs:get-component-value emitter-id 'emitter 'rate)
               (coerce rate 'single-float)))
       (when direction
         (setf (cl-fast-ecs:get-component-value emitter-id 'emitter 'direction)
               (coerce direction 'single-float)))
       (when velocity
         (setf (cl-fast-ecs:get-component-value emitter-id 'emitter 'velocity)
               (coerce velocity 'single-float)))
       (when enabled
         (setf (cl-fast-ecs:get-component-value emitter-id 'emitter 'enabled)
               enabled))

       ;; Broadcast update
       (broadcast-to-canvas canvas-id
         (jonathan:to-json
          (list :type "physics-emitter-updated"
                :emitter-id emitter-id
                :rate rate
                :direction direction
                :velocity velocity
                :enabled enabled)))))))

;; DELETE EMITTER
((string= msg-type "physics-delete-emitter")
 (let* ((emitter-id (gethash "emitterId" data))
        (storage (get-canvas-ecs-storage canvas-id)))

   (when storage
     (with-storage (storage)
       ;; Delete entity and all components
       (cl-fast-ecs:delete-entity emitter-id)

       ;; Delete from database
       (delete-physics-component canvas-id emitter-id)

       ;; Broadcast deletion
       (broadcast-to-canvas canvas-id
         (jonathan:to-json
          (list :type "physics-emitter-deleted"
                :emitter-id emitter-id)))))))
```

### 5.4 Frontend: Visualization

**File:** `frontend/src/physics-renderer.js`

```javascript
class PhysicsRenderer extends CanvasManager {
  constructor(app) {
    super(app);
    this.emitters = new Map(); // emitterId → PIXI.Graphics
  }

  /**
   * Create emitter visualization
   */
  createEmitter(emitterId, x, y, rate, direction, velocity, ballRadius) {
    if (this.emitters.has(emitterId)) {
      this.removeEmitter(emitterId);
    }

    const container = new PIXI.Container();
    container.x = x;
    container.y = y;

    // Draw emitter body (square)
    const body = new PIXI.Graphics();
    body.rect(-15, -15, 30, 30).fill({ color: 0xFF9800, alpha: 0.8 });
    body.rect(-15, -15, 30, 30).stroke({ width: 2, color: 0xFFFFFF });
    container.addChild(body);

    // Draw emission direction arrow
    const arrow = new PIXI.Graphics();
    const arrowLength = 40;
    const arrowX = Math.cos(direction) * arrowLength;
    const arrowY = Math.sin(direction) * arrowLength;

    arrow.moveTo(0, 0).lineTo(arrowX, arrowY).stroke({
      width: 3,
      color: 0xFFFFFF,
      alpha: 0.9
    });

    // Arrow head
    const headSize = 8;
    const headAngle = Math.atan2(arrowY, arrowX);
    const head1X = arrowX - headSize * Math.cos(headAngle - Math.PI / 6);
    const head1Y = arrowY - headSize * Math.sin(headAngle - Math.PI / 6);
    const head2X = arrowX - headSize * Math.cos(headAngle + Math.PI / 6);
    const head2Y = arrowY - headSize * Math.sin(headAngle + Math.PI / 6);

    arrow.moveTo(arrowX, arrowY).lineTo(head1X, head1Y)
         .moveTo(arrowX, arrowY).lineTo(head2X, head2Y)
         .stroke({ width: 3, color: 0xFFFFFF, alpha: 0.9 });

    container.addChild(arrow);

    // Add label
    const label = new PIXI.Text('E', {
      fontSize: 14,
      fill: 0xFFFFFF,
      fontWeight: 'bold'
    });
    label.anchor.set(0.5);
    label.x = 0;
    label.y = 0;
    body.addChild(label);

    // Make interactive
    container.interactive = true;
    container.cursor = 'pointer';

    // Store data
    container.userData = {
      type: 'emitter',
      emitterId,
      rate,
      direction,
      velocity,
      ballRadius
    };

    // Add to stage
    this.viewport.addChild(container);
    this.emitters.set(emitterId, container);

    // Setup drag-and-drop
    this.setupEmitterDrag(container);

    return container;
  }

  setupEmitterDrag(container) {
    let dragging = false;
    let dragOffset = { x: 0, y: 0 };

    container.on('pointerdown', (e) => {
      dragging = true;
      const pos = e.data.global;
      const worldPos = this.viewport.toWorld(pos);
      dragOffset.x = worldPos.x - container.x;
      dragOffset.y = worldPos.y - container.y;
    });

    container.on('pointermove', (e) => {
      if (!dragging) return;

      const pos = e.data.global;
      const worldPos = this.viewport.toWorld(pos);

      container.x = worldPos.x - dragOffset.x;
      container.y = worldPos.y - dragOffset.y;
    });

    container.on('pointerup', () => {
      if (!dragging) return;
      dragging = false;

      // Send update to server
      this.onEmitterMoved(
        container.userData.emitterId,
        container.x,
        container.y
      );
    });
  }

  removeEmitter(emitterId) {
    const emitter = this.emitters.get(emitterId);
    if (emitter) {
      this.viewport.removeChild(emitter);
      emitter.destroy({ children: true });
      this.emitters.delete(emitterId);
    }
  }

  updateEmitterDirection(emitterId, direction) {
    const emitter = this.emitters.get(emitterId);
    if (!emitter) return;

    // Update arrow rotation
    const arrow = emitter.children[1];
    arrow.clear();

    const arrowLength = 40;
    const arrowX = Math.cos(direction) * arrowLength;
    const arrowY = Math.sin(direction) * arrowLength;

    arrow.moveTo(0, 0).lineTo(arrowX, arrowY).stroke({
      width: 3,
      color: 0xFFFFFF,
      alpha: 0.9
    });

    // Redraw arrow head...
  }

  // Callback when emitter is moved
  onEmitterMoved(emitterId, x, y) {
    // Override in WebSocketClient
  }
}
```

### 5.5 Frontend: UI Controls

**File:** `frontend/src/physics-ui.js`

```javascript
class PhysicsUI {
  createEmitterControls() {
    const section = document.createElement('div');
    section.className = 'physics-section';
    section.innerHTML = `
      <h4 class="physics-section-title">Emitter Tool</h4>

      <button id="create-emitter-btn" class="physics-btn physics-btn-primary">
        <span class="btn-icon">💨</span>
        <span class="btn-text">Create Emitter</span>
      </button>

      <div class="emitter-controls">
        <label class="control-label">
          <span>Rate (balls/sec):</span>
          <input type="range" id="emitter-rate" min="0.1" max="10"
                 step="0.1" value="2">
          <span id="emitter-rate-value" class="control-value">2.0</span>
        </label>

        <label class="control-label">
          <span>Direction:</span>
          <input type="range" id="emitter-direction" min="0" max="6.28"
                 step="0.1" value="0">
          <span id="emitter-direction-value" class="control-value">0° (Right)</span>
        </label>

        <label class="control-label">
          <span>Velocity (px/s):</span>
          <input type="range" id="emitter-velocity" min="10" max="500"
                 step="10" value="100">
          <span id="emitter-velocity-value" class="control-value">100</span>
        </label>

        <label class="control-label">
          <span>Ball Radius:</span>
          <input type="range" id="emitter-ball-radius" min="5" max="30"
                 step="1" value="10">
          <span id="emitter-ball-radius-value" class="control-value">10px</span>
        </label>
      </div>
    `;

    return section;
  }

  setupEmitterEventListeners() {
    const createBtn = document.getElementById('create-emitter-btn');
    const rateSlider = document.getElementById('emitter-rate');
    const directionSlider = document.getElementById('emitter-direction');
    const velocitySlider = document.getElementById('emitter-velocity');
    const ballRadiusSlider = document.getElementById('emitter-ball-radius');

    // Update value displays
    rateSlider.addEventListener('input', (e) => {
      document.getElementById('emitter-rate-value').textContent =
        parseFloat(e.target.value).toFixed(1);
    });

    directionSlider.addEventListener('input', (e) => {
      const radians = parseFloat(e.target.value);
      const degrees = Math.round(radians * 180 / Math.PI);
      const directions = ['Right', 'Down-Right', 'Down', 'Down-Left',
                         'Left', 'Up-Left', 'Up', 'Up-Right'];
      const dirIndex = Math.round(degrees / 45) % 8;
      document.getElementById('emitter-direction-value').textContent =
        `${degrees}° (${directions[dirIndex]})`;
    });

    velocitySlider.addEventListener('input', (e) => {
      document.getElementById('emitter-velocity-value').textContent =
        e.target.value;
    });

    ballRadiusSlider.addEventListener('input', (e) => {
      document.getElementById('emitter-ball-radius-value').textContent =
        `${e.target.value}px`;
    });

    // Create emitter on canvas click
    createBtn.addEventListener('click', () => {
      this.emitterCreateMode = true;
      createBtn.textContent = 'Click to Place Emitter';
      createBtn.classList.add('active');
    });

    // Handle canvas click
    this.renderer.app.view.addEventListener('click', (e) => {
      if (!this.emitterCreateMode) return;

      const rect = this.renderer.app.view.getBoundingClientRect();
      const screenX = e.clientX - rect.left;
      const screenY = e.clientY - rect.top;

      const worldPos = this.renderer.viewport.toWorld(screenX, screenY);

      // Send create message
      this.ws.send(JSON.stringify({
        type: 'physics-create-emitter',
        canvasId: this.canvasId,
        x: worldPos.x,
        y: worldPos.y,
        rate: parseFloat(rateSlider.value),
        direction: parseFloat(directionSlider.value),
        velocity: parseFloat(velocitySlider.value),
        ballRadius: parseFloat(ballRadiusSlider.value)
      }));

      // Reset mode
      this.emitterCreateMode = false;
      createBtn.textContent = 'Create Emitter';
      createBtn.classList.remove('active');
    });
  }
}
```

### 5.6 Testing the Emitter

**Backend REPL Test:**
```lisp
;; Start server
(collabcanvas:start-server)

;; Initialize physics for test canvas
(collabcanvas:init-canvas-physics "test-canvas")

;; Get storage
(defparameter *test-storage*
  (collabcanvas:get-canvas-ecs-storage "test-canvas"))

;; Create emitter
(cl-fast-ecs:with-storage (*test-storage*)
  (cl-fast-ecs:make-object
   '((:position :x 400.0 :y 300.0)
     (:emitter :rate 2.0
              :direction 0.0
              :velocity 150.0
              :ball-radius 12.0
              :last-emit-time 0.0
              :enabled t))))

;; Run emitter system for 5 seconds
(dotimes (i 300)  ; 5 seconds at 60 Hz
  (cl-fast-ecs:with-storage (*test-storage*)
    (cl-fast-ecs:run-system 'collabcanvas::emitter-system
                            :current-time (/ i 60.0)))
  (sleep (/ 1.0 60.0)))

;; Check how many balls were created
(cl-fast-ecs:with-storage (*test-storage*)
  (let ((ball-count 0))
    (cl-fast-ecs:do-entities ()
      (when (cl-fast-ecs:has-component (cl-fast-ecs:current-entity) 'ball)
        (incf ball-count)))
    (format t "Created ~A balls~%" ball-count)))
;; Should print ~10 balls (2/sec * 5 sec)
```

**Frontend Manual Test:**
1. Open http://localhost:8080
2. Click "Create Emitter" button
3. Click on canvas to place emitter
4. Adjust rate, direction, velocity sliders
5. Watch balls spawn automatically
6. Drag emitter to new position
7. Check browser DevTools console for errors

---

## 6. Best Practices

### Performance Optimization

#### 1. Use Type Declarations

```lisp
;; GOOD: Compiler can optimize
(define-component fast-particle
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))

;; BAD: Generic storage, slow
(define-component slow-particle
  (x 0.0)
  (y 0.0))
```

#### 2. Skip Sleeping Entities

```lisp
(define-system my-system
  (:components-rw (position velocity))

  ;; ALWAYS check sleeping status
  (unless (cl-fast-ecs:has-component (current-entity) 'sleeping)
    ;; ... update logic ...
    ))
```

#### 3. Use Read-Only Components When Possible

```lisp
;; GOOD: Optimization hint to compiler
(define-system collision-system
  (:components-ro (ball)        ; Read-only (no modifications)
   :components-rw (position))   ; Read-write (will modify)
  ...)

;; BAD: Everything read-write (slower)
(define-system collision-system
  (:components-rw (ball position))
  ...)
```

#### 4. Batch Database Operations

```lisp
;; GOOD: Single transaction for 100 entities
(with-transaction ()
  (dotimes (i 100)
    (save-physics-component canvas-id entity-id "ball" data)))

;; BAD: 100 separate transactions
(dotimes (i 100)
  (save-physics-component canvas-id entity-id "ball" data))
```

### Common Pitfalls

#### 1. Division by Zero

```lisp
;; BAD: Can crash with divide-by-zero
(let ((dist (sqrt (+ (* dx dx) (* dy dy)))))
  (let ((nx (/ dx dist))
        (ny (/ dy dist)))
    ...))

;; GOOD: Guard against zero distance
(let ((dist (sqrt (+ (* dx dx) (* dy dy)))))
  (when (> dist 0.01)  ; <-- Check before division
    (let ((nx (/ dx dist))
          (ny (/ dy dist)))
      ...)))
```

#### 2. Forgetting to Reset Accumulative Forces

```lisp
;; BAD: Acceleration accumulates forever
(define-system apply-acceleration-system
  (:components-rw (velocity acceleration))

  (incf velocity-vx (* acceleration-ax dt))
  (incf velocity-vy (* acceleration-ay dt))
  ;; MISSING: Reset acceleration to zero!
  )

;; GOOD: Reset after applying
(define-system apply-acceleration-system
  (:components-rw (velocity acceleration))

  (incf velocity-vx (* acceleration-ax dt))
  (incf velocity-vy (* acceleration-ay dt))

  ;; Reset for next frame
  (setf acceleration-ax 0.0)
  (setf acceleration-ay 0.0))
```

#### 3. Not Handling Component Deletion

```lisp
;; BAD: Crash if component is deleted mid-loop
(defun update-all-balls ()
  (dolist (ball-id *ball-ids*)
    (update-ball ball-id)))  ; Might be deleted!

;; GOOD: Check entity validity
(defun update-all-balls ()
  (dolist (ball-id *ball-ids*)
    (when (cl-fast-ecs:entity-valid-p ball-id)
      (update-ball ball-id))))
```

#### 4. Coordinate System Confusion

```javascript
// BAD: Mixing screen and world coordinates
canvas.addEventListener('click', (e) => {
  const x = e.clientX;  // Screen coordinates!
  const y = e.clientY;
  createBall(x, y);  // Wrong coordinate system
});

// GOOD: Convert to world coordinates
canvas.addEventListener('click', (e) => {
  const rect = canvas.getBoundingClientRect();
  const screenX = e.clientX - rect.left;
  const screenY = e.clientY - rect.top;

  const worldPos = renderer.viewport.toWorld(screenX, screenY);
  createBall(worldPos.x, worldPos.y);  // Correct!
});
```

### Debugging Techniques

#### 1. Print Entity State

```lisp
;; Debug specific entity
(cl-fast-ecs:with-storage (*test-storage*)
  (cl-fast-ecs:print-entity entity-id))

;; Output:
;; Entity 42:
;;   position: (x: 150.5, y: 200.3)
;;   velocity: (vx: 50.2, vy: -30.1)
;;   ball: (radius: 15.0, mass: 1.0, restitution: 0.8)
```

#### 2. Count Entities by Component

```lisp
(defun count-entities-with-component (storage component-name)
  (cl-fast-ecs:with-storage (storage)
    (let ((count 0))
      (cl-fast-ecs:do-entities ()
        (when (cl-fast-ecs:has-component
               (cl-fast-ecs:current-entity) component-name)
          (incf count)))
      count)))

;; Usage:
(count-entities-with-component *test-storage* 'ball)
;; => 47
```

#### 3. Log System Execution Time

```lisp
(defun run-physics-step-with-profiling (storage dt gravity-x gravity-y)
  (with-storage (storage)

    ;; Measure force application time
    (let ((start (get-internal-real-time)))
      (run-system 'apply-forces-system :dt dt)
      (format t "Forces: ~,2F ms~%"
              (* (- (get-internal-real-time) start) 1000.0)))

    ;; Measure collision detection time
    (let ((start (get-internal-real-time)))
      (run-system 'collision-system :dt dt)
      (format t "Collisions: ~,2F ms~%"
              (* (- (get-internal-real-time) start) 1000.0)))))
```

#### 4. Frontend Debug Overlay

```javascript
class PhysicsDebugger {
  showEntityInfo(entityId) {
    const overlay = document.createElement('div');
    overlay.className = 'debug-overlay';
    overlay.innerHTML = `
      <h4>Entity ${entityId}</h4>
      <div>Position: (${entity.x.toFixed(2)}, ${entity.y.toFixed(2)})</div>
      <div>Velocity: (${entity.vx.toFixed(2)}, ${entity.vy.toFixed(2)})</div>
      <div>Components: ${entity.components.join(', ')}</div>
    `;
    document.body.appendChild(overlay);
  }

  visualizeForceField(fieldId, radius) {
    const debug = new PIXI.Graphics();
    debug.circle(0, 0, radius).stroke({
      width: 1,
      color: 0xFF0000,
      alpha: 0.3
    });
    // Add grid lines showing force strength
    for (let r = 0; r < radius; r += 50) {
      debug.circle(0, 0, r).stroke({
        width: 1,
        color: 0xFF0000,
        alpha: 0.1
      });
    }
    return debug;
  }
}
```

---

## 7. API Reference

### Backend: Component Definition

#### `define-component`

Define a new physics component type.

**Syntax:**
```lisp
(define-component component-name
  "Documentation string"
  (slot-name default-value :type type-specifier)
  ...)
```

**Example:**
```lisp
(define-component spring
  "Spring connection between two entities"
  (target-entity 0 :type fixnum)
  (rest-length 100.0 :type single-float)
  (stiffness 50.0 :type single-float)
  (damping 0.8 :type single-float))
```

### Backend: System Definition

#### `define-system`

Define a physics system that processes entities.

**Syntax:**
```lisp
(define-system system-name
  (:components-ro (comp1 comp2 ...)    ; Read-only components
   :components-rw (comp3 comp4 ...)    ; Read-write components
   :with (comp5 comp6 ...)             ; Inner loop query (optional)
   :arguments ((:arg1 type) ...))      ; System parameters

  "Documentation string"

  ;; System body (executes per entity)
  ...)
```

**Example:**
```lisp
(define-system apply-drag-system
  (:components-rw (velocity)
   :components-ro (ball)
   :arguments ((:drag-coefficient single-float)))

  "Apply air resistance to ball velocity"

  (let ((drag (* drag-coefficient ball-radius)))
    (setf velocity-vx (* velocity-vx (- 1.0 drag)))
    (setf velocity-vy (* velocity-vy (- 1.0 drag)))))
```

### Backend: Entity Operations

#### `make-object`

Create a new entity with components.

**Syntax:**
```lisp
(make-object component-specs)
```

**Returns:** Entity ID (fixnum)

**Example:**
```lisp
(make-object
 '((:position :x 100.0 :y 200.0)
   (:velocity :vx 50.0 :vy -30.0)
   (:ball :radius 15.0 :mass 1.0)))
;; => 42
```

#### `delete-entity`

Delete an entity and all its components.

**Syntax:**
```lisp
(cl-fast-ecs:delete-entity entity-id)
```

#### `has-component`

Check if entity has a specific component.

**Syntax:**
```lisp
(cl-fast-ecs:has-component entity-id component-name)
```

**Returns:** T if component exists, NIL otherwise

**Example:**
```lisp
(when (cl-fast-ecs:has-component entity-id 'sleeping)
  (format t "Entity is sleeping~%"))
```

#### `get-component-value`

Get a component slot value.

**Syntax:**
```lisp
(cl-fast-ecs:get-component-value entity-id component-name slot-name)
```

**Example:**
```lisp
(let ((radius (cl-fast-ecs:get-component-value entity-id 'ball 'radius)))
  (format t "Ball radius: ~,2F~%" radius))
```

### Backend: Canvas Physics Management

#### `init-canvas-physics`

Initialize physics ECS storage for a canvas.

**Syntax:**
```lisp
(init-canvas-physics canvas-id &key gravity-x gravity-y simulation-rate)
```

**Arguments:**
- `canvas-id` (string): Unique canvas identifier
- `gravity-x` (float, optional): Horizontal gravity (default: 0.0)
- `gravity-y` (float, optional): Vertical gravity (default: 9.8)
- `simulation-rate` (integer, optional): Simulation Hz (default: 60)

**Returns:** ECS storage instance

**Example:**
```lisp
(init-canvas-physics "canvas-123"
                     :gravity-y 19.6
                     :simulation-rate 120)
```

#### `get-canvas-ecs-storage`

Retrieve ECS storage for a canvas.

**Syntax:**
```lisp
(get-canvas-ecs-storage canvas-id)
```

**Returns:** Storage instance or NIL

**Example:**
```lisp
(let ((storage (get-canvas-ecs-storage "canvas-123")))
  (when storage
    (with-storage (storage)
      ...)))
```

#### `update-canvas-physics-settings`

Update physics settings for an active canvas.

**Syntax:**
```lisp
(update-canvas-physics-settings canvas-id
                                &key gravity-x gravity-y simulation-rate)
```

**Example:**
```lisp
(update-canvas-physics-settings "canvas-123" :gravity-y 4.9)
```

### Backend: Database Operations

#### `save-physics-component`

Save component to database.

**Syntax:**
```lisp
(save-physics-component canvas-id entity-id component-type json-data)
```

**Example:**
```lisp
(save-physics-component "canvas-123" 42 "ball"
  (jonathan:to-json (list :radius 15.0 :mass 1.0)))
```

#### `load-physics-components`

Load all components for a canvas.

**Syntax:**
```lisp
(load-physics-components canvas-id)
```

**Returns:** List of alists with component data

### Frontend: PhysicsRenderer API

#### `createGhostBall(ghostId, x, y, radius, vx, vy)`

Create client-side prediction ball.

**Parameters:**
- `ghostId` (string): Unique identifier
- `x, y` (number): Position in world coordinates
- `radius` (number): Ball radius
- `vx, vy` (number): Initial velocity

**Returns:** PIXI.Graphics object

#### `createServerBall(ballId, x, y, radius, color)`

Create server-authoritative ball.

**Parameters:**
- `ballId` (number): Entity ID from server
- `x, y` (number): Position
- `radius` (number): Ball radius
- `color` (hex): Ball color (e.g., 0xFF0000)

**Returns:** PIXI.Graphics object

#### `createForceField(fieldId, x, y, type, strength, radius, direction)`

Create force field visualization.

**Parameters:**
- `fieldId` (number): Entity ID
- `x, y` (number): Position
- `type` (string): 'fan', 'gravity-well', or 'magnet'
- `strength` (number): Force magnitude
- `radius` (number): Area of influence
- `direction` (number): Angle in radians (for fans)

**Returns:** PIXI.Container object

### WebSocket Message Formats

#### Client → Server

**Create Component:**
```json
{
  "type": "physics-create-magnet",
  "canvasId": "canvas-123",
  "x": 100.0,
  "y": 200.0,
  "polarity": "attract",
  "strength": 200.0,
  "radius": 150.0
}
```

**Update Component:**
```json
{
  "type": "physics-update-magnet",
  "canvasId": "canvas-123",
  "magnetId": 42,
  "strength": 300.0,
  "enabled": true
}
```

**Delete Component:**
```json
{
  "type": "physics-delete-magnet",
  "canvasId": "canvas-123",
  "magnetId": 42
}
```

#### Server → Client

**Component Created:**
```json
{
  "type": "physics-magnet-created",
  "magnetId": 42,
  "x": 100.0,
  "y": 200.0,
  "polarity": "attract",
  "strength": 200.0,
  "radius": 150.0
}
```

**Component Updated:**
```json
{
  "type": "physics-magnet-updated",
  "magnetId": 42,
  "strength": 300.0,
  "enabled": true
}
```

**Component Deleted:**
```json
{
  "type": "physics-magnet-deleted",
  "magnetId": 42
}
```

---

## Quick Reference Checklist

### Adding a New Component in <1 Day

- [ ] **Backend: Define component** (5 min)
  - Add `define-component` to `physics-components.lisp`
  - Use single-float types for performance
  - Add comprehensive docstring

- [ ] **Backend: Implement system** (30 min)
  - Add `define-system` to `physics-systems.lisp`
  - Handle component logic (forces, collisions, etc.)
  - Skip sleeping entities for optimization

- [ ] **Backend: Integrate into physics loop** (5 min)
  - Add system call to `run-physics-step` in correct order
  - Forces → Acceleration → Velocity → Collision → Sleep

- [ ] **Backend: Add WebSocket handlers** (30 min)
  - Handle create/update/delete messages in `websocket-adapter.lisp`
  - Validate inputs and convert types
  - Broadcast changes to all clients

- [ ] **Frontend: Create visualization** (1 hour)
  - Add rendering method to `physics-renderer.js`
  - Use PIXI.Graphics for drawing
  - Make interactive (drag, click, hover)

- [ ] **Frontend: Add UI controls** (1 hour)
  - Create control panel section in `physics-ui.js`
  - Add sliders/buttons for component properties
  - Wire up event listeners

- [ ] **Frontend: Add message handlers** (15 min)
  - Handle create/update/delete messages in `websocket.js`
  - Call renderer methods to update visualization

- [ ] **Testing** (1-2 hours)
  - Backend REPL: Create entities, run systems
  - Frontend: UI interaction, WebSocket sync
  - Multi-client: Test real-time collaboration

- [ ] **Documentation** (30 min)
  - Add usage example to this guide
  - Update API reference
  - Add troubleshooting notes

**Total Time: 4-6 hours** (with practice, can be done in 2-3 hours)

---

## Conclusion

The CollabCanvas physics engine's ECS architecture makes adding new components straightforward and efficient. By following the patterns outlined in this guide, you can extend the physics system with custom behaviors without modifying core systems.

**Key Takeaways:**
1. ECS separates data (components) from logic (systems)
2. Type declarations enable high-performance physics
3. Systems run in a specific order for deterministic simulation
4. Frontend visualization is decoupled from backend physics
5. WebSocket protocol keeps all clients synchronized

For questions or advanced use cases, refer to:
- `backend/tests/ecs-physics-benchmark.lisp` - Performance benchmarks
- `backend/src/physics-*.lisp` - Source implementations
- `.taskmaster/docs/prd-phys-engine.md` - Product requirements

Happy building!
