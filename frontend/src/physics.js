/**
 * physics.js - 2D Physics Engine for CollabCanvas Living Canvas
 *
 * JavaScript port of backend/src/physics.lisp for client-side prediction.
 * Implements Verlet integration physics engine with:
 * - Circles as dynamic objects (affected by physics)
 * - Rectangles as static walls/platforms
 * - Mass proportional to radius (area-based)
 * - Global gravity
 * - Boundary constraints (bounce)
 * - Circle-circle and circle-rectangle collision detection
 *
 * CRITICAL: All math must match backend/src/physics.lisp exactly for determinism!
 */

// ============================================================================
// Physics Constants (MUST match backend/src/physics.lisp)
// ============================================================================

export const PHYSICS_TIMESTEP = 0.02;          // Fixed timestep: 50 steps/second
export const DEFAULT_GRAVITY = 9.8;            // Default gravity acceleration
export const DEFAULT_FRICTION = 0.02;          // Air friction coefficient
export const DEFAULT_RESTITUTION = 0.7;        // Bounciness (0=dead, 1=perfectly elastic)
export const CANVAS_WIDTH = 4000;              // Physics simulation width
export const CANVAS_HEIGHT = 3000;             // Physics simulation height

// ============================================================================
// Physics Object Class
// ============================================================================

export class PhysicsObject {
  constructor(config) {
    this.id = config.id;                      // Object ID (string)
    this.type = config.type;                  // 'circle' or 'rectangle'
    this.x = config.x;                        // Current X position
    this.y = config.y;                        // Current Y position
    this.oldX = config.oldX || config.x;      // Previous X (for Verlet integration)
    this.oldY = config.oldY || config.y;      // Previous Y
    this.vx = config.vx || 0.0;               // Velocity X (derived from position delta)
    this.vy = config.vy || 0.0;               // Velocity Y
    this.ax = config.ax || 0.0;               // Acceleration X
    this.ay = config.ay || 0.0;               // Acceleration Y
    this.radius = config.radius;              // For circles
    this.width = config.width;                // For rectangles
    this.height = config.height;              // For rectangles
    this.mass = config.mass;                  // Mass (derived from size)
    this.isDynamic = config.isDynamic !== undefined ? config.isDynamic : true;  // T if affected by physics
    this.restitution = config.restitution || DEFAULT_RESTITUTION;  // Bounciness
    this.friction = config.friction || DEFAULT_FRICTION;           // Friction coefficient
    this.color = config.color || '#3498db';   // Color (hex string)
  }
}

// ============================================================================
// Physics Engine State
// ============================================================================

export class PhysicsEngine {
  constructor() {
    this.objects = new Map();                 // Map of physics objects by ID
    this.globalGravity = DEFAULT_GRAVITY;     // Current global gravity value
    this.paused = true;                       // Whether physics simulation is paused
    this.boundaryMode = 'contain';            // Boundary behavior: 'contain' (bounce) or 'wrap'
  }

  // ==========================================================================
  // Object Creation and Management
  // ==========================================================================

  /**
   * Create a dynamic circle physics object
   */
  createCircle(id, x, y, radius, options = {}) {
    const isDynamic = options.isDynamic !== undefined ? options.isDynamic : true;
    const color = options.color || '#3498db';
    const mass = this.calculateMassFromRadius(radius);

    const obj = new PhysicsObject({
      id,
      type: 'circle',
      x,
      y,
      oldX: x,
      oldY: y,
      vx: 0.0,
      vy: 0.0,
      ax: 0.0,
      ay: 0.0,
      radius,
      mass,
      isDynamic,
      restitution: DEFAULT_RESTITUTION,
      friction: DEFAULT_FRICTION,
      color
    });

    this.objects.set(id, obj);
    return obj;
  }

  /**
   * Create a static rectangle (wall/platform)
   */
  createRectangle(id, x, y, width, height, options = {}) {
    const color = options.color || '#95a5a6';

    const obj = new PhysicsObject({
      id,
      type: 'rectangle',
      x,
      y,
      oldX: x,
      oldY: y,
      vx: 0.0,
      vy: 0.0,
      ax: 0.0,
      ay: 0.0,
      width,
      height,
      mass: 0.0,            // Static objects have infinite mass
      isDynamic: false,     // Never moves
      restitution: 0.8,
      friction: 0.1,
      color
    });

    this.objects.set(id, obj);
    return obj;
  }

  /**
   * Remove an object from the physics simulation
   */
  removeObject(id) {
    return this.objects.delete(id);
  }

  /**
   * Get a physics object by ID
   */
  getObject(id) {
    return this.objects.get(id);
  }

  /**
   * Remove all physics objects
   */
  clearAll() {
    this.objects.clear();
  }

  // ==========================================================================
  // Mass Calculation
  // ==========================================================================

  /**
   * Calculate mass based on circle area (π * r²)
   */
  calculateMassFromRadius(radius) {
    return Math.PI * radius * radius;
  }

  // ==========================================================================
  // Verlet Integration
  // ==========================================================================

  /**
   * Update position using Verlet integration
   * MUST match backend/src/physics.lisp integrate-verlet exactly!
   */
  integrateVerlet(obj, dt) {
    if (!obj.isDynamic) return;

    const dt2 = dt * dt;

    // Calculate velocity from position delta (Verlet)
    let vx = obj.x - obj.oldX;
    let vy = obj.y - obj.oldY;

    // Apply friction (air resistance)
    const frictionFactor = 1.0 - obj.friction;
    vx *= frictionFactor;
    vy *= frictionFactor;

    // Calculate new position
    const newX = obj.x + vx + (obj.ax * dt2);
    const newY = obj.y + vy + (obj.ay * dt2);

    // Store old position
    obj.oldX = obj.x;
    obj.oldY = obj.y;

    // Update position
    obj.x = newX;
    obj.y = newY;

    // Store velocity for external use
    obj.vx = vx / dt;
    obj.vy = vy / dt;

    // Reset acceleration
    obj.ax = 0.0;
    obj.ay = 0.0;
  }

  // ==========================================================================
  // Forces
  // ==========================================================================

  /**
   * Apply global gravity force to object
   * MUST match backend/src/physics.lisp apply-gravity exactly!
   */
  applyGravity(obj) {
    if (obj.isDynamic) {
      obj.ay += this.globalGravity;
    }
  }

  /**
   * Apply a force to an object (F = ma, so a = F/m)
   */
  applyForce(obj, fx, fy) {
    if (obj.isDynamic && obj.mass > 0) {
      obj.ax += fx / obj.mass;
      obj.ay += fy / obj.mass;
    }
  }

  // ==========================================================================
  // Boundary Constraints
  // ==========================================================================

  /**
   * Apply canvas boundary constraints
   * MUST match backend/src/physics.lisp apply-boundary-constraints exactly!
   */
  applyBoundaryConstraints(obj) {
    if (!obj.isDynamic) return;

    if (obj.type === 'circle') {
      const r = obj.radius;
      const restitution = obj.restitution;

      // Left/Right walls
      if (obj.x < r) {
        obj.x = r;
        // Reverse velocity (bounce)
        const vx = obj.x - obj.oldX;
        obj.oldX = obj.x + (vx * restitution);
      }

      if (obj.x > (CANVAS_WIDTH - r)) {
        obj.x = CANVAS_WIDTH - r;
        const vx = obj.x - obj.oldX;
        obj.oldX = obj.x + (vx * restitution);
      }

      // Top/Bottom walls
      if (obj.y < r) {
        obj.y = r;
        const vy = obj.y - obj.oldY;
        obj.oldY = obj.y + (vy * restitution);
      }

      if (obj.y > (CANVAS_HEIGHT - r)) {
        obj.y = CANVAS_HEIGHT - r;
        const vy = obj.y - obj.oldY;
        obj.oldY = obj.y + (vy * restitution);
      }
    }
  }

  // ==========================================================================
  // Collision Detection
  // ==========================================================================

  /**
   * Check and resolve collision between two circles
   * MUST match backend/src/physics.lisp check-circle-circle-collision exactly!
   */
  checkCircleCircleCollision(c1, c2) {
    const dx = c2.x - c1.x;
    const dy = c2.y - c1.y;
    const distSq = (dx * dx) + (dy * dy);
    const rSum = c1.radius + c2.radius;
    const rSumSq = rSum * rSum;

    if (distSq < rSumSq) {
      // Collision detected
      const dist = Math.sqrt(distSq);
      const overlap = rSum - dist;
      const nx = dx / dist;
      const ny = dy / dist;

      // Move circles apart proportional to their masses
      const totalMass = c1.mass + c2.mass;
      const ratio1 = totalMass > 0 ? c2.mass / totalMass : 0.5;
      const ratio2 = totalMass > 0 ? c1.mass / totalMass : 0.5;

      // Separate circles
      if (c1.isDynamic) {
        c1.x -= overlap * nx * ratio1;
        c1.y -= overlap * ny * ratio1;
      }

      if (c2.isDynamic) {
        c2.x += overlap * nx * ratio2;
        c2.y += overlap * ny * ratio2;
      }

      // Apply elastic collision response (simplified)
      const restitution = Math.min(c1.restitution, c2.restitution);

      if (c1.isDynamic && c2.isDynamic) {
        // Exchange velocities along collision normal
        const v1x = c1.x - c1.oldX;
        const v1y = c1.y - c1.oldY;
        const v2x = c2.x - c2.oldX;
        const v2y = c2.y - c2.oldY;

        // Relative velocity along normal
        const dvn = ((v1x - v2x) * nx) + ((v1y - v2y) * ny);

        if (dvn < 0) { // Objects moving towards each other
          // Impulse magnitude
          const impulse = (1.0 - restitution) * dvn;

          // Apply impulse
          c1.oldX -= impulse * nx * ratio1;
          c1.oldY -= impulse * ny * ratio1;
          c2.oldX += impulse * nx * ratio2;
          c2.oldY += impulse * ny * ratio2;
        }
      }

      return true; // Collision occurred
    }

    return false;
  }

  /**
   * Check and resolve collision between circle and axis-aligned rectangle
   * MUST match backend/src/physics.lisp check-circle-rectangle-collision exactly!
   */
  checkCircleRectangleCollision(circle, rect) {
    const cx = circle.x;
    const cy = circle.y;
    const r = circle.radius;
    const rx = rect.x;
    const ry = rect.y;
    const rw = rect.width;
    const rh = rect.height;

    // Find closest point on rectangle to circle center
    const closestX = Math.max(rx, Math.min(cx, rx + rw));
    const closestY = Math.max(ry, Math.min(cy, ry + rh));

    // Distance from circle center to closest point
    const dx = cx - closestX;
    const dy = cy - closestY;
    const distSq = (dx * dx) + (dy * dy);

    if (distSq < (r * r)) {
      // Collision detected
      const dist = Math.sqrt(distSq);
      const overlap = r - dist;
      const nx = dist > 0 ? dx / dist : 0;
      const ny = dist > 0 ? dy / dist : -1; // Default to pushing up
      const restitution = circle.restitution;

      // Push circle out of rectangle
      if (circle.isDynamic) {
        circle.x += overlap * nx;
        circle.y += overlap * ny;

        // Bounce (reflect velocity)
        const vx = circle.x - circle.oldX;
        const vy = circle.y - circle.oldY;
        const dot = 2 * ((vx * nx) + (vy * ny));

        circle.oldX = circle.x - ((vx - (dot * nx)) * restitution);
        circle.oldY = circle.y - ((vy - (dot * ny)) * restitution);
      }

      return true; // Collision occurred
    }

    return false;
  }

  /**
   * Resolve all collisions between physics objects
   */
  resolveAllCollisions() {
    const objects = Array.from(this.objects.values());
    let collisionCount = 0;

    // Circle-circle collisions
    for (let i = 0; i < objects.length; i++) {
      for (let j = i + 1; j < objects.length; j++) {
        const obj1 = objects[i];
        const obj2 = objects[j];

        if (obj1.type === 'circle' && obj2.type === 'circle' &&
            (obj1.isDynamic || obj2.isDynamic)) {
          if (this.checkCircleCircleCollision(obj1, obj2)) {
            collisionCount++;
          }
        }
      }
    }

    // Circle-rectangle collisions
    for (const obj of objects) {
      if (obj.type === 'circle' && obj.isDynamic) {
        for (const other of objects) {
          if (other.type === 'rectangle') {
            if (this.checkCircleRectangleCollision(obj, other)) {
              collisionCount++;
            }
          }
        }
      }
    }

    return collisionCount;
  }

  // ==========================================================================
  // Main Simulation Step
  // ==========================================================================

  /**
   * Execute one physics simulation step
   * MUST match backend/src/physics.lisp physics-step exactly!
   */
  step() {
    if (this.paused) return;

    const dt = PHYSICS_TIMESTEP;

    // 1. Apply forces
    for (const obj of this.objects.values()) {
      this.applyGravity(obj);
    }

    // 2. Integrate (update positions)
    for (const obj of this.objects.values()) {
      this.integrateVerlet(obj, dt);
    }

    // 3. Apply constraints
    for (const obj of this.objects.values()) {
      this.applyBoundaryConstraints(obj);
    }

    // 4. Resolve collisions (multiple iterations for stability)
    for (let i = 0; i < 3; i++) {
      this.resolveAllCollisions();
    }
  }

  // ==========================================================================
  // Utility Functions
  // ==========================================================================

  /**
   * Get a snapshot of all dynamic object positions for broadcasting
   */
  getStateSnapshot() {
    const snapshot = [];

    for (const obj of this.objects.values()) {
      if (obj.isDynamic) {
        snapshot.push({
          id: obj.id,
          x: obj.x,
          y: obj.y,
          rotation: 0.0  // For future use
        });
      }
    }

    return snapshot;
  }

  /**
   * Set the global gravity value
   */
  setGlobalGravity(gravity) {
    this.globalGravity = gravity;
  }

  /**
   * Pause the physics simulation
   */
  pause() {
    this.paused = true;
  }

  /**
   * Resume the physics simulation
   */
  resume() {
    this.paused = false;
  }

  /**
   * Reset all physics objects to their initial state
   */
  reset() {
    this.pause();
    this.clearAll();
  }

  /**
   * Check if physics is paused
   */
  isPaused() {
    return this.paused;
  }

  // ==========================================================================
  // Sync with Canvas State
  // ==========================================================================

  /**
   * Sync a canvas object to the physics engine
   */
  syncCanvasObject(objData) {
    const id = objData.id;
    const type = objData.type;
    const x = objData.x;
    const y = objData.y;
    const color = objData.color;
    const isDynamic = objData.isDynamic;

    if (type === 'circle') {
      const radius = objData.radius;
      if (!this.getObject(id)) {
        this.createCircle(id, x, y, radius, {
          isDynamic: isDynamic !== undefined ? isDynamic : true,
          color: color || '#3498db'
        });
      }
    } else if (type === 'rectangle') {
      const width = objData.width;
      const height = objData.height;
      if (!this.getObject(id)) {
        this.createRectangle(id, x, y, width, height, {
          color: color || '#95a5a6'
        });
      }
    }
  }

  /**
   * Manually update a physics object's position (e.g., user drag)
   */
  updateObjectPosition(id, x, y) {
    const obj = this.getObject(id);
    if (obj) {
      obj.x = x;
      obj.y = y;
      obj.oldX = x;
      obj.oldY = y;
      obj.vx = 0.0;
      obj.vy = 0.0;
    }
  }
}

// ============================================================================
// Export singleton instance (optional - can also create new instances)
// ============================================================================

export const physicsEngine = new PhysicsEngine();
