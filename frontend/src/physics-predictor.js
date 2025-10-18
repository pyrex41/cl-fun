/**
 * SimplePhysicsPredictor
 *
 * Lightweight ghost prediction system using simple velocity-based physics.
 * No external physics engine - just position + velocity updates.
 *
 * Target: <5ms prediction latency for 100+ ghosts
 */

export class SimplePhysicsPredictor {
  constructor() {
    // Map of ghostId -> ghost object
    this.ghosts = new Map();

    // Optional physics parameters
    this.gravity = 9.8; // pixels/second^2 (can be adjusted or disabled)
    this.restitution = 0.8; // Bounce coefficient (0-1)
    this.enableGravity = false; // Disabled by default
    this.enableBoundaryBounce = false; // Disabled by default

    // Boundary settings (can be updated based on canvas size)
    this.boundaries = {
      minX: 0,
      maxX: 1920,
      minY: 0,
      maxY: 1080
    };
  }

  /**
   * Spawn a new ghost with initial position and velocity
   *
   * @param {string|number} ghostId - Unique identifier for the ghost
   * @param {number} x - Initial x position
   * @param {number} y - Initial y position
   * @param {number} vx - Initial x velocity (default: 0)
   * @param {number} vy - Initial y velocity (default: 0)
   * @param {number} radius - Ghost radius (default: 10)
   * @returns {Object} The created ghost object
   */
  spawnGhost(ghostId, x, y, vx = 0, vy = 0, radius = 10) {
    const ghost = {
      id: ghostId,
      x,
      y,
      vx,
      vy,
      radius,
      createdAt: Date.now()
    };

    this.ghosts.set(ghostId, ghost);
    return ghost;
  }

  /**
   * Update all ghost positions based on velocity and time delta
   *
   * @param {number} dt - Time delta in seconds
   */
  step(dt) {
    for (const [ghostId, ghost] of this.ghosts.entries()) {
      // Simple velocity-based position update
      ghost.x += ghost.vx * dt;
      ghost.y += ghost.vy * dt;

      // Optional: Apply gravity
      if (this.enableGravity) {
        ghost.vy += this.gravity * dt;
      }

      // Optional: Boundary bounce with restitution
      if (this.enableBoundaryBounce) {
        this._handleBoundaryCollision(ghost);
      }
    }
  }

  /**
   * Handle boundary collision with bounce
   *
   * @private
   * @param {Object} ghost - Ghost object to check
   */
  _handleBoundaryCollision(ghost) {
    // Left boundary
    if (ghost.x - ghost.radius < this.boundaries.minX) {
      ghost.x = this.boundaries.minX + ghost.radius;
      ghost.vx = Math.abs(ghost.vx) * this.restitution;
    }

    // Right boundary
    if (ghost.x + ghost.radius > this.boundaries.maxX) {
      ghost.x = this.boundaries.maxX - ghost.radius;
      ghost.vx = -Math.abs(ghost.vx) * this.restitution;
    }

    // Top boundary
    if (ghost.y - ghost.radius < this.boundaries.minY) {
      ghost.y = this.boundaries.minY + ghost.radius;
      ghost.vy = Math.abs(ghost.vy) * this.restitution;
    }

    // Bottom boundary
    if (ghost.y + ghost.radius > this.boundaries.maxY) {
      ghost.y = this.boundaries.maxY - ghost.radius;
      ghost.vy = -Math.abs(ghost.vy) * this.restitution;
    }
  }

  /**
   * Remove a ghost from the prediction system
   *
   * @param {string|number} ghostId - ID of ghost to remove
   * @returns {boolean} True if ghost was removed, false if not found
   */
  removeGhost(ghostId) {
    return this.ghosts.delete(ghostId);
  }

  /**
   * Get a specific ghost by ID
   *
   * @param {string|number} ghostId - ID of ghost to retrieve
   * @returns {Object|undefined} The ghost object or undefined if not found
   */
  getGhost(ghostId) {
    return this.ghosts.get(ghostId);
  }

  /**
   * Get all ghosts as an array
   *
   * @returns {Array<Object>} Array of all ghost objects
   */
  getAllGhosts() {
    return Array.from(this.ghosts.values());
  }

  /**
   * Get the number of active ghosts
   *
   * @returns {number} Count of ghosts
   */
  getGhostCount() {
    return this.ghosts.size;
  }

  /**
   * Clear all ghosts from the prediction system
   */
  clearAllGhosts() {
    this.ghosts.clear();
  }

  /**
   * Update boundary settings for collision detection
   *
   * @param {number} minX - Minimum X boundary
   * @param {number} maxX - Maximum X boundary
   * @param {number} minY - Minimum Y boundary
   * @param {number} maxY - Maximum Y boundary
   */
  setBoundaries(minX, maxX, minY, maxY) {
    this.boundaries = { minX, maxX, minY, maxY };
  }

  /**
   * Enable or disable gravity
   *
   * @param {boolean} enabled - Whether gravity is enabled
   * @param {number} gravityValue - Gravity acceleration (default: 9.8)
   */
  setGravity(enabled, gravityValue = 9.8) {
    this.enableGravity = enabled;
    this.gravity = gravityValue;
  }

  /**
   * Enable or disable boundary bounce
   *
   * @param {boolean} enabled - Whether boundary bounce is enabled
   * @param {number} restitutionValue - Bounce coefficient 0-1 (default: 0.8)
   */
  setBoundaryBounce(enabled, restitutionValue = 0.8) {
    this.enableBoundaryBounce = enabled;
    this.restitution = Math.max(0, Math.min(1, restitutionValue));
  }

  /**
   * Update a ghost's velocity (useful for applying forces)
   *
   * @param {string|number} ghostId - ID of ghost to update
   * @param {number} vx - New x velocity
   * @param {number} vy - New y velocity
   * @returns {boolean} True if ghost was updated, false if not found
   */
  setGhostVelocity(ghostId, vx, vy) {
    const ghost = this.ghosts.get(ghostId);
    if (ghost) {
      ghost.vx = vx;
      ghost.vy = vy;
      return true;
    }
    return false;
  }

  /**
   * Update a ghost's position (useful for server corrections)
   *
   * @param {string|number} ghostId - ID of ghost to update
   * @param {number} x - New x position
   * @param {number} y - New y position
   * @returns {boolean} True if ghost was updated, false if not found
   */
  setGhostPosition(ghostId, x, y) {
    const ghost = this.ghosts.get(ghostId);
    if (ghost) {
      ghost.x = x;
      ghost.y = y;
      return true;
    }
    return false;
  }
}

// Export a default instance for convenience
export default SimplePhysicsPredictor;
