// src/physics-renderer.js
// Physics Renderer Extension for CanvasManager
// Handles ghost ball rendering, server ball interpolation, and force field visualizations

import * as PIXI from 'pixi.js';
import { CanvasManager } from './canvas.js';

/**
 * PhysicsRenderer extends CanvasManager to add physics-specific rendering:
 * - Ghost balls (client-side predictions) with semi-transparent dashed outlines
 * - Server balls with smooth 20Hz → 60 FPS interpolation
 * - Force field visualizations (fans, gravity wells)
 * - Performance monitoring for physics objects
 */
export class PhysicsRenderer extends CanvasManager {
  constructor(app) {
    super(app);

    // Physics-specific state
    this.ghostBalls = new Map(); // ghostId → PIXI.Graphics (client predictions)
    this.serverBalls = new Map(); // ballId → { graphics, serverX, serverY, currentX, currentY, vx, vy }
    this.forceFields = new Map(); // fieldId → PIXI.Graphics (fans, gravity wells)

    // Performance tracking
    this.physicsStats = {
      activeObjects: 0,
      sleepingObjects: 0,
      ghostCount: 0,
      serverBallCount: 0,
      forceFieldCount: 0
    };

    // Interpolation settings
    this.interpolationFactor = 0.3; // Smooth interpolation from server updates
    this.serverUpdateRate = 20; // Hz (server sends updates at 20 Hz)
    this.renderFps = 60; // Target render FPS

    // Setup physics rendering loop
    this.setupPhysicsRenderLoop();

    console.log('PhysicsRenderer initialized');
  }

  // ==================== Physics Render Loop ====================

  setupPhysicsRenderLoop() {
    // Add physics-specific update to the render loop
    this.app.ticker.add((deltaTime) => {
      this.updatePhysicsObjects(deltaTime);
      this.updatePhysicsStats();
    });
  }

  updatePhysicsObjects(deltaTime) {
    // Interpolate server ball positions for smooth 60 FPS rendering
    // Server sends updates at 20 Hz, we interpolate to 60 FPS
    this.serverBalls.forEach((ball, id) => {
      if (ball.graphics && ball.serverX !== undefined && ball.serverY !== undefined) {
        // Smooth interpolation: ball.currentX += (ball.serverX - ball.currentX) * 0.3
        ball.currentX += (ball.serverX - ball.currentX) * this.interpolationFactor;
        ball.currentY += (ball.serverY - ball.currentY) * this.interpolationFactor;

        // Update graphics position
        ball.graphics.x = ball.currentX;
        ball.graphics.y = ball.currentY;
      }
    });
  }

  updatePhysicsStats() {
    // Update performance statistics
    this.physicsStats.ghostCount = this.ghostBalls.size;
    this.physicsStats.serverBallCount = this.serverBalls.size;
    this.physicsStats.forceFieldCount = this.forceFields.size;
    this.physicsStats.activeObjects = this.serverBalls.size + this.ghostBalls.size;
    // TODO: Track sleeping objects when physics engine is integrated
    this.physicsStats.sleepingObjects = 0;
  }

  // ==================== Ghost Ball Rendering ====================

  /**
   * Create a ghost ball (client-side prediction)
   * @param {string} ghostId - Unique identifier for the ghost
   * @param {number} x - Initial x position
   * @param {number} y - Initial y position
   * @param {number} radius - Ball radius
   * @param {number} vx - Initial x velocity (for predictor)
   * @param {number} vy - Initial y velocity (for predictor)
   */
  createGhostBall(ghostId, x, y, radius = 20, vx = 0, vy = 0) {
    // Remove existing ghost if it exists
    if (this.ghostBalls.has(ghostId)) {
      this.removeGhostBall(ghostId);
    }

    const ghost = new PIXI.Graphics();

    // Semi-transparent circle (60% opacity)
    ghost.circle(0, 0, radius).fill({ color: 0xFFFFFF, alpha: 0.6 });

    // Dashed outline (simulated with multiple arcs)
    const dashCount = 16;
    const dashAngle = (Math.PI * 2) / dashCount;
    const dashLength = dashAngle * 0.6; // 60% dash, 40% gap

    for (let i = 0; i < dashCount; i++) {
      const startAngle = i * dashAngle;
      const endAngle = startAngle + dashLength;

      ghost.arc(0, 0, radius, startAngle, endAngle).stroke({
        width: 2,
        color: 0xFFFFFF,
        alpha: 0.8
      });
    }

    // Add 'G' label
    const label = new PIXI.Text('G', {
      fontSize: radius * 0.8,
      fill: 0xFFFFFF,
      fontWeight: 'bold',
      align: 'center'
    });
    label.anchor.set(0.5);
    label.x = 0;
    label.y = 0;
    label.alpha = 0.8;
    ghost.addChild(label);

    // Position the ghost
    ghost.x = x;
    ghost.y = y;

    // Ghost balls are non-interactive
    ghost.interactive = false;
    ghost.interactiveChildren = false;

    // Higher z-index to render on top of other objects
    ghost.zIndex = 100;

    // Store velocity for predictor integration
    ghost.userData = {
      type: 'ghost',
      radius,
      vx,
      vy,
      createdAt: performance.now()
    };

    this.ghostBalls.set(ghostId, ghost);
    this.viewport.addChild(ghost);

    console.log(`Created ghost ball ${ghostId} at (${x}, ${y}) with velocity (${vx}, ${vy})`);

    return ghost;
  }

  /**
   * Update ghost ball position (called by physics predictor)
   * @param {string} ghostId - Ghost identifier
   * @param {number} x - New x position
   * @param {number} y - New y position
   */
  updateGhostBall(ghostId, x, y) {
    const ghost = this.ghostBalls.get(ghostId);
    if (ghost) {
      ghost.x = x;
      ghost.y = y;
    }
  }

  /**
   * Remove a ghost ball (when server confirms)
   * @param {string} ghostId - Ghost identifier
   */
  removeGhostBall(ghostId) {
    const ghost = this.ghostBalls.get(ghostId);
    if (ghost) {
      this.viewport.removeChild(ghost);
      ghost.destroy({ children: true, texture: false, baseTexture: false });
      this.ghostBalls.delete(ghostId);
      console.log(`Removed ghost ball ${ghostId}`);
    }
  }

  /**
   * Transition ghost to server ball
   * @param {string} ghostId - Ghost identifier
   * @param {string} ballId - Server ball identifier
   * @param {number} serverX - Server-confirmed x position
   * @param {number} serverY - Server-confirmed y position
   */
  ghostToServerTransition(ghostId, ballId, serverX, serverY) {
    const ghost = this.ghostBalls.get(ghostId);

    if (ghost) {
      const radius = ghost.userData.radius;
      const currentX = ghost.x;
      const currentY = ghost.y;

      // Remove ghost
      this.removeGhostBall(ghostId);

      // Create server ball at current ghost position for smooth transition
      this.createServerBall(ballId, serverX, serverY, radius, currentX, currentY);

      console.log(`Transitioned ghost ${ghostId} to server ball ${ballId}`);
    } else {
      // No ghost to transition, just create server ball
      this.createServerBall(ballId, serverX, serverY);
    }
  }

  // ==================== Server Ball Rendering ====================

  /**
   * Create a server ball (confirmed by server)
   * @param {string} ballId - Unique identifier for the ball
   * @param {number} serverX - Server x position
   * @param {number} serverY - Server y position
   * @param {number} radius - Ball radius
   * @param {number} currentX - Optional current interpolated x (for smooth transition)
   * @param {number} currentY - Optional current interpolated y (for smooth transition)
   */
  createServerBall(ballId, serverX, serverY, radius = 20, currentX = null, currentY = null) {
    // Remove existing server ball if it exists
    if (this.serverBalls.has(ballId)) {
      this.removeServerBall(ballId);
    }

    const ball = new PIXI.Graphics();

    // Solid circle (100% opacity)
    ball.circle(0, 0, radius).fill({ color: 0x3498db, alpha: 1.0 });

    // Black outline
    ball.circle(0, 0, radius).stroke({ width: 2, color: 0x000000 });

    // Position the ball at interpolated position
    ball.x = currentX !== null ? currentX : serverX;
    ball.y = currentY !== null ? currentY : serverY;

    // Server balls are interactive (can be selected/dragged)
    ball.interactive = true;
    ball.cursor = 'pointer';

    // Enable smooth rendering
    ball.roundPixels = false;

    // Store data for interpolation
    ball.userData = { type: 'serverBall', radius };

    // Store ball state for interpolation
    const ballState = {
      graphics: ball,
      serverX: serverX,
      serverY: serverY,
      currentX: currentX !== null ? currentX : serverX,
      currentY: currentY !== null ? currentY : serverY,
      vx: 0,
      vy: 0
    };

    // Make ball draggable and selectable
    this.makeDraggable(ball, ballId);
    this.makeSelectable(ball, ballId);

    this.serverBalls.set(ballId, ballState);
    this.viewport.addChild(ball);

    console.log(`Created server ball ${ballId} at server(${serverX}, ${serverY}) current(${ballState.currentX}, ${ballState.currentY})`);

    return ball;
  }

  /**
   * Update server ball position (from server updates at 20 Hz)
   * @param {string} ballId - Ball identifier
   * @param {number} serverX - New server x position
   * @param {number} serverY - New server y position
   */
  updateServerBall(ballId, serverX, serverY) {
    const ball = this.serverBalls.get(ballId);
    if (ball) {
      // Update server position - interpolation happens in updatePhysicsObjects
      ball.serverX = serverX;
      ball.serverY = serverY;
    } else {
      // Ball doesn't exist, create it
      this.createServerBall(ballId, serverX, serverY);
    }
  }

  /**
   * Remove a server ball
   * @param {string} ballId - Ball identifier
   */
  removeServerBall(ballId) {
    const ball = this.serverBalls.get(ballId);
    if (ball && ball.graphics) {
      this.viewport.removeChild(ball.graphics);
      ball.graphics.destroy({ children: true, texture: false, baseTexture: false });
      this.serverBalls.delete(ballId);
      console.log(`Removed server ball ${ballId}`);
    }
  }

  // ==================== Force Field Visualizations ====================

  /**
   * Create a fan force field
   * @param {string} fieldId - Unique identifier for the field
   * @param {number} x - Field x position
   * @param {number} y - Field y position
   * @param {number} direction - Direction in radians
   * @param {number} strength - Force strength (visual scale)
   */
  createFanField(fieldId, x, y, direction = 0, strength = 1.0) {
    // Remove existing field if it exists
    if (this.forceFields.has(fieldId)) {
      this.removeForceField(fieldId);
    }

    const field = new PIXI.Graphics();

    // Draw arrow indicating direction
    const arrowLength = 40 * strength;
    const arrowWidth = 20 * strength;
    const headLength = 15 * strength;

    // Rotate arrow based on direction
    const cos = Math.cos(direction);
    const sin = Math.sin(direction);

    // Arrow shaft
    field.moveTo(0, -arrowWidth / 2);
    field.lineTo(arrowLength - headLength, -arrowWidth / 2);
    field.lineTo(arrowLength - headLength, -arrowWidth);
    field.lineTo(arrowLength, 0);
    field.lineTo(arrowLength - headLength, arrowWidth);
    field.lineTo(arrowLength - headLength, arrowWidth / 2);
    field.lineTo(0, arrowWidth / 2);
    field.lineTo(0, -arrowWidth / 2);
    field.fill({ color: 0x3498db, alpha: 0.5 }); // Blue color

    // Outline
    field.moveTo(0, -arrowWidth / 2);
    field.lineTo(arrowLength - headLength, -arrowWidth / 2);
    field.lineTo(arrowLength - headLength, -arrowWidth);
    field.lineTo(arrowLength, 0);
    field.lineTo(arrowLength - headLength, arrowWidth);
    field.lineTo(arrowLength - headLength, arrowWidth / 2);
    field.lineTo(0, arrowWidth / 2);
    field.lineTo(0, -arrowWidth / 2);
    field.stroke({ width: 2, color: 0x2980b9 });

    // Position and rotate
    field.x = x;
    field.y = y;
    field.rotation = direction;

    // Force fields are non-interactive
    field.interactive = false;
    field.interactiveChildren = false;

    // Lower z-index to render behind balls
    field.zIndex = -10;

    field.userData = { type: 'fan', direction, strength };

    this.forceFields.set(fieldId, field);
    this.viewport.addChild(field);

    console.log(`Created fan field ${fieldId} at (${x}, ${y}) direction ${direction}`);

    return field;
  }

  /**
   * Create a gravity well force field
   * @param {string} fieldId - Unique identifier for the field
   * @param {number} x - Field x position
   * @param {number} y - Field y position
   * @param {number} radius - Field radius (visual scale)
   * @param {number} strength - Force strength (visual intensity)
   */
  createGravityWellField(fieldId, x, y, radius = 50, strength = 1.0) {
    // Remove existing field if it exists
    if (this.forceFields.has(fieldId)) {
      this.removeForceField(fieldId);
    }

    const field = new PIXI.Graphics();

    // Draw swirl/radial pattern for gravity well
    const spirals = 3;
    const segments = 64;
    const alpha = 0.4 * strength;

    for (let spiral = 0; spiral < spirals; spiral++) {
      const angleOffset = (spiral / spirals) * Math.PI * 2;

      for (let i = 0; i < segments; i++) {
        const t = i / segments;
        const angle = angleOffset + t * Math.PI * 4; // 2 full rotations
        const r = radius * (1 - t); // Spiral inward

        const x1 = Math.cos(angle) * r;
        const y1 = Math.sin(angle) * r;

        if (i === 0) {
          field.moveTo(x1, y1);
        } else {
          field.lineTo(x1, y1);
        }
      }
    }

    field.stroke({ width: 3, color: 0x9b59b6, alpha }); // Purple color

    // Add central circle
    field.circle(0, 0, 8).fill({ color: 0x9b59b6, alpha: alpha * 1.5 });
    field.circle(0, 0, 8).stroke({ width: 2, color: 0x8e44ad });

    // Position
    field.x = x;
    field.y = y;

    // Force fields are non-interactive
    field.interactive = false;
    field.interactiveChildren = false;

    // Lower z-index to render behind balls
    field.zIndex = -10;

    field.userData = { type: 'gravityWell', radius, strength };

    this.forceFields.set(fieldId, field);
    this.viewport.addChild(field);

    console.log(`Created gravity well field ${fieldId} at (${x}, ${y}) radius ${radius}`);

    return field;
  }

  /**
   * Create a magnet force field
   * @param {string} fieldId - Unique identifier for the field
   * @param {number} x - Field x position
   * @param {number} y - Field y position
   * @param {number} polarity - Magnet polarity (+1 or -1)
   * @param {number} radius - Field radius (visual scale)
   * @param {number} strength - Force strength (visual intensity)
   */
  createMagnetField(fieldId, x, y, polarity = 1, radius = 80, strength = 1.0) {
    // Remove existing field if it exists
    if (this.forceFields.has(fieldId)) {
      this.removeForceField(fieldId);
    }

    const field = new PIXI.Graphics();

    // Color based on polarity: red for positive (+1), blue for negative (-1)
    const magnetColor = polarity > 0 ? 0xe74c3c : 0x3498db;
    const alpha = 0.3 * strength;

    // Draw horseshoe magnet shape
    const magnetWidth = radius * 0.8;
    const magnetHeight = radius * 1.2;
    const thickness = radius * 0.3;

    // Horseshoe path (U-shape)
    // Left bar
    field.rect(-magnetWidth / 2 - thickness / 2, -magnetHeight / 2, thickness, magnetHeight)
         .fill({ color: magnetColor, alpha });

    // Bottom bar
    field.rect(-magnetWidth / 2 - thickness / 2, magnetHeight / 2 - thickness, magnetWidth + thickness, thickness)
         .fill({ color: magnetColor, alpha });

    // Right bar
    field.rect(magnetWidth / 2 - thickness / 2, -magnetHeight / 2, thickness, magnetHeight)
         .fill({ color: magnetColor, alpha });

    // Magnetic field lines (radiating from poles)
    const fieldLines = 8;
    for (let i = 0; i < fieldLines; i++) {
      const angle = (i / fieldLines) * Math.PI * 2;
      const lineRadius = radius * 1.3;
      const x1 = Math.cos(angle) * radius * 0.6;
      const y1 = Math.sin(angle) * radius * 0.6;
      const x2 = Math.cos(angle) * lineRadius;
      const y2 = Math.sin(angle) * lineRadius;

      field.moveTo(x1, y1).lineTo(x2, y2).stroke({
        width: 2,
        color: magnetColor,
        alpha: alpha * 0.5
      });
    }

    // Add polarity label (N/S or +/-)
    const polarityText = polarity > 0 ? 'N' : 'S';
    const label = new PIXI.Text(polarityText, {
      fontSize: radius * 0.4,
      fill: magnetColor,
      fontWeight: 'bold',
      align: 'center'
    });
    label.anchor.set(0.5);
    label.x = 0;
    label.y = 0;
    label.alpha = 0.9;
    field.addChild(label);

    // Position
    field.x = x;
    field.y = y;

    // Force fields are non-interactive
    field.interactive = false;
    field.interactiveChildren = false;

    // Lower z-index to render behind balls
    field.zIndex = -10;

    field.userData = { type: 'magnet', polarity, radius, strength };

    this.forceFields.set(fieldId, field);
    this.viewport.addChild(field);

    console.log(`Created magnet field ${fieldId} at (${x}, ${y}) polarity ${polarity > 0 ? '+' : '-'}`);

    return field;
  }

  /**
   * Remove a force field
   * @param {string} fieldId - Field identifier
   */
  removeForceField(fieldId) {
    const field = this.forceFields.get(fieldId);
    if (field) {
      this.viewport.removeChild(field);
      field.destroy({ children: true, texture: false, baseTexture: false });
      this.forceFields.delete(fieldId);
      console.log(`Removed force field ${fieldId}`);
    }
  }

  // ==================== Emitter Visualizations (Post-MVP) ====================

  /**
   * Create an emitter visualization
   * @param {string} emitterId - Unique identifier for the emitter
   * @param {number} x - Emitter x position
   * @param {number} y - Emitter y position
   * @param {number} direction - Emission direction in radians
   * @param {number} rate - Emission rate in balls/sec
   * @param {boolean} enabled - Whether emitter is active
   */
  createEmitter(emitterId, x, y, direction = 0, rate = 1.0, enabled = true) {
    // Remove existing emitter if it exists
    if (this.forceFields.has(emitterId)) {
      this.removeForceField(emitterId);
    }

    const emitter = new PIXI.Graphics();
    const alpha = enabled ? 0.8 : 0.3;

    // Draw emitter nozzle (trapezoid shape)
    const nozzleLength = 40;
    const nozzleWidth = 30;
    const nozzleTip = 10;

    // Trapezoid path (pointing right initially, will be rotated)
    emitter.moveTo(0, -nozzleWidth / 2);
    emitter.lineTo(nozzleLength, -nozzleTip / 2);
    emitter.lineTo(nozzleLength, nozzleTip / 2);
    emitter.lineTo(0, nozzleWidth / 2);
    emitter.lineTo(0, -nozzleWidth / 2);
    emitter.fill({ color: 0xf39c12, alpha }); // Orange color

    // Outline
    emitter.moveTo(0, -nozzleWidth / 2);
    emitter.lineTo(nozzleLength, -nozzleTip / 2);
    emitter.lineTo(nozzleLength, nozzleTip / 2);
    emitter.lineTo(0, nozzleWidth / 2);
    emitter.lineTo(0, -nozzleWidth / 2);
    emitter.stroke({ width: 2, color: 0xe67e22, alpha });

    // Draw emission particles (3 small circles showing direction)
    const particleCount = 3;
    const particleSpacing = 15;
    const particleAlpha = enabled ? 0.6 : 0.2;

    for (let i = 1; i <= particleCount; i++) {
      const particleX = nozzleLength + (i * particleSpacing);
      const particleRadius = 4 - i; // Smaller as they get farther
      emitter.circle(particleX, 0, particleRadius).fill({
        color: 0xf39c12,
        alpha: particleAlpha * (1 - i * 0.2)
      });
    }

    // Add emission rate indicator (rings around nozzle)
    const rateRings = Math.min(Math.ceil(rate), 5); // 1-5 rings based on rate
    for (let i = 0; i < rateRings; i++) {
      const ringRadius = 15 + (i * 5);
      emitter.circle(0, 0, ringRadius).stroke({
        width: 1,
        color: 0xf39c12,
        alpha: (enabled ? 0.3 : 0.1) * (1 - i * 0.15)
      });
    }

    // Add 'E' label
    const label = new PIXI.Text('E', {
      fontSize: 16,
      fill: 0xf39c12,
      fontWeight: 'bold',
      align: 'center'
    });
    label.anchor.set(0.5);
    label.x = -10;
    label.y = 0;
    label.alpha = alpha;
    emitter.addChild(label);

    // Position and rotate
    emitter.x = x;
    emitter.y = y;
    emitter.rotation = direction;

    // Emitters are interactive (can be selected/moved)
    emitter.interactive = true;
    emitter.cursor = 'pointer';

    // Lower z-index to render behind balls
    emitter.zIndex = -5;

    emitter.userData = {
      type: 'emitter',
      direction,
      rate,
      enabled
    };

    this.forceFields.set(emitterId, emitter);
    this.viewport.addChild(emitter);

    console.log(`Created emitter ${emitterId} at (${x}, ${y}) direction ${direction} rate ${rate}/s enabled=${enabled}`);

    return emitter;
  }

  /**
   * Update emitter properties
   * @param {string} emitterId - Emitter identifier
   * @param {Object} updates - Properties to update (direction, rate, enabled)
   */
  updateEmitter(emitterId, updates) {
    const emitter = this.forceFields.get(emitterId);
    if (!emitter) {
      console.warn(`Emitter ${emitterId} not found for update`);
      return;
    }

    const userData = emitter.userData;

    // Update properties
    if (updates.direction !== undefined) {
      userData.direction = updates.direction;
      emitter.rotation = updates.direction;
    }

    if (updates.rate !== undefined) {
      userData.rate = updates.rate;
      // Recreate emitter to update rate rings
      const x = emitter.x;
      const y = emitter.y;
      const enabled = updates.enabled !== undefined ? updates.enabled : userData.enabled;
      this.createEmitter(emitterId, x, y, userData.direction, updates.rate, enabled);
      return;
    }

    if (updates.enabled !== undefined) {
      userData.enabled = updates.enabled;
      // Recreate emitter to update alpha
      const x = emitter.x;
      const y = emitter.y;
      this.createEmitter(emitterId, x, y, userData.direction, userData.rate, updates.enabled);
      return;
    }

    console.log(`Updated emitter ${emitterId}:`, updates);
  }

  /**
   * Remove an emitter
   * @param {string} emitterId - Emitter identifier
   */
  removeEmitter(emitterId) {
    this.removeForceField(emitterId);
  }

  // ==================== Performance Monitoring ====================

  /**
   * Get physics-specific performance statistics
   * @returns {Object} Performance stats including FPS, object counts, etc.
   */
  getPhysicsStats() {
    const baseStats = this.getPerformanceStats();

    return {
      ...baseStats,
      ...this.physicsStats,
      interpolationFactor: this.interpolationFactor,
      serverUpdateRate: this.serverUpdateRate,
      renderFps: this.renderFps
    };
  }

  /**
   * Log physics performance statistics to console
   */
  logPhysicsStats() {
    const stats = this.getPhysicsStats();
    console.log('=== Physics Performance Stats ===');
    console.log(`FPS: ${stats.current} (avg: ${stats.average}, min: ${stats.min}, max: ${stats.max})`);
    console.log(`Ghost Balls: ${stats.ghostCount}`);
    console.log(`Server Balls: ${stats.serverBallCount}`);
    console.log(`Force Fields: ${stats.forceFieldCount}`);
    console.log(`Active Objects: ${stats.activeObjects}`);
    console.log(`Sleeping Objects: ${stats.sleepingObjects}`);
    console.log(`Interpolation Factor: ${stats.interpolationFactor}`);
    console.log('================================');
  }

  // ==================== Physics Message Handlers ====================

  /**
   * Handle physics-delta message from server
   * Processes physics updates at 20 Hz and interpolates to 60 FPS
   * @param {Object} data - Physics delta message with entities array
   */
  handlePhysicsDelta(data) {
    if (!data || !data.entities) {
      console.warn('handlePhysicsDelta: Invalid data received', data);
      return;
    }

    // Process each entity in the update
    data.entities.forEach(entity => {
      const ballId = `ball-${entity['entity-id']}`;
      const x = entity.x;
      const y = entity.y;
      const vx = entity.vx || 0;
      const vy = entity.vy || 0;
      const radius = entity.radius || 20;

      // Check if this is a new ball or an existing one
      const existingBall = this.serverBalls.get(ballId);

      if (existingBall) {
        // Update existing ball's server position
        this.updateServerBall(ballId, x, y);
      } else {
        // Create new server ball
        this.createServerBall(ballId, x, y, radius);
        console.log(`Physics delta created new ball: ${ballId} at (${x.toFixed(1)}, ${y.toFixed(1)})`);
      }
    });
  }

  // ==================== Cleanup ====================

  /**
   * Clear all physics objects
   */
  clearAllPhysicsObjects() {
    console.log('Clearing all physics objects...');

    // Clear ghost balls
    this.ghostBalls.forEach((ghost, id) => {
      this.removeGhostBall(id);
    });

    // Clear server balls
    this.serverBalls.forEach((ball, id) => {
      this.removeServerBall(id);
    });

    // Clear force fields
    this.forceFields.forEach((field, id) => {
      this.removeForceField(id);
    });

    console.log('All physics objects cleared');
  }

  /**
   * Extended destroy method to clean up physics objects
   */
  destroy() {
    // Clear physics objects
    this.clearAllPhysicsObjects();

    // Call parent destroy
    super.destroy();

    console.log('PhysicsRenderer destroyed');
  }
}
