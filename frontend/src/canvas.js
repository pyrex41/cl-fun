// src/canvas.js
// Complete PixiJS Canvas Manager for CollabCanvas
import * as PIXI from 'pixi.js';
import {
  PhysicsEngine,
  PHYSICS_TIMESTEP,
  clampRadius,
  clampRectangleDimensions,
  EMITTER_PRESETS
} from './physics.js';

// Note: CullerPlugin may not be available in all PixiJS v8 builds
// We have custom viewport culling as a fallback
try {
  // Attempt to import and register CullerPlugin if available
  if (PIXI.extensions && PIXI.CullerPlugin) {
    PIXI.extensions.add(PIXI.CullerPlugin);
    console.log('CullerPlugin registered successfully');
  }
} catch (e) {
  console.log('CullerPlugin not available, using custom culling implementation');
}

export class PerformanceMonitor {
  constructor(app, canvasManager) {
    this.app = app;
    this.canvasManager = canvasManager;
    this.fpsHistory = [];
    this.maxHistorySize = 60;
    this.currentFps = 60;
    this.lastTime = performance.now();
    this.frameCount = 0;

    // Bind ticker update
    this.app.ticker.add(this.update.bind(this));

    console.log('PerformanceMonitor initialized');
  }

  update(deltaTime) {
    // Calculate FPS based on delta time (in seconds)
    const now = performance.now();
    const deltaMs = now - this.lastTime;

    if (deltaMs > 0) {
      this.currentFps = Math.round(1000 / deltaMs);
    }

    this.lastTime = now;

    // Add to history
    this.fpsHistory.push(this.currentFps);
    if (this.fpsHistory.length > this.maxHistorySize) {
      this.fpsHistory.shift(); // Remove oldest entry
    }

    // Check for low FPS and log warnings
    if (this.currentFps < 55) {
      const avgFps = this.getAverageFps();
      const objectCount = this.canvasManager ? this.canvasManager.objects.size : 0;
      console.warn(`⚠️ Low FPS detected: ${this.currentFps} FPS (avg: ${avgFps.toFixed(1)}, objects: ${objectCount})`);
    }
  }

  getAverageFps() {
    if (this.fpsHistory.length === 0) return 60;
    const sum = this.fpsHistory.reduce((a, b) => a + b, 0);
    return sum / this.fpsHistory.length;
  }

  getStats() {
    if (this.fpsHistory.length === 0) {
      return {
        current: this.currentFps,
        average: 60,
        min: 60,
        max: 60
      };
    }

    return {
      current: this.currentFps,
      average: Math.round(this.getAverageFps() * 10) / 10,
      min: Math.min(...this.fpsHistory),
      max: Math.max(...this.fpsHistory)
    };
  }

  destroy() {
    if (this.app && this.app.ticker) {
      this.app.ticker.remove(this.update.bind(this));
    }
  }
}

export class CanvasManager {
  constructor(app) {
    this.app = app;
    this.viewport = new PIXI.Container();
    this.objects = new Map(); // objectId -> PIXI Graphics
    this.selectedObjects = new Set();
    this.selectionIndicators = new Map(); // objectId -> selection box Graphics
    this.remoteCursors = new Map(); // userId -> cursor container

    // State
    this.isPanning = false;
    this.panStart = { x: 0, y: 0 };
    this.currentTool = 'select'; // 'select', 'rectangle', 'circle', 'text'
    this.currentColor = 0x3498db; // Default blue

    // Drag state (centralized for performance)
    this.isDragging = false;
    this.draggedObject = null;
    this.draggedObjectId = null;
    this.dragOffset = { x: 0, y: 0 };
    this.lastDragUpdate = 0;

    // Viewport culling
    this.cullingEnabled = true;
    this.cullingPadding = 200; // Extra padding around viewport for smooth scrolling
    this.lastViewportBounds = null;

    // Performance monitoring
    this.performanceMonitor = new PerformanceMonitor(app, this);

    // Shared cursor texture for performance optimization
    this.cursorTexture = this.createSharedCursorTexture();

    // Physics Engine for client-side prediction
    this.physicsEngine = new PhysicsEngine();
    this.physicsEngine.pause(); // Start paused, will resume when needed

    // Physics prediction and reconciliation state
    this.physicsEnabled = false; // Physics is opt-in per canvas
    this.lastPhysicsUpdate = 0;
    this.physicsAccumulator = 0; // For fixed timestep
    this.reconciliationLerpFactor = 0.3; // Gentle interpolation for server corrections

    // Server snapshot tracking
    this.lastServerSnapshot = null;
    this.serverSnapshotTime = 0;

    // Viewport setup
    this.app.stage.addChild(this.viewport);
    this.viewport.sortableChildren = true;

    // Enable culling on viewport for PixiJS built-in CullerPlugin (if available)
    // Falls back to our custom culling implementation in setupViewportCulling()
    this.viewport.cullable = true;

    // Grid background (optional visual aid)
    this.drawGrid();

    // Setup interaction
    this.setupPanZoom();
    this.setupCentralizedDrag(); // NEW: Centralized drag handler
    this.setupKeyboardShortcuts();
    this.setupToolHandlers();
    this.setupViewportCulling();

    console.log('Canvas initialized');
  }

  createSharedCursorTexture() {
    // Create cursor shape using Graphics
    const graphics = new PIXI.Graphics();

    // Draw cursor pointer (triangle) using v8 builder pattern
    graphics.poly([
      { x: 0, y: 0 },
      { x: 12, y: 18 },
      { x: 6, y: 18 },
      { x: 0, y: 24 }
    ]).fill(0xFFFFFF); // White fill (will be tinted)

    // Render to texture
    const texture = this.app.renderer.generateTexture(graphics, {
      resolution: 1,
      scaleMode: PIXI.SCALE_MODES.LINEAR
    });

    // Clean up graphics object
    graphics.destroy();

    return texture;
  }

  // ==================== Grid ====================
  
  drawGrid() {
    const grid = new PIXI.Graphics();
    
    // Enable smooth rendering for grid
    grid.roundPixels = false;

    const gridSize = 50;
    const gridExtent = 5000;

    // PixiJS v8 builder pattern for lines
    // Vertical lines
    for (let x = -gridExtent; x <= gridExtent; x += gridSize) {
      grid.moveTo(x, -gridExtent);
      grid.lineTo(x, gridExtent);
    }

    // Horizontal lines
    for (let y = -gridExtent; y <= gridExtent; y += gridSize) {
      grid.moveTo(-gridExtent, y);
      grid.lineTo(gridExtent, y);
    }
    
    // Apply stroke at the end with v8 API
    grid.stroke({ width: 1, color: 0x333333, alpha: 0.3 });

    grid.zIndex = -1;
    // Grid is non-interactive - optimize event traversal
    grid.interactive = false;
    grid.interactiveChildren = false;
    this.viewport.addChild(grid);
  }
  
  // ==================== Pan & Zoom ====================
  
  setupPanZoom() {
    const canvas = this.app.canvas;
    
    // Pan with middle mouse or Alt+drag
    canvas.addEventListener('mousedown', (e) => {
      if (e.button === 1 || (e.button === 0 && e.altKey)) {
        this.isPanning = true;
        this.panStart = { x: e.clientX, y: e.clientY };
        canvas.style.cursor = 'grabbing';
        e.preventDefault();
      }
    });
    
    canvas.addEventListener('mousemove', (e) => {
      if (this.isPanning) {
        const dx = e.clientX - this.panStart.x;
        const dy = e.clientY - this.panStart.y;
        
        this.viewport.x += dx;
        this.viewport.y += dy;
        
        this.panStart = { x: e.clientX, y: e.clientY };
      }
    });
    
    canvas.addEventListener('mouseup', () => {
      if (this.isPanning) {
        this.isPanning = false;
        canvas.style.cursor = 'default';
      }
    });
    
    // Zoom with mouse wheel
    canvas.addEventListener('wheel', (e) => {
      e.preventDefault();
      
      const zoomFactor = e.deltaY > 0 ? 0.9 : 1.1;
      const mouseX = e.clientX;
      const mouseY = e.clientY;
      
      // Get position before zoom
      const worldPosBefore = this.screenToWorld(mouseX, mouseY);
      
      // Apply zoom
      const newScale = this.viewport.scale.x * zoomFactor;
      if (newScale >= 0.1 && newScale <= 10) {
        this.viewport.scale.set(newScale);
        
        // Adjust position to keep mouse over same world point
        const worldPosAfter = this.screenToWorld(mouseX, mouseY);
        this.viewport.x += (worldPosAfter.x - worldPosBefore.x) * this.viewport.scale.x;
        this.viewport.y += (worldPosAfter.y - worldPosBefore.y) * this.viewport.scale.y;
      }
    }, { passive: false });
  }

  // ==================== Centralized Drag (Performance Optimized) ====================

  setupCentralizedDrag() {
    const canvas = this.app.canvas;

    // Global mousemove handler (only one for all objects)
    canvas.addEventListener('mousemove', (e) => {
      if (this.isDragging && this.draggedObject) {
        const worldPos = this.screenToWorld(e.clientX, e.clientY);

        // Update position immediately
        this.draggedObject.x = worldPos.x - this.dragOffset.x;
        this.draggedObject.y = worldPos.y - this.dragOffset.y;

        // Throttle network updates
        const now = performance.now();
        if (now - this.lastDragUpdate >= 16) { // 60 FPS
          if (this.onObjectUpdated) {
            this.onObjectUpdated(this.draggedObjectId, {
              x: this.draggedObject.x,
              y: this.draggedObject.y
            });
          }
          this.lastDragUpdate = now;
        }
      }
    });

    // Global mouseup handler
    canvas.addEventListener('mouseup', () => {
      if (this.isDragging && this.draggedObject) {
        this.draggedObject.alpha = 1;

        // Send final position
        if (this.onObjectUpdated) {
          this.onObjectUpdated(this.draggedObjectId, {
            x: this.draggedObject.x,
            y: this.draggedObject.y
          });
        }

        // Reset drag state
        this.isDragging = false;
        this.draggedObject = null;
        this.draggedObjectId = null;
      }
    });
  }

  screenToWorld(screenX, screenY) {
    return {
      x: (screenX - this.viewport.x) / this.viewport.scale.x,
      y: (screenY - this.viewport.y) / this.viewport.scale.y
    };
  }
  
  worldToScreen(worldX, worldY) {
    return {
      x: worldX * this.viewport.scale.x + this.viewport.x,
      y: worldY * this.viewport.scale.y + this.viewport.y
    };
  }
  
  // ==================== Keyboard Shortcuts ====================
  
  setupKeyboardShortcuts() {
    document.addEventListener('keydown', (e) => {
      // Tool shortcuts
      if (e.key === 'r' || e.key === 'R') {
        this.setTool('rectangle');
      } else if (e.key === 'c' || e.key === 'C') {
        this.setTool('circle');
      } else if (e.key === 'e' || e.key === 'E') {
        this.setTool('emitter');
      } else if (e.key === 't' || e.key === 'T') {
        this.setTool('text');
      } else if (e.key === 'v' || e.key === 'V' || e.key === 'Escape') {
        this.setTool('select');
      } else if (e.key === 'Delete' || e.key === 'Backspace') {
        this.deleteSelected();
      }
    });
  }
  
  setTool(tool) {
    this.currentTool = tool;
    console.log('Tool:', tool);
    // Update UI indicator if you have one
  }
  
  // ==================== Tool Handlers ====================
  
  setupToolHandlers() {
    const canvas = this.app.canvas;
    let drawStart = null;
    let previewShape = null;
    let lastPreviewUpdate = 0;
    let previewUpdatePending = false;

    // Track cursor movement
    canvas.addEventListener('mousemove', (e) => {
      const worldPos = this.screenToWorld(e.clientX, e.clientY);

      // Notify about cursor movement
      if (this.onCursorMoved) {
        this.onCursorMoved(worldPos.x, worldPos.y);
      }

      // Handle preview shape drawing with RAF throttling
      if (drawStart && previewShape && !previewUpdatePending) {
        previewUpdatePending = true;
        requestAnimationFrame(() => {
          const now = performance.now();
          // Throttle preview updates to 60 FPS
          if (now - lastPreviewUpdate >= 16) {
            const currentWorldPos = this.screenToWorld(e.clientX, e.clientY);
            const width = currentWorldPos.x - drawStart.x;
            const height = currentWorldPos.y - drawStart.y;

            previewShape.clear();

            // PixiJS v8 builder pattern
            if (this.currentTool === 'rectangle') {
              const clamped = clampRectangleDimensions(Math.abs(width), Math.abs(height));
              previewShape.rect(drawStart.x, drawStart.y, clamped.width, clamped.height).fill(this.currentColor);
            } else if (this.currentTool === 'circle') {
              const radius = Math.sqrt(width * width + height * height);
              const clampedRadius = clampRadius(radius);
              previewShape.circle(drawStart.x, drawStart.y, clampedRadius).fill(this.currentColor);
            }
            lastPreviewUpdate = now;
          }
          previewUpdatePending = false;
        });
      }
    });

    canvas.addEventListener('mousedown', (e) => {
      if (e.button !== 0 || e.altKey) return; // Left click only, not panning

      const worldPos = this.screenToWorld(e.clientX, e.clientY);

      if (this.currentTool === 'rectangle' || this.currentTool === 'circle') {
        drawStart = worldPos;

        // Create preview shape with smooth rendering
        previewShape = new PIXI.Graphics();
        previewShape.alpha = 0.5;
        previewShape.roundPixels = false;
        this.viewport.addChild(previewShape);
      } else if (this.currentTool === 'emitter') {
        // Emitters are created immediately on click (no drag needed)
        const objData = this.createToolObject(worldPos, worldPos);

        // Notify about new object (callback to WebSocket)
        if (this.onObjectCreated) {
          this.onObjectCreated(objData);
        }
      }
    });

    canvas.addEventListener('mouseup', (e) => {
      if (drawStart && previewShape) {
        const worldPos = this.screenToWorld(e.clientX, e.clientY);
        
        // Create actual object
        const objData = this.createToolObject(drawStart, worldPos);
        
        // Clean up preview
        this.viewport.removeChild(previewShape);
        previewShape = null;
        drawStart = null;
        
        // Notify about new object (callback to WebSocket)
        if (this.onObjectCreated) {
          this.onObjectCreated(objData);
        }
      }
    });
  }

  // ==================== Viewport Culling ====================

  setupViewportCulling() {
    // Listen for viewport changes (pan and zoom)
    this.app.ticker.add(() => {
      if (this.cullingEnabled) {
        this.updateVisibleObjects();
      }
    });

    // Setup physics prediction loop (runs at 60 FPS with fixed timestep)
    this.app.ticker.add((deltaTime) => {
      if (this.physicsEnabled && !this.physicsEngine.isPaused()) {
        this.updatePhysicsPrediction(deltaTime);
      }
    });
  }

  updateVisibleObjects() {
    // Calculate current viewport bounds in world coordinates
    const viewportBounds = this.getViewportBounds();

    // Check if viewport has changed significantly
    if (!this.lastViewportBounds ||
        Math.abs(viewportBounds.left - this.lastViewportBounds.left) > 50 ||
        Math.abs(viewportBounds.top - this.lastViewportBounds.top) > 50 ||
        Math.abs(viewportBounds.right - this.lastViewportBounds.right) > 50 ||
        Math.abs(viewportBounds.bottom - this.lastViewportBounds.bottom) > 50) {

      this.lastViewportBounds = viewportBounds;

      // Update visibility for all objects
      this.objects.forEach((obj, id) => {
        const objBounds = this.getObjectBounds(obj);
        const isVisible = this.isBoundsVisible(objBounds, viewportBounds);

        // Only change visibility if it actually changed to avoid unnecessary operations
        if (obj.visible !== isVisible) {
          obj.visible = isVisible;
        }
      });
    }
  }

  getViewportBounds() {
    // Get screen dimensions
    const screenWidth = this.app.renderer.width;
    const screenHeight = this.app.renderer.height;

    // Convert screen corners to world coordinates
    const topLeft = this.screenToWorld(0, 0);
    const bottomRight = this.screenToWorld(screenWidth, screenHeight);

    return {
      left: topLeft.x - this.cullingPadding,
      top: topLeft.y - this.cullingPadding,
      right: bottomRight.x + this.cullingPadding,
      bottom: bottomRight.y + this.cullingPadding
    };
  }

  getObjectBounds(obj) {
    // Calculate object bounds based on type
    if (obj.userData) {
      if (obj.userData.type === 'rectangle') {
        return {
          left: obj.x,
          top: obj.y,
          right: obj.x + obj.userData.width,
          bottom: obj.y + obj.userData.height
        };
      } else if (obj.userData.type === 'circle') {
        const radius = obj.userData.radius;
        return {
          left: obj.x - radius,
          top: obj.y - radius,
          right: obj.x + radius,
          bottom: obj.y + radius
        };
      }
    }

    // Fallback for objects without userData (like text)
    if (obj.width && obj.height) {
      return {
        left: obj.x,
        top: obj.y,
        right: obj.x + obj.width,
        bottom: obj.y + obj.height
      };
    }

    // Last resort - treat as point
    return {
      left: obj.x,
      top: obj.y,
      right: obj.x,
      bottom: obj.y
    };
  }

  isBoundsVisible(objBounds, viewportBounds) {
    // Check if object bounds intersect with viewport bounds
    return !(objBounds.right < viewportBounds.left ||
             objBounds.left > viewportBounds.right ||
             objBounds.bottom < viewportBounds.top ||
             objBounds.top > viewportBounds.bottom);
  }

  // ==================== Physics Prediction and Reconciliation ====================

  /**
   * Update physics prediction at 60 FPS using fixed timestep
   * Subtask 5.1: Integrate prediction in animation loop
   */
  updatePhysicsPrediction(deltaTime) {
    // deltaTime is in PIXI frames (60 FPS ≈ 1.0), convert to seconds
    const deltaSeconds = deltaTime / 60.0;
    this.physicsAccumulator += deltaSeconds;

    // Run physics at fixed timestep (50 Hz = 0.02s)
    while (this.physicsAccumulator >= PHYSICS_TIMESTEP) {
      this.physicsEngine.step();
      this.physicsAccumulator -= PHYSICS_TIMESTEP;
    }

    // Sync visual positions from physics engine
    this.syncVisualFromPhysics();
  }

  /**
   * Sync PIXI visual objects from physics engine state
   */
  syncVisualFromPhysics() {
    for (const [id, physicsObj] of this.physicsEngine.objects) {
      const pixiObj = this.objects.get(id);
      if (pixiObj && physicsObj.isDynamic) {
        // Only update dynamic objects (circles)
        pixiObj.x = physicsObj.x;
        pixiObj.y = physicsObj.y;
      }
    }
  }

  /**
   * Handle physics snapshot from server and reconcile
   * Subtasks 5.2 and 5.3: Snapshot reception + reconciliation logic
   */
  handlePhysicsSnapshot(snapshot) {
    if (!this.physicsEnabled) return;

    this.lastServerSnapshot = snapshot;
    this.serverSnapshotTime = performance.now();

    // Reconcile each object in the snapshot
    snapshot.forEach(serverState => {
      const id = serverState.id;
      const physicsObj = this.physicsEngine.getObject(id);
      const pixiObj = this.objects.get(id);

      if (!physicsObj || !pixiObj) {
        // Object doesn't exist locally - create it
        if (pixiObj) {
          // Sync to physics engine
          this.syncObjectToPhysics(id);
        }
        return;
      }

      if (!physicsObj.isDynamic) {
        // Static objects don't need reconciliation
        return;
      }

      // Calculate divergence between predicted and server positions
      const dx = serverState.x - physicsObj.x;
      const dy = serverState.y - physicsObj.y;
      const divergence = Math.sqrt(dx * dx + dy * dy);

      // Reconcile based on divergence magnitude
      if (divergence > 100) {
        // Large divergence - snap immediately (teleport)
        console.warn(`Large divergence detected for ${id}: ${divergence.toFixed(2)}px - snapping`);
        physicsObj.x = serverState.x;
        physicsObj.y = serverState.y;
        physicsObj.oldX = serverState.x;
        physicsObj.oldY = serverState.y;
        pixiObj.x = serverState.x;
        pixiObj.y = serverState.y;
      } else if (divergence > 1) {
        // Small divergence - gentle interpolation using lerp factor
        const lerpX = physicsObj.x + (dx * this.reconciliationLerpFactor);
        const lerpY = physicsObj.y + (dy * this.reconciliationLerpFactor);

        physicsObj.x = lerpX;
        physicsObj.y = lerpY;

        // Maintain velocity by adjusting oldX/oldY
        const vx = physicsObj.x - physicsObj.oldX;
        const vy = physicsObj.y - physicsObj.oldY;
        physicsObj.oldX = lerpX - vx;
        physicsObj.oldY = lerpY - vy;

        pixiObj.x = lerpX;
        pixiObj.y = lerpY;
      }
      // Divergence < 1px: prediction is accurate, no correction needed
    });
  }

  /**
   * Sync a canvas object to the physics engine
   */
  syncObjectToPhysics(id) {
    const pixiObj = this.objects.get(id);
    if (!pixiObj || !pixiObj.userData) return;

    const objData = {
      id,
      type: pixiObj.userData.type,
      x: pixiObj.x,
      y: pixiObj.y,
      color: this.colorToHexString(pixiObj.tint || 0x3498db),
      isDynamic: pixiObj.userData.isDynamic !== undefined ? pixiObj.userData.isDynamic : (pixiObj.userData.type === 'circle')
    };

    if (pixiObj.userData.type === 'circle') {
      objData.radius = pixiObj.userData.radius;
    } else if (pixiObj.userData.type === 'rectangle') {
      objData.width = pixiObj.userData.width;
      objData.height = pixiObj.userData.height;
    }

    this.physicsEngine.syncCanvasObject(objData);
  }

  /**
   * Enable physics simulation
   */
  enablePhysics() {
    if (this.physicsEnabled) return;

    console.log('Enabling physics simulation...');
    this.physicsEnabled = true;

    // Sync all existing objects to physics engine
    this.objects.forEach((pixiObj, id) => {
      this.syncObjectToPhysics(id);
    });

    // Resume physics simulation
    this.physicsEngine.resume();
    console.log(`Physics enabled with ${this.physicsEngine.objects.size} objects`);
  }

  /**
   * Disable physics simulation
   */
  disablePhysics() {
    if (!this.physicsEnabled) return;

    console.log('Disabling physics simulation...');
    this.physicsEnabled = false;
    this.physicsEngine.pause();
  }

  /**
   * Set global gravity for physics engine
   */
  setGravity(gravity) {
    this.physicsEngine.setGlobalGravity(gravity);
    console.log(`Gravity set to: ${gravity}`);
  }

  createToolObject(start, end) {
    const id = this.generateId();

    if (this.currentTool === 'rectangle') {
      const width = Math.abs(end.x - start.x);
      const height = Math.abs(end.y - start.y);
      const clamped = clampRectangleDimensions(width, height); // Apply size limits
      const x = Math.min(start.x, end.x);
      const y = Math.min(start.y, end.y);

      this.createRectangle(id, x, y, clamped.width, clamped.height, this.currentColor);

      return {
        id,
        type: 'rectangle',
        x, y,
        width: clamped.width,
        height: clamped.height,
        color: this.colorToHexString(this.currentColor),
        // Physics properties: rectangles are always static (non-dynamic)
        isDynamic: false,
        friction: 0.02,
        restitution: 0.7
      };
    } else if (this.currentTool === 'circle') {
      const dx = end.x - start.x;
      const dy = end.y - start.y;
      const radius = Math.sqrt(dx * dx + dy * dy);
      const clampedRadius = clampRadius(radius); // Apply size limits

      this.createCircle(id, start.x, start.y, clampedRadius, this.currentColor);

      return {
        id,
        type: 'circle',
        x: start.x,
        y: start.y,
        radius: clampedRadius,
        color: this.colorToHexString(this.currentColor),
        // Physics properties: circles are dynamic by default
        isDynamic: true,
        friction: 0.02,
        restitution: 0.7
        // Note: mass is calculated from radius (π*r²) and not stored
      };
    } else if (this.currentTool === 'emitter') {
      // Use medium emitter preset as default
      const preset = EMITTER_PRESETS.medium;
      const rate = preset.rate;
      const lifespan = preset.lifespan;
      const particleSize = preset.particleSize;
      const initialVelocity = { x: 0, y: -50 }; // upward velocity
      const color = parseInt(preset.color.substring(1), 16); // Convert #ff6600 to 0xff6600

      this.createEmitter(id, start.x, start.y, rate, lifespan, particleSize, initialVelocity, color);

      return {
        id,
        type: 'emitter',
        x: start.x,
        y: start.y,
        rate,
        lifespan,
        particleSize,
        initialVelocity,
        color: preset.color // Use preset color
      };
    }
  }
  
  // ==================== Object Creation ====================
  
  createRectangle(id, x, y, width, height, color) {
    const rect = new PIXI.Graphics();
    // PixiJS v8 builder pattern with anti-aliasing settings
    rect.rect(0, 0, width, height).fill(color);
    rect.x = x;
    rect.y = y;
    rect.interactive = true;
    rect.cursor = 'pointer'; // v8 replaces buttonMode
    rect.visible = true; // Start visible, culling will handle visibility

    // Enable smooth rendering
    rect.roundPixels = false;

    // Store dimensions for selection box and physics properties
    rect.userData = {
      width,
      height,
      type: 'rectangle',
      isDynamic: false // Rectangles are static by default
    };

    this.makeDraggable(rect, id);
    this.makeSelectable(rect, id);

    this.objects.set(id, rect);
    this.viewport.addChild(rect);

    // Sync to physics engine if enabled
    if (this.physicsEnabled) {
      this.syncObjectToPhysics(id);
    }

    return rect;
  }
  
  createCircle(id, x, y, radius, color) {
    const circle = new PIXI.Graphics();
    // PixiJS v8 builder pattern with anti-aliasing settings
    circle.circle(0, 0, radius).fill(color);
    circle.x = x;
    circle.y = y;
    circle.interactive = true;
    circle.cursor = 'pointer'; // v8 replaces buttonMode
    circle.visible = true; // Start visible, culling will handle visibility

    // Enable smooth rendering
    circle.roundPixels = false;

    // Store dimensions for selection box and physics properties
    circle.userData = {
      radius,
      type: 'circle',
      isDynamic: true // Circles are dynamic by default
    };

    this.makeDraggable(circle, id);
    this.makeSelectable(circle, id);

    this.objects.set(id, circle);
    this.viewport.addChild(circle);

    // Sync to physics engine if enabled
    if (this.physicsEnabled) {
      this.syncObjectToPhysics(id);
    }

    return circle;
  }

  createEmitter(id, x, y, rate, lifespan, particleSize, initialVelocity, color) {
    // Create visual representation of the emitter (a glowing star shape)
    const emitter = new PIXI.Container();
    emitter.x = x;
    emitter.y = y;
    emitter.interactive = true;
    emitter.cursor = 'pointer';
    emitter.visible = true;

    // Create star shape for emitter visualization
    const starGraphics = new PIXI.Graphics();
    const numPoints = 8;
    const outerRadius = 20;
    const innerRadius = 10;

    // Draw star using polygon
    const points = [];
    for (let i = 0; i < numPoints * 2; i++) {
      const angle = (i * Math.PI) / numPoints;
      const radius = i % 2 === 0 ? outerRadius : innerRadius;
      points.push({
        x: Math.cos(angle) * radius,
        y: Math.sin(angle) * radius
      });
    }

    starGraphics.poly(points).fill(color);
    starGraphics.circle(0, 0, 5).fill(0xFFFFFF); // White center dot
    starGraphics.roundPixels = false;

    // Add pulsing animation
    let pulseTime = 0;
    const pulseAnimation = (delta) => {
      pulseTime += delta * 0.05;
      const scale = 1 + Math.sin(pulseTime) * 0.1;
      starGraphics.scale.set(scale);
    };

    emitter.addChild(starGraphics);

    // Store emitter properties
    emitter.userData = {
      type: 'emitter',
      rate,
      lifespan,
      particleSize,
      initialVelocity,
      pulseAnimation,
      isDynamic: false // Emitters are static
    };

    // Add pulse animation to ticker
    this.app.ticker.add(pulseAnimation);

    this.makeDraggable(emitter, id);
    this.makeSelectable(emitter, id);

    this.objects.set(id, emitter);
    this.viewport.addChild(emitter);

    return emitter;
  }
  
  createText(id, text, x, y, fontSize, color) {
    const textObj = new PIXI.Text(text, {
      fontSize,
      fill: color,
      fontFamily: 'Arial'
    });
    textObj.x = x;
    textObj.y = y;
    textObj.interactive = true;
    textObj.buttonMode = true;
    textObj.visible = true; // Start visible, culling will handle visibility

    this.makeDraggable(textObj, id);
    this.makeSelectable(textObj, id);

    this.objects.set(id, textObj);
    this.viewport.addChild(textObj);

    return textObj;
  }
  
  // ==================== Interaction ====================
  
  makeDraggable(obj, id) {
    // Only handle pointerdown - centralized handler does the rest
    obj.on('pointerdown', (event) => {
      if (this.currentTool !== 'select') return;

      const worldPos = this.screenToWorld(event.data.global.x, event.data.global.y);

      // Set centralized drag state
      this.isDragging = true;
      this.draggedObject = obj;
      this.draggedObjectId = id;
      this.dragOffset.x = worldPos.x - obj.x;
      this.dragOffset.y = worldPos.y - obj.y;

      obj.alpha = 0.7;
      event.stopPropagation();
    });
  }
  
  makeSelectable(obj, id) {
    obj.on('click', (event) => {
      if (this.currentTool === 'select') {
        if (event.data.originalEvent.shiftKey) {
          // Multi-select
          if (this.selectedObjects.has(id)) {
            this.deselectObject(id);
          } else {
            this.selectObject(id);
          }
        } else {
          // Single select
          this.clearSelection();
          this.selectObject(id);
        }
        event.stopPropagation();
      }
    });
  }
  
  selectObject(id) {
    const obj = this.objects.get(id);
    if (!obj) return;

    this.selectedObjects.add(id);

    // Trigger selection change callback
    if (this.onSelectionChange) {
      this.onSelectionChange(this.selectedObjects);
    }

    // Remove existing selection indicator if any
    const existingIndicator = this.selectionIndicators.get(id);
    if (existingIndicator) {
      this.viewport.removeChild(existingIndicator);
      existingIndicator.destroy();
    }

    // Create selection indicator
    const indicator = new PIXI.Graphics();
    
    // Enable smooth rendering for selection borders
    indicator.roundPixels = false;

    if (obj.userData) {
      if (obj.userData.type === 'rectangle') {
        // Draw selection box around rectangle using v8 builder pattern
        const { width, height } = obj.userData;
        indicator.rect(-2, -2, width + 4, height + 4).stroke({ width: 2, color: 0x00FF00, alignment: 0.5 });
        indicator.x = obj.x;
        indicator.y = obj.y;
      } else if (obj.userData.type === 'circle') {
        // Draw selection box around circle using v8 builder pattern
        const { radius } = obj.userData;
        indicator.circle(0, 0, radius + 2).stroke({ width: 2, color: 0x00FF00, alignment: 0.5 });
        indicator.x = obj.x;
        indicator.y = obj.y;
      }
    }

    // Selection indicators are non-interactive - optimize event traversal
    indicator.interactive = false;
    indicator.interactiveChildren = false;

    // Add to viewport and store reference
    this.viewport.addChild(indicator);
    this.selectionIndicators.set(id, indicator);
  }

  deselectObject(id) {
    const obj = this.objects.get(id);
    if (!obj) return;

    this.selectedObjects.delete(id);

    // Trigger selection change callback
    if (this.onSelectionChange) {
      this.onSelectionChange(this.selectedObjects);
    }

    // Remove selection indicator
    const indicator = this.selectionIndicators.get(id);
    if (indicator) {
      this.viewport.removeChild(indicator);
      indicator.destroy();
      this.selectionIndicators.delete(id);
    }
  }
  
  clearSelection() {
    this.selectedObjects.forEach(id => this.deselectObject(id));
    this.selectedObjects.clear();

    // Trigger selection change callback (empty selection)
    if (this.onSelectionChange) {
      this.onSelectionChange(this.selectedObjects);
    }
  }
  
  deleteSelected() {
    if (this.selectedObjects.size === 0) {
      return;
    }

    const idsToDelete = Array.from(this.selectedObjects);

    // Use bulk deletion for better performance
    const deletedIds = this.deleteObjects(idsToDelete);

    // Clear selection
    this.selectedObjects.clear();

    // Notify about bulk deletion
    if (this.onObjectsDeleted && deletedIds.length > 0) {
      this.onObjectsDeleted(deletedIds);
    }

    console.log(`Deleted ${deletedIds.length} selected objects`);
  }
  
  // ==================== Object Management ====================

  loadState(canvasState) {
    console.error('========================================');
    console.error('=== LOAD STATE CALLED ===');
    console.error('========================================');
    console.error('Canvas state received:', canvasState);
    console.error('Canvas state type:', typeof canvasState);
    console.error('Is array?', Array.isArray(canvasState));

    // Clear existing objects
    console.error('Clearing existing objects...');
    this.objects.forEach((obj, id) => {
      this.deleteObject(id);
    });
    console.error('Objects cleared. Map size:', this.objects.size);

    // Load objects from state
    if (canvasState && typeof canvasState === 'object') {
      // If it's an array, iterate through it
      if (Array.isArray(canvasState)) {
        console.error(`=== Loading ${canvasState.length} objects from ARRAY ===`);
        canvasState.forEach((objData, index) => {
          console.error(`Loading object ${index}:`, JSON.stringify(objData));
          this.createRemoteObject(objData);
        });
      } else {
        // If it's an object/hash, iterate through its values
        const values = Object.values(canvasState);
        console.error(`=== Loading ${values.length} objects from OBJECT ===`);
        values.forEach((objData, index) => {
          console.error(`Loading object ${index}:`, JSON.stringify(objData));
          this.createRemoteObject(objData);
        });
      }
    } else {
      console.error('!!! INVALID canvas state !!!:', canvasState);
    }

    console.error('========================================');
    console.error(`=== LOAD STATE COMPLETE: ${this.objects.size} objects ===`);
    console.error('Current objects in map:', Array.from(this.objects.keys()));
    console.error('========================================');

    // Trigger viewport culling after loading objects
    if (this.cullingEnabled) {
      this.updateVisibleObjects();
    }
  }

  applyDelta(id, delta) {
    const obj = this.objects.get(id);
    if (!obj) return;

    // Apply position and visual properties
    for (const [key, value] of Object.entries(delta)) {
      if (key === 'x' || key === 'y') {
        obj[key] = value;
      }
    }

    // Apply physics properties to userData
    if (delta.isDynamic !== undefined && obj.userData) {
      obj.userData.isDynamic = delta.isDynamic;
    }
    if (delta.friction !== undefined && obj.userData) {
      obj.userData.friction = delta.friction;
    }
    if (delta.restitution !== undefined && obj.userData) {
      obj.userData.restitution = delta.restitution;
    }

    // Handle special cases for Graphics objects
    if (obj instanceof PIXI.Graphics) {
      if (delta.width !== undefined || delta.height !== undefined ||
          delta.color !== undefined || delta.rotation !== undefined) {
        // Trigger redraw for visual properties
        this.redrawGraphicsObject(obj);
      }
    }

    // Sync to physics engine if physics is enabled
    if (this.physicsEnabled && (delta.isDynamic !== undefined || delta.friction !== undefined || delta.restitution !== undefined)) {
      this.syncObjectToPhysics(id);
    }
  }

  redrawGraphicsObject(obj) {
    // Clear and redraw the graphics object based on its current properties
    obj.clear();

    // Basic rectangle drawing using v8 builder pattern
    if (obj.width && obj.height) {
      obj.rect(0, 0, obj.width, obj.height).fill(obj.color || 0xFF0000);
    }

    // Apply rotation if set
    if (obj.rotation) {
      obj.rotation = obj.rotation;
    }
  }

  updateObject(id, updates) {
    const obj = this.objects.get(id);
    if (!obj) return;

    if (updates.x !== undefined) obj.x = updates.x;
    if (updates.y !== undefined) obj.y = updates.y;

    // For Graphics objects, need to redraw if dimensions change
    if (obj instanceof PIXI.Graphics) {
      if (updates.width !== undefined || updates.height !== undefined) {
        // Recreate the shape - this is simplistic, you might want a better approach
        console.log('Dimension updates for Graphics require recreation');
      }
    }
  }

  deleteObject(id) {
    const obj = this.objects.get(id);
    if (obj) {
      this.viewport.removeChild(obj);
      this.objects.delete(id);

      // Destroy object but preserve shared textures
      obj.destroy({ children: true, texture: false, baseTexture: false });

      // Clean up selection indicator if it exists
      const indicator = this.selectionIndicators.get(id);
      if (indicator) {
        this.viewport.removeChild(indicator);
        indicator.destroy({ children: true, texture: false, baseTexture: false });
        this.selectionIndicators.delete(id);
      }

      // Remove from selected objects set
      this.selectedObjects.delete(id);

      console.log(`Deleted object ${id} with proper texture preservation`);
    }
  }

  deleteObjects(ids) {
    if (!Array.isArray(ids) || ids.length === 0) {
      console.warn('deleteObjects: Expected non-empty array of IDs');
      return;
    }

    console.log(`Bulk deleting ${ids.length} objects:`, ids);

    // Collect objects to delete for broadcasting
    const deletedObjects = [];

    // Delete each object
    ids.forEach(id => {
      const obj = this.objects.get(id);
      if (obj) {
        this.viewport.removeChild(obj);
        this.objects.delete(id);

        // Destroy object but preserve shared textures
        obj.destroy({ children: true, texture: false, baseTexture: false });

        // Clean up selection indicator if it exists
        const indicator = this.selectionIndicators.get(id);
        if (indicator) {
          this.viewport.removeChild(indicator);
          indicator.destroy({ children: true, texture: false, baseTexture: false });
          this.selectionIndicators.delete(id);
        }

        // Remove from selected objects set
        this.selectedObjects.delete(id);

        deletedObjects.push(id);
      } else {
        console.warn(`Object ${id} not found for deletion`);
      }
    });

    console.log(`Bulk deleted ${deletedObjects.length} objects successfully`);

    // Return deleted IDs for broadcasting
    return deletedObjects;
  }

  verifyObjectDeletion(ids) {
    if (!Array.isArray(ids)) {
      ids = [ids];
    }

    let allClean = true;
    const issues = [];

    ids.forEach(id => {
      // Check if object still exists in maps
      if (this.objects.has(id)) {
        issues.push(`Object ${id} still in objects map`);
        allClean = false;
      }

      if (this.selectionIndicators.has(id)) {
        issues.push(`Selection indicator for ${id} still exists`);
        allClean = false;
      }

      if (this.selectedObjects.has(id)) {
        issues.push(`Object ${id} still in selectedObjects set`);
        allClean = false;
      }

      // Check if PIXI object still has parent (indicating it's still in scene)
      const obj = this.viewport.children.find(child => child._objectId === id);
      if (obj) {
        issues.push(`Object ${id} still in viewport children`);
        allClean = false;
      }
    });

    // Check for orphaned PIXI objects in viewport
    const orphanedPixiObjects = this.viewport.children.filter(child => {
      // Look for objects that don't have corresponding entries in our maps
      return child._objectId && !this.objects.has(child._objectId);
    });

    if (orphanedPixiObjects.length > 0) {
      issues.push(`${orphanedPixiObjects.length} orphaned PIXI objects found in viewport`);
      allClean = false;
    }

    // Check texture memory (basic check)
    // Note: In PixiJS v8, texture cache access has changed
    const textureCount = PIXI.Cache ? Object.keys(PIXI.Cache._cache).length : 0;
    if (textureCount > 100) { // Arbitrary threshold
      issues.push(`High texture count detected: ${textureCount} textures in cache`);
    }

    if (!allClean) {
      console.warn('Memory leak verification failed:', issues);
    } else {
      console.log(`Memory leak verification passed for ${ids.length} deleted objects`);
    }

    return {
      success: allClean,
      issues: issues,
      textureCount: textureCount,
      orphanedObjects: orphanedPixiObjects.length
    };
  }

  getMemoryStats() {
    const pixiChildren = this.viewport.children.length;
    const objectsInMap = this.objects.size;
    const selectionIndicators = this.selectionIndicators.size;
    const selectedObjects = this.selectedObjects.size;
    const remoteCursors = this.remoteCursors.size;

    return {
      pixiChildren,
      objectsInMap,
      selectionIndicators,
      selectedObjects,
      remoteCursors,
      totalTrackedObjects: objectsInMap + selectionIndicators + selectedObjects + remoteCursors,
      // PixiJS v8: Cache API has changed
      textureCacheSize: PIXI.Cache ? Object.keys(PIXI.Cache._cache).length : 0,
      baseTextureCacheSize: 0 // BaseTextureCache deprecated in v8
    };
  }

  // ==================== Remote Object Sync ====================

  createRemoteObject(objData) {
    console.log('Creating remote object:', objData);
    console.log('Object properties:', {
      id: objData.id,
      type: objData.type,
      x: objData.x,
      y: objData.y,
      width: objData.width,
      height: objData.height,
      radius: objData.radius,
      color: objData.color,
      colorType: typeof objData.color
    });

    // Convert color to proper format (ensure it's a number)
    const color = this.normalizeColor(objData.color);
    console.log('Normalized color:', color, 'type:', typeof color);

    // Infer type from data if missing (backwards compatibility)
    let type = objData.type;
    if (!type) {
      console.warn('Object missing type field, inferring from properties:', objData);
      if (objData.radius !== undefined) {
        type = 'circle';
      } else if (objData.width !== undefined && objData.height !== undefined) {
        type = 'rectangle';
      } else if (objData.text !== undefined) {
        type = 'text';
      } else {
        console.error('Cannot infer type for object:', objData);
        return;
      }
    }

    if (type === 'rectangle') {
      // Validate dimensions
      if (!objData.width || !objData.height || objData.width <= 0 || objData.height <= 0) {
        console.warn('Skipping rectangle with invalid dimensions:', objData);
        return;
      }

      console.log('Creating rectangle with:', {
        id: objData.id,
        x: objData.x,
        y: objData.y,
        width: objData.width,
        height: objData.height,
        color: color
      });
      this.createRectangle(
        objData.id,
        objData.x,
        objData.y,
        objData.width,
        objData.height,
        color
      );
      console.log('Rectangle created successfully. Objects in map:', this.objects.size);
    } else if (type === 'circle') {
      console.log('Creating circle with:', {
        id: objData.id,
        x: objData.x,
        y: objData.y,
        radius: objData.radius,
        color: color
      });
      this.createCircle(
        objData.id,
        objData.x,
        objData.y,
        objData.radius,
        color
      );
      console.log('Circle created successfully');
    } else if (type === 'text') {
      console.log('Creating text with:', {
        id: objData.id,
        text: objData.text,
        x: objData.x,
        y: objData.y,
        fontSize: objData.fontSize,
        color: color
      });
      this.createText(
        objData.id,
        objData.text,
        objData.x,
        objData.y,
        objData.fontSize,
        color
      );
      console.log('Text created successfully');
    } else {
      console.error('Unknown object type:', type);
    }
  }

  normalizeColor(color) {
    // If it's already a number (0xRRGGBB format), return it
    if (typeof color === 'number') {
      return color;
    }

    // If it's a hex string like "#3498db", convert to number
    if (typeof color === 'string') {
      if (color.startsWith('#')) {
        return parseInt(color.substring(1), 16);
      }
      // If it's a string number like "3498db", convert to number
      return parseInt(color, 16);
    }

    // Default color if something goes wrong
    return 0x3498db;
  }

  updateRemoteObject(objectId, delta) {
    console.log('Updating remote object with delta:', objectId, delta);
    this.applyDelta(objectId, delta);
  }

  deleteRemoteObject(objectId) {
    console.log('Deleting remote object:', objectId);
    this.deleteObject(objectId);
  }
  
  getObject(id) {
    return this.objects.get(id);
  }

  // Alias for compatibility with main.js
  getObjectById(id) {
    return this.objects.get(id);
  }

  getAllObjects() {
    return Array.from(this.objects.entries()).map(([id, obj]) => ({
      id,
      type: obj.constructor.name,
      x: obj.x,
      y: obj.y
    }));
  }

  getPerformanceStats() {
    return this.performanceMonitor.getStats();
  }
  
  // ==================== Remote Cursors ====================
  
  updateRemoteCursor(userId, username, x, y, color) {
    let cursor = this.remoteCursors.get(userId);

    // Convert color string (like "#FF6B6B") to number
    const colorNum = color && typeof color === 'string' && color.startsWith('#')
      ? parseInt(color.substring(1), 16)
      : 0xFF6B6B;

    if (!cursor) {
      // Create new cursor
      cursor = new PIXI.Container();

      // Cursor pointer using shared texture and sprite
      const pointer = new PIXI.Sprite(this.cursorTexture);
      pointer.tint = colorNum; // Apply color using tint instead of recreating graphics

      // Username label
      const label = new PIXI.Text(username, {
        fontSize: 12,
        fill: 0xFFFFFF,
        fontWeight: 'bold',
        stroke: colorNum,
        strokeThickness: 2,
        dropShadow: true,
        dropShadowColor: 0x000000,
        dropShadowAlpha: 0.7,
        dropShadowDistance: 1
      });
      label.x = 15;
      label.y = -5; // Position above cursor tip

      cursor.addChild(pointer);
      cursor.addChild(label);
      cursor.zIndex = 1000;

      // Remote cursors are non-interactive - optimize event traversal
      cursor.interactive = false;
      cursor.interactiveChildren = false;

      this.remoteCursors.set(userId, cursor);
      this.viewport.addChild(cursor);
    }

    // Add interpolation for smooth rendering despite batching/throttling
    if (!cursor.lastUpdate) {
      cursor.lastUpdate = { x, y, time: performance.now() };
    }

    const now = performance.now();
    const dt = now - cursor.lastUpdate.time;

    if (dt > 0 && dt < 100) { // Interpolate over last 100ms for smoothness
      const lerpFactor = Math.min(dt / 100, 1);
      cursor.x = cursor.lastUpdate.x + (x - cursor.lastUpdate.x) * lerpFactor;
      cursor.y = cursor.lastUpdate.y + (y - cursor.lastUpdate.y) * lerpFactor;
    } else {
      // Large time gap or first update - jump to position
      cursor.x = x;
      cursor.y = y;
    }

    cursor.lastUpdate = { x, y, time: now };
  }

  removeRemoteCursor(userId) {
    const cursor = this.remoteCursors.get(userId);
    if (cursor) {
      this.viewport.removeChild(cursor);
      cursor.destroy();
      this.remoteCursors.delete(userId);
    }
  }

  clearAllRemoteCursors() {
    console.log(`Clearing all remote cursors (${this.remoteCursors.size} cursors)`)
    this.remoteCursors.forEach((cursor, userId) => {
      this.viewport.removeChild(cursor)
      cursor.destroy()
    })
    this.remoteCursors.clear()
    console.log('All remote cursors cleared')
  }

  startPeriodicCleanup(intervalMs = 60000) {
    // Run cleanup every minute to catch any orphaned objects
    this.cleanupInterval = setInterval(() => {
      this.performCleanup()
    }, intervalMs)
    console.log(`Started periodic cleanup (every ${intervalMs / 1000}s)`)
  }

  stopPeriodicCleanup() {
    if (this.cleanupInterval) {
      clearInterval(this.cleanupInterval)
      this.cleanupInterval = null
      console.log('Stopped periodic cleanup')
    }
  }

  performCleanup() {
    console.log('=== Performing periodic cleanup ===')
    let orphanedIndicators = 0
    let orphanedCursors = 0

    // Clean up selection indicators for deleted objects
    this.selectionIndicators.forEach((indicator, id) => {
      if (!this.objects.has(id)) {
        this.viewport.removeChild(indicator)
        indicator.destroy()
        this.selectionIndicators.delete(id)
        orphanedIndicators++
      }
    })

    // Clean up selected objects that no longer exist
    this.selectedObjects.forEach(id => {
      if (!this.objects.has(id)) {
        this.selectedObjects.delete(id)
      }
    })

    // Clean up remote cursors that haven't updated in a while (>5 minutes)
    const fiveMinutesAgo = performance.now() - (5 * 60 * 1000)
    this.remoteCursors.forEach((cursor, userId) => {
      if (cursor.lastUpdate && cursor.lastUpdate.time < fiveMinutesAgo) {
        this.removeRemoteCursor(userId)
        orphanedCursors++
      }
    })

    if (orphanedIndicators > 0 || orphanedCursors > 0) {
      console.log(`Cleanup complete: ${orphanedIndicators} indicators, ${orphanedCursors} cursors removed`)
    } else {
      console.log('Cleanup complete: No orphaned objects found')
    }
  }

  // ==================== Utilities ====================
  
  generateId() {
    return 'obj-' + Math.random().toString(36).substr(2, 9);
  }
  
  setColor(color) {
    this.currentColor = color;
  }

  colorToHexString(color) {
    // Convert JavaScript color number (0x3498db) to CSS hex string ("#3498db")
    if (typeof color === 'number') {
      return '#' + color.toString(16).padStart(6, '0');
    }
    // If it's already a string with #, return as-is
    if (typeof color === 'string' && color.startsWith('#')) {
      return color;
    }
    // Default fallback
    return '#3498db';
  }

  getCanvasState() {
    const objects = [];
    this.objects.forEach((obj, id) => {
      // Serialize object data
      objects.push({
        id,
        type: obj.constructor.name,
        x: obj.x,
        y: obj.y,
        // Add more properties as needed
      });
    });
    return { objects };
  }

  destroy() {
    // Clean up performance monitor
    if (this.performanceMonitor) {
      this.performanceMonitor.destroy();
    }

    // Clean up shared cursor texture
    if (this.cursorTexture) {
      this.cursorTexture.destroy();
    }

    // Clean up PIXI objects
    this.objects.forEach(obj => {
      if (obj && typeof obj.destroy === 'function') {
        obj.destroy();
      }
    });
    this.objects.clear();

    this.selectionIndicators.forEach(indicator => {
      if (indicator && typeof indicator.destroy === 'function') {
        indicator.destroy();
      }
    });
    this.selectionIndicators.clear();

    this.remoteCursors.forEach(cursor => {
      if (cursor && typeof cursor.destroy === 'function') {
        cursor.destroy();
      }
    });
    this.remoteCursors.clear();
  }
  
  // Callbacks - set these from outside
  onObjectCreated = null;
  onObjectMoved = null;
  onObjectDeleted = null;
  onCursorMoved = null;
  onSelectionChange = null; // Callback for physics properties UI
}
