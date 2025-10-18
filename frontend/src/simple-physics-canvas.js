// simple-physics-canvas.js - Minimal Physics Canvas for Debugging
// Stripped down version focused ONLY on physics ball rendering

import * as PIXI from 'pixi.js';

/**
 * SimplePhysicsCanvas - Minimal canvas manager for debugging physics rendering
 * 
 * Features:
 * - Basic PixiJS initialization
 * - Physics ball rendering with visible IDs
 * - Server position updates with interpolation
 * - Pan/zoom controls (middle mouse button)
 * - Extensive debug logging
 * 
 * Excluded (for now):
 * - Manual shape creation tools
 * - Selection/dragging
 * - Remote cursors
 * - Grid background
 * - Performance optimizations
 */
export class SimplePhysicsCanvas {
    constructor(app) {
        console.log('[SimplePhysicsCanvas] Initializing...');
        
        this.app = app;
        
        // Create viewport container for pan/zoom
        this.viewport = new PIXI.Container();
        this.viewport.sortableChildren = true; // Enable z-index sorting
        this.app.stage.addChild(this.viewport);
        
        // Ball storage: ballId -> { graphics, serverX, serverY, currentX, currentY, radius }
        this.balls = new Map();
        
        // Pan/zoom state
        this.viewportX = 0;
        this.viewportY = 0;
        this.zoom = 1.0;
        
        // Interpolation settings
        this.interpolationFactor = 0.3; // Smooth interpolation
        
        // Setup controls and rendering
        this.setupPanZoom();
        this.setupRenderLoop();
        this.setupDebugVisualization();
        
        console.log('[SimplePhysicsCanvas] Initialized successfully');
        console.log('[SimplePhysicsCanvas] Canvas size:', this.app.screen.width, 'x', this.app.screen.height);
    }
    
    // ==================== Pan/Zoom Controls ====================
    
    setupPanZoom() {
        console.log('[SimplePhysicsCanvas] Setting up pan/zoom controls...');
        
        let isPanning = false;
        let lastPanX = 0;
        let lastPanY = 0;
        
        // Middle mouse button for panning
        this.app.canvas.addEventListener('mousedown', (e) => {
            if (e.button === 1) { // Middle mouse button
                isPanning = true;
                lastPanX = e.clientX;
                lastPanY = e.clientY;
                e.preventDefault();
                console.log('[Pan] Started at', lastPanX, lastPanY);
            }
        });
        
        this.app.canvas.addEventListener('mousemove', (e) => {
            if (isPanning) {
                const deltaX = e.clientX - lastPanX;
                const deltaY = e.clientY - lastPanY;
                
                this.viewportX += deltaX;
                this.viewportY += deltaY;
                
                this.viewport.x = this.viewportX;
                this.viewport.y = this.viewportY;
                
                lastPanX = e.clientX;
                lastPanY = e.clientY;
            }
        });
        
        this.app.canvas.addEventListener('mouseup', (e) => {
            if (e.button === 1) {
                isPanning = false;
                console.log('[Pan] Stopped at viewport:', this.viewportX, this.viewportY);
            }
        });
        
        // Mouse wheel for zooming
        this.app.canvas.addEventListener('wheel', (e) => {
            e.preventDefault();
            
            const zoomFactor = e.deltaY < 0 ? 1.1 : 0.9;
            const newZoom = Math.max(0.1, Math.min(5.0, this.zoom * zoomFactor));
            
            if (newZoom !== this.zoom) {
                // Zoom towards mouse position
                const mouseX = e.clientX - this.viewportX;
                const mouseY = e.clientY - this.viewportY;
                
                this.zoom = newZoom;
                this.viewport.scale.set(this.zoom);
                
                console.log('[Zoom] New zoom level:', this.zoom.toFixed(2));
            }
        });
        
        console.log('[SimplePhysicsCanvas] Pan/zoom controls ready (middle mouse to pan, wheel to zoom)');
    }
    
    // ==================== Render Loop ====================
    
    setupRenderLoop() {
        console.log('[SimplePhysicsCanvas] Setting up render loop...');
        
        this.app.ticker.add(() => {
            this.updateBallInterpolation();
        });
        
        console.log('[SimplePhysicsCanvas] Render loop started');
    }
    
    updateBallInterpolation() {
        // Interpolate ball positions for smooth 60 FPS rendering
        this.balls.forEach((ball, ballId) => {
            if (ball.serverX !== undefined && ball.serverY !== undefined) {
                // Smooth interpolation
                ball.currentX += (ball.serverX - ball.currentX) * this.interpolationFactor;
                ball.currentY += (ball.serverY - ball.currentY) * this.interpolationFactor;
                
                // Update graphics position
                ball.graphics.x = ball.currentX;
                ball.graphics.y = ball.currentY;
            }
        });
    }
    
    // ==================== Ball Creation & Updates ====================
    
    /**
     * Create a new physics ball
     * @param {string} ballId - Unique identifier (e.g., "ball-123")
     * @param {number} x - Initial x position
     * @param {number} y - Initial y position
     * @param {number} radius - Ball radius (default 20)
     * @param {number} color - Ball color (default blue)
     */
    createBall(ballId, x, y, radius = 20, color = 0x3498db) {
        console.log(`[CreateBall] Creating ball ${ballId} at (${x.toFixed(1)}, ${y.toFixed(1)}) radius=${radius}`);
        
        // Remove existing ball if it exists
        if (this.balls.has(ballId)) {
            console.warn(`[CreateBall] Ball ${ballId} already exists, removing old one`);
            this.removeBall(ballId);
        }
        
        // Create graphics object
        const graphics = new PIXI.Graphics();
        
        // Draw filled circle
        graphics.circle(0, 0, radius);
        graphics.fill({ color: color, alpha: 1.0 });
        
        // Draw outline
        graphics.circle(0, 0, radius);
        graphics.stroke({ width: 2, color: 0x000000 });
        
        // Add ID label (very visible for debugging)
        const idText = ballId.replace('ball-', ''); // Show just the number
        const label = new PIXI.Text({
            text: idText,
            style: {
                fontSize: radius * 0.6,
                fill: 0xFFFFFF,
                fontWeight: 'bold',
                align: 'center'
            }
        });
        label.anchor.set(0.5);
        label.x = 0;
        label.y = 0;
        graphics.addChild(label);
        
        // Position the ball
        graphics.x = x;
        graphics.y = y;
        
        // Make ball interactive (for future selection)
        graphics.interactive = true;
        graphics.cursor = 'pointer';
        graphics.eventMode = 'static';
        
        // Store ball state
        const ballState = {
            graphics: graphics,
            serverX: x,
            serverY: y,
            currentX: x,
            currentY: y,
            radius: radius,
            color: color
        };
        
        this.balls.set(ballId, ballState);
        this.viewport.addChild(graphics);
        
        console.log(`[CreateBall] ✓ Ball ${ballId} created successfully, total balls: ${this.balls.size}`);
        console.log(`[CreateBall]   Graphics added to viewport:`, graphics.x, graphics.y);
        console.log(`[CreateBall]   Viewport children count:`, this.viewport.children.length);
        
        return graphics;
    }
    
    /**
     * Update ball position from server
     * @param {string} ballId - Ball identifier
     * @param {number} x - New server x position
     * @param {number} y - New server y position
     */
    updateBall(ballId, x, y) {
        const ball = this.balls.get(ballId);
        
        if (ball) {
            // Update server position (interpolation happens in render loop)
            ball.serverX = x;
            ball.serverY = y;
            // Note: Don't log every update, it's too spammy at 20Hz
        } else {
            console.warn(`[UpdateBall] Ball ${ballId} not found, creating it`);
            this.createBall(ballId, x, y);
        }
    }
    
    /**
     * Remove a ball
     * @param {string} ballId - Ball identifier
     */
    removeBall(ballId) {
        console.log(`[RemoveBall] Removing ball ${ballId}`);
        
        const ball = this.balls.get(ballId);
        if (ball && ball.graphics) {
            this.viewport.removeChild(ball.graphics);
            ball.graphics.destroy({ children: true });
            this.balls.delete(ballId);
            console.log(`[RemoveBall] ✓ Ball ${ballId} removed, remaining balls: ${this.balls.size}`);
        } else {
            console.warn(`[RemoveBall] Ball ${ballId} not found`);
        }
    }
    
    /**
     * Clear all balls
     */
    clearAllBalls() {
        console.log('[ClearAllBalls] Clearing all balls...');
        const count = this.balls.size;
        
        this.balls.forEach((ball, ballId) => {
            if (ball.graphics) {
                this.viewport.removeChild(ball.graphics);
                ball.graphics.destroy({ children: true });
            }
        });
        
        this.balls.clear();
        console.log(`[ClearAllBalls] ✓ Cleared ${count} balls`);
    }
    
    // ==================== Physics Message Handler ====================
    
    /**
     * Handle physics-delta message from server
     * @param {Object} data - Physics delta with entities array
     */
    handlePhysicsDelta(data) {
        if (!data) {
            console.error('[PhysicsDelta] Received null/undefined data');
            return;
        }
        
        if (!data.entities) {
            console.error('[PhysicsDelta] Missing entities array in data:', data);
            return;
        }
        
        console.log(`[PhysicsDelta] Processing ${data.entities.length} entities`);
        
        // Process each entity
        data.entities.forEach((entity, index) => {
            const entityId = entity['entity-id'];
            const ballId = `ball-${entityId}`;
            const x = entity.x;
            const y = entity.y;
            const radius = entity.radius || 20;
            
            // Log first entity for debugging
            if (index === 0) {
                console.log(`[PhysicsDelta] Entity[0]: id=${entityId}, pos=(${x.toFixed(1)}, ${y.toFixed(1)}), radius=${radius}`);
            }
            
            // Check if ball exists
            const existingBall = this.balls.get(ballId);
            
            if (existingBall) {
                // Update existing ball
                this.updateBall(ballId, x, y);
            } else {
                // Create new ball
                console.log(`[PhysicsDelta] Creating new ball ${ballId}`);
                this.createBall(ballId, x, y, radius);
            }
        });
        
        console.log(`[PhysicsDelta] ✓ Processed, total balls: ${this.balls.size}`);
    }
    
    // ==================== Debug Visualization ====================
    
    setupDebugVisualization() {
        console.log('[Debug] Setting up debug visualization...');
        
        // Draw coordinate system origin
        const origin = new PIXI.Graphics();
        
        // X-axis (red)
        origin.moveTo(-100, 0);
        origin.lineTo(100, 0);
        origin.stroke({ width: 3, color: 0xFF0000 });
        
        // Y-axis (green)
        origin.moveTo(0, -100);
        origin.lineTo(0, 100);
        origin.stroke({ width: 3, color: 0x00FF00 });
        
        // Origin label
        const originLabel = new PIXI.Text({
            text: '(0, 0)',
            style: {
                fontSize: 16,
                fill: 0xFFFFFF,
                fontWeight: 'bold'
            }
        });
        originLabel.x = 10;
        originLabel.y = 10;
        origin.addChild(originLabel);
        
        this.viewport.addChild(origin);
        
        // Log ball count periodically
        this.debugLogInterval = setInterval(() => {
            if (this.balls.size > 0) {
                console.log(`[Debug] Active balls: ${this.balls.size}`);
                // Log first few balls
                let count = 0;
                this.balls.forEach((ball, id) => {
                    if (count < 3) {
                        console.log(`  ${id}: current=(${ball.currentX.toFixed(1)}, ${ball.currentY.toFixed(1)}) server=(${ball.serverX.toFixed(1)}, ${ball.serverY.toFixed(1)})`);
                        count++;
                    }
                });
            }
        }, 5000);
        
        console.log('[Debug] Debug visualization ready (origin axes visible, ball count logged every 5s)');
    }
    
    // ==================== Cleanup ====================
    
    destroy() {
        console.log('[SimplePhysicsCanvas] Destroying...');
        
        // Clear debug interval
        if (this.debugLogInterval) {
            clearInterval(this.debugLogInterval);
        }
        
        // Clear all balls
        this.clearAllBalls();
        
        // Destroy viewport
        this.viewport.destroy({ children: true });
        
        console.log('[SimplePhysicsCanvas] Destroyed');
    }
}
