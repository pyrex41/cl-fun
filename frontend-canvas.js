// src/canvas.js
// Complete PixiJS Canvas Manager for CollabCanvas
import * as PIXI from 'pixi.js';

export class CanvasManager {
  constructor(app) {
    this.app = app;
    this.viewport = new PIXI.Container();
    this.objects = new Map(); // objectId -> PIXI Graphics
    this.selectedObjects = new Set();
    this.remoteCursors = new Map(); // userId -> cursor container
    
    // State
    this.isPanning = false;
    this.panStart = { x: 0, y: 0 };
    this.currentTool = 'select'; // 'select', 'rectangle', 'circle', 'text'
    this.currentColor = 0x3498db; // Default blue
    
    // Viewport setup
    this.app.stage.addChild(this.viewport);
    this.viewport.sortableChildren = true;
    
    // Grid background (optional visual aid)
    this.drawGrid();
    
    // Setup interaction
    this.setupPanZoom();
    this.setupKeyboardShortcuts();
    this.setupToolHandlers();
    
    console.log('Canvas initialized');
  }
  
  // ==================== Grid ====================
  
  drawGrid() {
    const grid = new PIXI.Graphics();
    grid.lineStyle(1, 0x333333, 0.3);
    
    const gridSize = 50;
    const gridExtent = 5000;
    
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
    
    grid.zIndex = -1;
    this.viewport.addChild(grid);
  }
  
  // ==================== Pan & Zoom ====================
  
  setupPanZoom() {
    const canvas = this.app.view;
    
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
    const canvas = this.app.view;
    let drawStart = null;
    let previewShape = null;
    
    canvas.addEventListener('mousedown', (e) => {
      if (e.button !== 0 || e.altKey) return; // Left click only, not panning
      
      const worldPos = this.screenToWorld(e.clientX, e.clientY);
      
      if (this.currentTool === 'rectangle' || this.currentTool === 'circle') {
        drawStart = worldPos;
        
        // Create preview shape
        previewShape = new PIXI.Graphics();
        previewShape.alpha = 0.5;
        this.viewport.addChild(previewShape);
      }
    });
    
    canvas.addEventListener('mousemove', (e) => {
      if (drawStart && previewShape) {
        const worldPos = this.screenToWorld(e.clientX, e.clientY);
        const width = worldPos.x - drawStart.x;
        const height = worldPos.y - drawStart.y;
        
        previewShape.clear();
        previewShape.beginFill(this.currentColor);
        
        if (this.currentTool === 'rectangle') {
          previewShape.drawRect(drawStart.x, drawStart.y, width, height);
        } else if (this.currentTool === 'circle') {
          const radius = Math.sqrt(width * width + height * height);
          previewShape.drawCircle(drawStart.x, drawStart.y, radius);
        }
        
        previewShape.endFill();
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
  
  createToolObject(start, end) {
    const id = this.generateId();
    
    if (this.currentTool === 'rectangle') {
      const width = Math.abs(end.x - start.x);
      const height = Math.abs(end.y - start.y);
      const x = Math.min(start.x, end.x);
      const y = Math.min(start.y, end.y);
      
      this.createRectangle(id, x, y, width, height, this.currentColor);
      
      return {
        id,
        type: 'rectangle',
        x, y, width, height,
        color: this.currentColor
      };
    } else if (this.currentTool === 'circle') {
      const dx = end.x - start.x;
      const dy = end.y - start.y;
      const radius = Math.sqrt(dx * dx + dy * dy);
      
      this.createCircle(id, start.x, start.y, radius, this.currentColor);
      
      return {
        id,
        type: 'circle',
        x: start.x,
        y: start.y,
        radius,
        color: this.currentColor
      };
    }
  }
  
  // ==================== Object Creation ====================
  
  createRectangle(id, x, y, width, height, color) {
    const rect = new PIXI.Graphics();
    rect.beginFill(color);
    rect.drawRect(0, 0, width, height);
    rect.endFill();
    rect.x = x;
    rect.y = y;
    rect.interactive = true;
    rect.buttonMode = true;
    
    this.makeDraggable(rect, id);
    this.makeSelectable(rect, id);
    
    this.objects.set(id, rect);
    this.viewport.addChild(rect);
    
    return rect;
  }
  
  createCircle(id, x, y, radius, color) {
    const circle = new PIXI.Graphics();
    circle.beginFill(color);
    circle.drawCircle(0, 0, radius);
    circle.endFill();
    circle.x = x;
    circle.y = y;
    circle.interactive = true;
    circle.buttonMode = true;
    
    this.makeDraggable(circle, id);
    this.makeSelectable(circle, id);
    
    this.objects.set(id, circle);
    this.viewport.addChild(circle);
    
    return circle;
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
    
    this.makeDraggable(textObj, id);
    this.makeSelectable(textObj, id);
    
    this.objects.set(id, textObj);
    this.viewport.addChild(textObj);
    
    return textObj;
  }
  
  // ==================== Interaction ====================
  
  makeDraggable(obj, id) {
    let dragData = null;
    
    obj.on('pointerdown', (event) => {
      if (this.currentTool !== 'select') return;
      
      dragData = event.data;
      obj.alpha = 0.7;
      dragData.dragging = true;
      event.stopPropagation();
    });
    
    obj.on('pointerup', () => {
      if (dragData && dragData.dragging) {
        obj.alpha = 1;
        dragData.dragging = false;
        
        // Notify about position change
        if (this.onObjectMoved) {
          this.onObjectMoved(id, obj.x, obj.y);
        }
        
        dragData = null;
      }
    });
    
    obj.on('pointermove', () => {
      if (dragData && dragData.dragging) {
        const newPosition = dragData.getLocalPosition(obj.parent);
        obj.x = newPosition.x;
        obj.y = newPosition.y;
      }
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
    
    // Visual selection indicator
    if (obj instanceof PIXI.Graphics) {
      obj.lineStyle(2, 0x00FF00);
      obj.drawRect(-2, -2, obj.width + 4, obj.height + 4);
    }
  }
  
  deselectObject(id) {
    const obj = this.objects.get(id);
    if (!obj) return;
    
    this.selectedObjects.delete(id);
    
    // Remove selection indicator
    if (obj instanceof PIXI.Graphics) {
      obj.lineStyle(0);
    }
  }
  
  clearSelection() {
    this.selectedObjects.forEach(id => this.deselectObject(id));
    this.selectedObjects.clear();
  }
  
  deleteSelected() {
    this.selectedObjects.forEach(id => {
      this.deleteObject(id);
      if (this.onObjectDeleted) {
        this.onObjectDeleted(id);
      }
    });
    this.selectedObjects.clear();
  }
  
  // ==================== Object Management ====================
  
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
      obj.destroy();
    }
  }
  
  getObject(id) {
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
  
  // ==================== Remote Cursors ====================
  
  updateRemoteCursor(userId, username, x, y) {
    let cursor = this.remoteCursors.get(userId);
    
    if (!cursor) {
      // Create new cursor
      cursor = new PIXI.Container();
      
      // Cursor pointer (triangle)
      const pointer = new PIXI.Graphics();
      pointer.beginFill(0xFF6B6B);
      pointer.moveTo(0, 0);
      pointer.lineTo(12, 18);
      pointer.lineTo(6, 18);
      pointer.lineTo(0, 24);
      pointer.endFill();
      
      // Username label
      const label = new PIXI.Text(username, {
        fontSize: 12,
        fill: 0xFFFFFF,
        backgroundColor: 0xFF6B6B,
        padding: 4
      });
      label.x = 15;
      label.y = 0;
      
      cursor.addChild(pointer);
      cursor.addChild(label);
      cursor.zIndex = 1000;
      
      this.remoteCursors.set(userId, cursor);
      this.viewport.addChild(cursor);
    }
    
    cursor.x = x;
    cursor.y = y;
  }
  
  removeRemoteCursor(userId) {
    const cursor = this.remoteCursors.get(userId);
    if (cursor) {
      this.viewport.removeChild(cursor);
      cursor.destroy();
      this.remoteCursors.delete(userId);
    }
  }
  
  // ==================== Utilities ====================
  
  generateId() {
    return 'obj-' + Math.random().toString(36).substr(2, 9);
  }
  
  setColor(color) {
    this.currentColor = color;
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
  
  // Callbacks - set these from outside
  onObjectCreated = null;
  onObjectMoved = null;
  onObjectDeleted = null;
  onCursorMoved = null;
}
