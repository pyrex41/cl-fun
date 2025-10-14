// src/canvas.js
// Complete PixiJS Canvas Manager for CollabCanvas
import * as PIXI from 'pixi.js';

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

    // Track cursor movement
    canvas.addEventListener('mousemove', (e) => {
      const worldPos = this.screenToWorld(e.clientX, e.clientY);

      // Notify about cursor movement
      if (this.onCursorMoved) {
        this.onCursorMoved(worldPos.x, worldPos.y);
      }

      // Handle preview shape drawing
      if (drawStart && previewShape) {
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

    // Store dimensions for selection box
    rect.userData = { width, height, type: 'rectangle' };

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

    // Store dimensions for selection box
    circle.userData = { radius, type: 'circle' };

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
    let dragOffset = { x: 0, y: 0 };

    obj.on('pointerdown', (event) => {
      if (this.currentTool !== 'select') return;

      dragData = event.data;
      obj.alpha = 0.7;
      dragData.dragging = true;

      // Store the offset between cursor and object position
      const cursorPos = dragData.getLocalPosition(obj.parent);
      dragOffset.x = cursorPos.x - obj.x;
      dragOffset.y = cursorPos.y - obj.y;

      event.stopPropagation();
    });

    obj.on('pointerup', () => {
      if (dragData && dragData.dragging) {
        obj.alpha = 1;
        dragData.dragging = false;

        // Notify about position change
        if (this.onObjectUpdated) {
          this.onObjectUpdated(id, { x: obj.x, y: obj.y });
        }

        dragData = null;
      }
    });

    obj.on('pointermove', () => {
      if (dragData && dragData.dragging) {
        const newPosition = dragData.getLocalPosition(obj.parent);
        // Apply the offset to maintain cursor position relative to object
        obj.x = newPosition.x - dragOffset.x;
        obj.y = newPosition.y - dragOffset.y;
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

    // Remove existing selection indicator if any
    const existingIndicator = this.selectionIndicators.get(id);
    if (existingIndicator) {
      this.viewport.removeChild(existingIndicator);
      existingIndicator.destroy();
    }

    // Create selection indicator
    const indicator = new PIXI.Graphics();
    indicator.lineStyle(2, 0x00FF00);

    if (obj.userData) {
      if (obj.userData.type === 'rectangle') {
        // Draw selection box around rectangle
        const { width, height } = obj.userData;
        indicator.drawRect(-2, -2, width + 4, height + 4);
        indicator.x = obj.x;
        indicator.y = obj.y;
      } else if (obj.userData.type === 'circle') {
        // Draw selection box around circle
        const { radius } = obj.userData;
        indicator.drawCircle(0, 0, radius + 2);
        indicator.x = obj.x;
        indicator.y = obj.y;
      }
    }

    // Add to viewport and store reference
    this.viewport.addChild(indicator);
    this.selectionIndicators.set(id, indicator);
  }

  deselectObject(id) {
    const obj = this.objects.get(id);
    if (!obj) return;

    this.selectedObjects.delete(id);

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
      obj.destroy();

      // Clean up selection indicator if it exists
      const indicator = this.selectionIndicators.get(id);
      if (indicator) {
        this.viewport.removeChild(indicator);
        indicator.destroy();
        this.selectionIndicators.delete(id);
      }

      // Remove from selected objects set
      this.selectedObjects.delete(id);
    }
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

  updateRemoteObject(objectId, updates) {
    console.log('Updating remote object:', objectId, updates);
    this.updateObject(objectId, updates);
  }

  deleteRemoteObject(objectId) {
    console.log('Deleting remote object:', objectId);
    this.deleteObject(objectId);
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
  
  updateRemoteCursor(userId, username, x, y, color) {
    let cursor = this.remoteCursors.get(userId);

    // Convert color string (like "#FF6B6B") to number
    const colorNum = color && typeof color === 'string' && color.startsWith('#')
      ? parseInt(color.substring(1), 16)
      : 0xFF6B6B;

    if (!cursor) {
      // Create new cursor
      cursor = new PIXI.Container();

      // Cursor pointer (triangle)
      const pointer = new PIXI.Graphics();
      pointer.beginFill(colorNum);
      pointer.moveTo(0, 0);
      pointer.lineTo(12, 18);
      pointer.lineTo(6, 18);
      pointer.lineTo(0, 24);
      pointer.endFill();

      // Username label
      const label = new PIXI.Text(username, {
        fontSize: 12,
        fill: 0xFFFFFF,
        backgroundColor: colorNum,
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
