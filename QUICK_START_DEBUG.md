# Physics Debug Mode - Quick Start

## 🚀 Start in 3 Commands

```bash
# Terminal 1: Backend
cd backend && ./start.sh

# Terminal 2: Frontend  
cd frontend && pnpm run dev

# Browser: Open http://localhost:5173
```

## ✅ What You Should See

1. **Console output:**
   ```
   [INIT] ==================== PHYSICS DEBUG MODE ====================
   [INIT] Authentication BYPASSED for debugging
   ```

2. **Canvas:**
   - Red X-axis and green Y-axis at origin
   - No auth modal (bypassed)

3. **Controls:**
   - Physics Controls panel on right

## 🎯 Test Ball Spawning

1. Click **"Spawn Ball"** button
2. Click on canvas
3. Watch console for:
   ```
   [CreateBall] Creating ball ball-123 at (400.0, 300.0)
   [CreateBall] ✓ Ball ball-123 created successfully
   ```
4. Ball should appear with ID number visible
5. Ball should fall and bounce

## 🐛 Debug Commands

Open console (F12), then:

```javascript
// List all balls
window.debugPhysics.listBalls()

// Spawn test ball at center
window.debugPhysics.spawnTest()

// Check viewport
window.debugPhysics.viewport()

// Clear all
window.debugPhysics.clearAll()
```

## ❌ If Ball Doesn't Appear

```javascript
// 1. Check ball was created
window.debugPhysics.listBalls()
// Should show Map with entries

// 2. Check viewport has children
window.debugPhysics.viewport()
// childrenCount should be > 1

// 3. Check WebSocket
window.debugPhysics.ws()
// Should show connected: true
```

## 📝 Console Log Tags

- `[INIT]` - Initialization
- `[CreateBall]` - Ball creation
- `[PhysicsDelta]` - Server updates
- `[WebSocket]` - Connection status
- `[Debug]` - Periodic info (every 5s)

## 🎮 Controls

- **Middle Mouse + Drag**: Pan
- **Mouse Wheel**: Zoom
- **Spawn Ball Button**: Enter spawn mode

## 📚 Full Documentation

See `frontend/PHYSICS_DEBUG_GUIDE.md` for complete testing guide.

## 🔄 Revert to Full Canvas

```bash
git checkout frontend/src/main.js
git clean -f frontend/src/simple-physics-canvas.js
```

## 🎯 Success = Ball appears, falls, and bounces with visible ID
