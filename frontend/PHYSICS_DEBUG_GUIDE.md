# Physics Canvas Debug Mode - Testing Guide

## Overview

This is a simplified version of CollabCanvas focused **exclusively** on debugging physics ball rendering. All collaboration features have been temporarily removed.

## What's Changed

### New Files
- `src/simple-physics-canvas.js` - Minimal PixiJS canvas manager
  - Only handles physics balls (circles with visible IDs)
  - Pan/zoom with middle mouse + wheel
  - Server position interpolation (20Hz → 60 FPS)
  - Extensive debug logging

### Modified Files
- `src/main.js` - Simplified initialization
  - **Authentication bypassed** with test credentials
  - Uses `SimplePhysicsCanvas` instead of `PhysicsRenderer`
  - Only physics WebSocket handlers active
  - Global debug commands at `window.debugPhysics`

## Running the System

### 1. Start Backend
```bash
cd backend
./start.sh
# Or manually:
ros run
(ql:quickload :collabcanvas)
(collabcanvas:start)
```

### 2. Start Frontend
```bash
cd frontend
pnpm install  # If not already installed
pnpm run dev
```

### 3. Open Browser
Navigate to: http://localhost:5173

**Expected behavior:**
- Auth modal should NOT appear (bypassed)
- Canvas loads immediately with coordinate axes visible
- Red X-axis and green Y-axis at origin (0, 0)

## Testing Ball Lifecycle

### Step 1: Spawn a Ball

**Action:** Click the "Spawn Ball" button in the Physics Controls panel

**Expected Console Output:**
```
[PhysicsUI] Spawn mode activated
[WebSocket] → spawn-ball message sent
[WebSocket] ← physics-delta received, entities: 1
[PhysicsDelta] Processing 1 entities
[PhysicsDelta] Entity[0]: id=123, pos=(400.0, 300.0), radius=20
[PhysicsDelta] Creating new ball ball-123
[CreateBall] Creating ball ball-123 at (400.0, 300.0) radius=20
[CreateBall] ✓ Ball ball-123 created successfully, total balls: 1
```

**Expected Visual:**
- A blue circle appears on canvas
- Circle has a visible ID number inside (e.g., "123")
- Ball should fall due to gravity

### Step 2: Verify Position Updates

**Action:** Wait 1-2 seconds, observe ball movement

**Expected Console Output (periodic):**
```
[PhysicsDelta] Processing 1 entities
[Debug] Active balls: 1
  ball-123: current=(400.0, 450.2) server=(400.0, 451.0)
```

**Expected Visual:**
- Ball smoothly falls downward
- Position updates are smooth (interpolation working)
- Ball should bounce when hitting bottom

### Step 3: Spawn Multiple Balls

**Action:** Click "Spawn Ball" button 2-3 more times

**Expected Visual:**
- Each ball appears where you click
- All balls have visible ID numbers
- Balls collide with each other
- All balls respond to gravity

## Debug Commands

Open browser console (F12) and try these commands:

### List All Balls
```javascript
window.debugPhysics.listBalls()
// Shows: ball IDs, positions, radius
```

### Get Canvas Info
```javascript
window.debugPhysics.canvas()
// Returns: SimplePhysicsCanvas instance
```

### Viewport Info
```javascript
window.debugPhysics.viewport()
// Shows: pan offset, zoom level, child count
```

### Spawn Test Ball
```javascript
window.debugPhysics.spawnTest()
// Creates red test ball at canvas center
```

### Clear All Balls
```javascript
window.debugPhysics.clearAll()
// Removes all balls from canvas
```

### Get WebSocket Client
```javascript
window.debugPhysics.ws()
// Returns: WebSocketClient instance
```

## Controls

- **Middle Mouse Button + Drag**: Pan canvas
- **Mouse Wheel**: Zoom in/out
- **Spawn Ball Button**: Enter spawn mode, click canvas to spawn
- **Gravity Slider**: Adjust gravity (-20 to +20 m/s²)

## Expected Problems to Debug

If balls don't appear, check these in order:

### 1. Ball Creation
```javascript
window.debugPhysics.listBalls()
// Should show Map with ball entries
```

If empty, check console for:
- `[CreateBall]` messages
- WebSocket connection status
- `physics-delta` messages received

### 2. Graphics Added to Viewport
```javascript
window.debugPhysics.viewport()
// childrenCount should match ball count + 1 (axes)
```

If childrenCount is 1 (only axes), balls aren't being added to viewport.

### 3. Ball Position
```javascript
window.debugPhysics.listBalls()
// Check current/server positions
```

If positions are off-screen (negative or > canvas size), adjust spawn logic.

### 4. WebSocket Messages
```javascript
// In console, filter by "[WebSocket]"
// Should see:
//   ✓ Authentication successful
//   ← physics-delta received
```

If no physics-delta messages, backend physics loop may not be running.

## Debugging Checklist

- [ ] Backend server running on port 8080
- [ ] Frontend dev server running on port 5173
- [ ] Browser console open (F12)
- [ ] No auth modal appears (bypassed)
- [ ] Canvas loads with red/green axes visible
- [ ] WebSocket connects successfully
- [ ] "Spawn Ball" button visible in Physics Controls
- [ ] Click spawn button → console shows `[PhysicsUI] Spawn mode activated`
- [ ] Click canvas → console shows `[WebSocket] → spawn-ball`
- [ ] Console shows `[WebSocket] ← physics-delta` messages
- [ ] Console shows `[CreateBall]` messages
- [ ] Ball appears on canvas with visible ID
- [ ] Ball moves/falls (gravity active)
- [ ] Multiple balls can be spawned
- [ ] Balls collide with each other

## Console Log Legend

- `[INIT]` - Application initialization
- `[SimplePhysicsCanvas]` - Canvas manager operations
- `[WebSocket]` - WebSocket messages (← incoming, → outgoing)
- `[PhysicsDelta]` - Processing physics updates from server
- `[CreateBall]` - Ball creation operations
- `[UpdateBall]` - Ball position updates
- `[RemoveBall]` - Ball removal operations
- `[Pan]` / `[Zoom]` - Pan/zoom controls
- `[Debug]` - Periodic debug info
- `✓` - Success indicator
- `✗` - Error indicator

## Next Steps After Debugging

Once balls render and update correctly:

1. Re-enable authentication in `main.js`
2. Switch back to `PhysicsRenderer` (full canvas manager)
3. Re-add manual shape creation tools
4. Re-enable remote cursors and collaboration
5. Add back selection/dragging
6. Re-implement performance optimizations

## Common Issues

### Issue: Balls spawn but don't appear
**Check:** Ball position relative to viewport
**Fix:** Use `window.debugPhysics.viewport()` to check pan offset

### Issue: Balls appear but don't move
**Check:** Physics-delta messages arriving
**Fix:** Verify backend physics loop running at 20 Hz

### Issue: Balls lag or jitter
**Check:** Interpolation factor (default 0.3)
**Fix:** Adjust in `simple-physics-canvas.js`

### Issue: WebSocket not connecting
**Check:** Backend server on port 8080
**Fix:** Start backend with `./start.sh`

### Issue: "Spawn Ball" button does nothing
**Check:** PhysicsUI initialization
**Fix:** Look for `[WebSocket] ✓ Physics UI initialized` in console

## Files Modified for Debug Mode

1. `frontend/src/simple-physics-canvas.js` (NEW)
2. `frontend/src/main.js` (MODIFIED - auth bypass, simplified)

**To revert:** Git checkout these files or restore from backup.

## Support

If problems persist after following this guide:
1. Share full console output (all `[...]` prefixed messages)
2. Share `window.debugPhysics.listBalls()` output
3. Share `window.debugPhysics.viewport()` output
4. Share screenshot of canvas state
