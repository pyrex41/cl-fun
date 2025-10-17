# Physics-Canvas State Synchronization Implementation

## Overview

This document describes the implementation of bidirectional synchronization between the canvas state and physics engine for the CollabCanvas physics branch.

## Task Completed: Task 14 - Implement Physics-Canvas State Synchronization

### Implementation Summary

The physics engine and canvas state are now fully synchronized bidirectionally:

1. **Canvas → Physics**: All canvas object operations sync to physics engine
2. **Physics → Canvas**: Physics simulation broadcasts position updates via WebSocket
3. **Startup Sync**: Existing canvas objects are loaded into physics on initialization
4. **User Interaction**: Manual drag operations override physics positions

### Modified Files

#### 1. `/backend/src/websocket-adapter.lisp`

**Changes in `handle-object-create-message`:**
- Added call to `sync-canvas-object-to-physics` after saving object to canvas state
- Ensures all newly created objects are added to physics simulation
- Error handling to prevent physics sync failures from blocking object creation

**Changes in `handle-object-update-message`:**
- Added position update detection (checks for `:X` or `:Y` in updates)
- Calls `update-physics-object-position` when position changes (user drag)
- Manually overrides physics velocity and position to allow user control
- Error handling for physics sync failures

**Changes in `handle-object-delete-message`:**
- Added call to `remove-physics-object` when object is deleted
- Ensures physics engine stays in sync with canvas state
- Error handling to prevent deletion failures

**Changes in `handle-auth-message`:**
- Added canvas object loading into physics when user authenticates
- Syncs all canvas objects to physics for new connections
- Ensures physics state matches canvas state for each user session
- Error handling for sync failures during authentication

#### 2. `/backend/src/physics-loop.lisp`

**New Functions Added:**

```lisp
(defun load-canvas-objects-into-physics (canvas-id)
  "Load all objects from a canvas into the physics engine."
  ...)

(defun load-all-canvas-objects-into-physics ()
  "Load objects from all active canvases into the physics engine."
  ...)
```

**Changes in `initialize-physics-system`:**
- Added call to `load-all-canvas-objects-into-physics`
- Loads existing canvas objects during physics system startup
- Ensures physics engine has all objects from the start

#### 3. `/backend/collabcanvas.asd`

**Dependency Updates:**
- Changed `physics-loop` dependencies from `"websocket-adapter"` to `"canvas-state"`
- Added `"physics"` to `websocket-adapter` dependencies
- Ensures proper compilation order and function availability

### Synchronization Flow

#### Object Creation Flow

```
User creates object in UI
    ↓
Frontend sends object-create message
    ↓
handle-object-create-message receives message
    ↓
1. Save to canvas state (update-canvas-object)
    ↓
2. Sync to physics engine (sync-canvas-object-to-physics)
    ↓
3. Broadcast to other users
```

#### Object Update Flow (User Drag)

```
User drags object in UI
    ↓
Frontend sends object-update message with new x/y
    ↓
handle-object-update-message receives message
    ↓
1. Merge updates with current object
    ↓
2. Save to canvas state (update-canvas-object)
    ↓
3. If x/y changed: update-physics-object-position (override physics)
    ↓
4. Broadcast delta to other users
```

#### Object Deletion Flow

```
User deletes object in UI
    ↓
Frontend sends object-delete message
    ↓
handle-object-delete-message receives message
    ↓
1. Delete from canvas state (delete-canvas-object)
    ↓
2. Remove from physics engine (remove-physics-object)
    ↓
3. Broadcast deletion to other users
```

#### Startup/Authentication Flow

```
Server starts OR User authenticates
    ↓
initialize-physics-system OR handle-auth-message
    ↓
load-all-canvas-objects-into-physics
    ↓
For each canvas:
    ↓
    load-canvas-objects-into-physics(canvas-id)
        ↓
        get-canvas-objects(canvas-id)
        ↓
        For each object:
            ↓
            sync-canvas-object-to-physics(object)
```

#### Physics Simulation Loop (Continuous)

```
Every 20ms (50Hz):
    ↓
physics-step() executes
    ↓
Every 3 ticks (~16.67Hz):
    ↓
generate-physics-snapshot()
    ↓
broadcast-physics-snapshot() to all clients
    ↓
Frontend receives physics-snapshot message
    ↓
Update object positions in UI (if not being dragged)
```

### Key Functions Used from physics.lisp

1. **`sync-canvas-object-to-physics(obj-data)`**
   - Creates physics object from canvas object data
   - Handles circles (dynamic) and rectangles (static)
   - Uses `is-dynamic` property to determine physics behavior
   - Called on: object creation, canvas load, user authentication

2. **`update-physics-object-position(id, x, y)`**
   - Manually sets object position and zeroes velocity
   - Used when user drags objects to override physics
   - Prevents physics simulation from interfering with user interaction
   - Called on: object-update messages with position changes

3. **`remove-physics-object(id)`**
   - Removes object from physics simulation
   - Called on: object deletion

4. **`get-physics-state-snapshot()`**
   - Returns current positions of all dynamic objects
   - Called by: physics-loop every 3 ticks for broadcasting
   - Used to sync physics positions back to all clients

### Physics Properties

Objects in canvas state are enhanced with physics properties via `ensure-physics-properties` in `canvas-state.lisp`:

- **Circles**: Default to `isDynamic: true` (affected by physics)
- **Rectangles**: Default to `isDynamic: false` (static walls/platforms)
- **All objects**: Default friction (0.02) and restitution (0.7)

These defaults can be overridden by frontend when creating objects.

### Error Handling

All physics sync operations use `handler-case` to catch errors:
- Physics sync failures do not block canvas operations
- Errors are logged with `[WS WARN]` prefix
- Canvas state remains authoritative (physics is secondary)

### Testing Checklist

To verify the implementation works correctly:

1. **Object Creation Sync**
   - [ ] Create circle in UI → verify it appears in physics (falls with gravity)
   - [ ] Create rectangle in UI → verify it's static in physics
   - [ ] Check server logs for sync success messages

2. **Object Update Sync**
   - [ ] Drag a circle → verify physics velocity is zeroed
   - [ ] Release circle → verify it resumes physics simulation
   - [ ] Check server logs for position sync messages

3. **Object Deletion Sync**
   - [ ] Delete an object → verify it disappears from physics
   - [ ] Check server logs for removal messages

4. **Startup Sync**
   - [ ] Create objects, restart server
   - [ ] Verify objects are loaded into physics on startup
   - [ ] Check server logs for load count messages

5. **Authentication Sync**
   - [ ] Create objects in one tab
   - [ ] Open new tab and authenticate
   - [ ] Verify new tab loads objects into physics
   - [ ] Check server logs for auth sync messages

### Performance Considerations

- Physics sync operations are non-blocking (use `handler-case`)
- Physics loop runs at 50Hz independently of sync operations
- Snapshot broadcasts are throttled to ~16.67Hz (every 3 ticks)
- Canvas state has debounced saves (configurable via `*state-save-debounce*`)

### Future Enhancements

Potential improvements for future tasks:

1. **Conflict Resolution**: Handle simultaneous physics updates and user drags
2. **Selective Sync**: Only sync objects visible in user's viewport
3. **Physics Replication**: Deterministic physics on client side
4. **Undo/Redo**: Track physics state changes in canvas history
5. **Performance Optimization**: Spatial partitioning for large numbers of objects

## Conclusion

Task 14 is complete. The physics engine and canvas state are now fully synchronized bidirectionally with proper error handling and startup initialization.

All integration points specified in the task requirements have been implemented:
- ✅ Load existing objects into physics on startup
- ✅ Sync user-created objects to physics
- ✅ Handle user drag by overriding physics positions
- ✅ Remove objects from physics on deletion
- ✅ Bidirectional sync: canvas ↔ physics

The implementation is ready for testing and integration with the frontend physics rendering.
