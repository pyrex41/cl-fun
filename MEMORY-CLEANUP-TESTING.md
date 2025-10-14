# Memory Cleanup Testing Guide

This document describes how to test and verify that the memory cleanup implementation prevents memory leaks during connect/disconnect cycles.

## Overview

The memory cleanup system consists of:

1. **Backend Cleanup** (websocket.lisp):
   - Cancels flush timers and process timers when rooms become empty
   - Clears message queues (cursor batches, object batches)
   - Removes empty rooms from the global hash table

2. **Frontend Cleanup** (websocket.js, canvas.js, main.js):
   - Stops cursor throttle timer on disconnect
   - Resets bandwidth tracking stats
   - Clears all remote cursors and their PixiJS graphics
   - Periodic cleanup (every 60s) removes orphaned objects and inactive cursors

## Quick Test

### 1. Open Browser DevTools Memory Profiler

```javascript
// Chrome: DevTools → Memory tab
// Firefox: DevTools → Memory tab
// Safari: DevTools → Timelines → Memory
```

### 2. Take Initial Memory Snapshot

```javascript
// In browser console:
console.log('Initial memory:', Math.round(performance.memory.usedJSHeapSize / 1024 / 1024), 'MB')
```

### 3. Connect/Disconnect Test

Open the application in two browser windows:

**Window 1 (Observer):**
1. Log in as User A
2. Keep window open to monitor memory

**Window 2 (Connect/Disconnect):**
1. Log in as User B
2. Move cursor around (creates cursor updates)
3. Create 5-10 objects (rectangles/circles)
4. Log out
5. Repeat steps 1-4 five times

**Back to Window 1:**
```javascript
// Check memory after 5 cycles:
console.log('Memory after 5 cycles:', Math.round(performance.memory.usedJSHeapSize / 1024 / 1024), 'MB')

// Force garbage collection (Chrome only):
// DevTools → Memory → Click garbage can icon

// Check memory after GC:
console.log('Memory after GC:', Math.round(performance.memory.usedJSHeapSize / 1024 / 1024), 'MB')
```

**Expected Result:** Memory should return to near-initial levels after GC (±2-5 MB variance is normal).

## Detailed Testing

### Test 1: Cursor Throttle Timer Cleanup

**Purpose:** Verify cursor throttle timer is stopped on disconnect

**Steps:**
1. Open browser console
2. Log in to the application
3. Check that cursor throttle is running:
   ```javascript
   console.log('Cursor throttle active:', !!window.collabCanvas.wsClient.cursorThrottle.intervalId)
   // Should output: true
   ```
4. Log out or close tab
5. In a new session, check WebSocket client state:
   ```javascript
   console.log('Cursor throttle after disconnect:', window.collabCanvas.wsClient.cursorThrottle.intervalId)
   // Should output: null
   ```

**Expected Result:** Cursor throttle timer should be null after disconnect

### Test 2: Remote Cursor Cleanup

**Purpose:** Verify remote cursors are properly destroyed

**Steps:**
1. Open two browser windows (User A and User B)
2. In User A console:
   ```javascript
   // Check initial cursors
   console.log('Remote cursors:', window.collabCanvas.canvasManager.remoteCursors.size)
   // Should output: 0
   ```
3. User B moves cursor around
4. In User A console:
   ```javascript
   console.log('Remote cursors after User B joins:', window.collabCanvas.canvasManager.remoteCursors.size)
   // Should output: 1
   ```
5. User B disconnects (logs out or closes tab)
6. In User A console:
   ```javascript
   console.log('Remote cursors after User B leaves:', window.collabCanvas.canvasManager.remoteCursors.size)
   // Should output: 0
   ```

**Expected Result:** Remote cursor count should return to 0 after disconnect

### Test 3: Periodic Cleanup - Orphaned Objects

**Purpose:** Verify periodic cleanup removes orphaned selection indicators

**Steps:**
1. Log in to the application
2. Create 10 objects (rectangles/circles)
3. Select an object (creates selection indicator)
4. Manually corrupt the state to create orphan:
   ```javascript
   // Force-delete object without removing indicator
   const objectId = Array.from(window.collabCanvas.canvasManager.objects.keys())[0]
   window.collabCanvas.canvasManager.objects.delete(objectId)

   // Check orphaned indicator exists
   console.log('Selection indicators:', window.collabCanvas.canvasManager.selectionIndicators.size)
   // Should be > 0
   ```
5. Wait 60 seconds for periodic cleanup or force it:
   ```javascript
   window.collabCanvas.canvasManager.performCleanup()
   ```
6. Check that orphan is removed:
   ```javascript
   console.log('Selection indicators after cleanup:', window.collabCanvas.canvasManager.selectionIndicators.size)
   // Should output: 0
   ```

**Expected Result:** Orphaned indicators should be removed after cleanup

### Test 4: Periodic Cleanup - Inactive Cursors

**Purpose:** Verify periodic cleanup removes inactive cursors (>5 minutes old)

**Steps:**
1. Open two browser windows (User A and User B)
2. User B moves cursor (creates remote cursor in User A)
3. In User A console, simulate old cursor:
   ```javascript
   // Get User B's cursor
   const cursors = Array.from(window.collabCanvas.canvasManager.remoteCursors.values())
   const userBCursor = cursors[0]

   // Simulate 6-minute-old cursor
   userBCursor.lastUpdate.time = performance.now() - (6 * 60 * 1000)

   console.log('Remote cursors before cleanup:', window.collabCanvas.canvasManager.remoteCursors.size)
   // Should output: 1
   ```
4. Force periodic cleanup:
   ```javascript
   window.collabCanvas.canvasManager.performCleanup()
   ```
5. Check cursor was removed:
   ```javascript
   console.log('Remote cursors after cleanup:', window.collabCanvas.canvasManager.remoteCursors.size)
   // Should output: 0
   ```

**Expected Result:** Inactive cursor should be removed after cleanup

### Test 5: Backend Room Cleanup

**Purpose:** Verify backend cleans up empty rooms

**Check backend logs** after User B disconnects:

```
Client disconnected: <user-id>
Checking if room is empty after client disconnect
Room is empty, cleaning up resources
Stopped flush timer for room: default-canvas
Stopped process timer for room: default-canvas
Cleared cursor batch: 0 pending updates
Cleared object batch: 0 pending updates
Removed empty room: default-canvas
```

**Expected Result:** Backend should log room cleanup when last user disconnects

### Test 6: Long-Term Memory Test (Stress Test)

**Purpose:** Verify no memory leaks over extended connect/disconnect cycles

**Automated Script:**
```javascript
// Run this in User A console (observer window)
async function stressTestMemoryCleanup() {
    const initialMemory = performance.memory.usedJSHeapSize
    console.log('Initial memory:', Math.round(initialMemory / 1024 / 1024), 'MB')

    const cycles = 10
    const delayBetweenCycles = 5000 // 5 seconds

    for (let i = 0; i < cycles; i++) {
        console.log(`\n=== Cycle ${i + 1}/${cycles} ===`)

        // Instructions to open/close User B window manually
        console.log('Open User B window, create 10 objects, move cursor, then close')
        console.log('Waiting 5 seconds...')

        await new Promise(resolve => setTimeout(resolve, delayBetweenCycles))

        const currentMemory = performance.memory.usedJSHeapSize
        const delta = currentMemory - initialMemory
        console.log('Current memory:', Math.round(currentMemory / 1024 / 1024), 'MB')
        console.log('Delta from initial:', Math.round(delta / 1024 / 1024), 'MB')

        // Alert if memory growth exceeds 20 MB
        if (delta > 20 * 1024 * 1024) {
            console.warn('⚠️ Memory growth detected! Potential leak.')
        }
    }

    console.log('\n=== Test Complete ===')
    console.log('Force GC and check final memory (Chrome: DevTools → Memory → GC button)')
}

// Run the test
stressTestMemoryCleanup()
```

**Manual steps for each cycle:**
1. Open User B window
2. Log in as different user
3. Create 10 objects
4. Move cursor around for 10 seconds
5. Close User B window
6. Wait for script to proceed

**Expected Result:** Memory delta should remain <20 MB after 10 cycles

## Monitoring Tools

### Chrome DevTools Memory Profiler

1. **Heap Snapshot:**
   - Take snapshot before test
   - Run connect/disconnect cycles
   - Take snapshot after test
   - Compare snapshots to identify retained objects

2. **Allocation Timeline:**
   - Start recording
   - Run connect/disconnect cycles
   - Stop recording
   - Look for objects that are allocated but not freed

3. **Performance Monitor:**
   - Open: DevTools → More Tools → Performance Monitor
   - Monitor "JS heap size" during test
   - Look for sawtooth pattern (normal) vs. linear growth (leak)

### Console Memory Tracking

```javascript
// Monitor memory continuously
let memoryHistory = []
setInterval(() => {
    const mb = Math.round(performance.memory.usedJSHeapSize / 1024 / 1024)
    memoryHistory.push({ time: Date.now(), memory: mb })
    console.log(`Memory: ${mb} MB`)

    // Keep only last 100 samples
    if (memoryHistory.length > 100) {
        memoryHistory.shift()
    }
}, 5000) // Every 5 seconds

// View history
console.table(memoryHistory)
```

### Backend Memory Monitoring (Common Lisp)

```lisp
;; In REPL:
(sb-ext:gc :full t)  ; Force garbage collection
(room)               ; Show memory usage

;; Monitor room count
(hash-table-count collabcanvas.websocket::*canvas-rooms*)
;; Should be 0 when no users connected

;; Monitor client count in a room
(let ((room (gethash "default-canvas" collabcanvas.websocket::*canvas-rooms*)))
  (when room
    (length (collabcanvas.websocket::room-clients room))))
```

## Expected Cleanup Behavior

### On User Disconnect:

**Backend:**
- ✅ Client removed from room's client list
- ✅ Empty room cleanup triggered
- ✅ Flush timer cancelled
- ✅ Process timer cancelled
- ✅ Message queues cleared
- ✅ Room removed from hash table

**Frontend (Disconnecting User):**
- ✅ WebSocket closed gracefully
- ✅ Cursor throttle timer stopped
- ✅ Bandwidth stats reset
- ✅ Periodic cleanup timer stopped (on logout)

**Frontend (Other Users):**
- ✅ Remote cursor removed and destroyed
- ✅ User removed from presence list
- ✅ UI updated to show user left

### On Periodic Cleanup (Every 60s):

**Frontend:**
- ✅ Orphaned selection indicators removed
- ✅ Inactive cursors (>5 min old) removed
- ✅ Console logs cleanup statistics

## Troubleshooting

### Memory Not Released After Disconnect

**Symptom:** Memory usage increases after each connect/disconnect cycle

**Checks:**
1. Verify cursor throttle is stopped:
   ```javascript
   console.log('Throttle ID:', window.collabCanvas.wsClient.cursorThrottle.intervalId)
   // Should be null after disconnect
   ```

2. Verify periodic cleanup is running:
   ```javascript
   console.log('Cleanup interval:', window.collabCanvas.canvasManager.cleanupInterval)
   // Should be a valid interval ID
   ```

3. Check for lingering remote cursors:
   ```javascript
   console.log('Remote cursors:', window.collabCanvas.canvasManager.remoteCursors.size)
   // Should be 0 when alone
   ```

### Backend Rooms Not Cleaning Up

**Symptom:** Rooms remain in hash table after all users disconnect

**Checks:**
1. Verify client count is 0:
   ```lisp
   (let ((room (gethash "default-canvas" collabcanvas.websocket::*canvas-rooms*)))
     (when room
       (length (collabcanvas.websocket::room-clients room))))
   ```

2. Check backend logs for cleanup messages

3. Verify timers are being cancelled (check logs)

### Orphaned Objects Not Removed

**Symptom:** Periodic cleanup doesn't remove orphaned indicators

**Checks:**
1. Verify cleanup is running:
   ```javascript
   // Should see logs every 60 seconds
   console.log('Cleanup interval:', window.collabCanvas.canvasManager.cleanupInterval)
   ```

2. Force cleanup to test:
   ```javascript
   window.collabCanvas.canvasManager.performCleanup()
   ```

3. Check cleanup logs in console for statistics

## Performance Impact

### Expected Overhead:

- **Cursor Throttle Timer:** Negligible (<0.1% CPU)
- **Periodic Cleanup (60s):** <50ms every 60 seconds
- **Disconnect Cleanup:** <10ms per disconnect

### Verify No Performance Regression:

```javascript
// Run performance test before and after cleanup implementation
window.collabCanvas.runPerformanceTest()

// Compare results:
// - Static FPS: Should remain ≥55 FPS
// - Pan FPS: Should remain ≥55 FPS
// - Zoom FPS: Should remain ≥55 FPS
```

## Acceptance Criteria

✅ **Memory Stability:**
- Memory usage after 10 connect/disconnect cycles should be within 10 MB of initial (after GC)

✅ **Cursor Cleanup:**
- Remote cursors removed immediately on user disconnect
- Inactive cursors (>5 min) removed by periodic cleanup

✅ **Timer Cleanup:**
- All timers (cursor throttle, periodic cleanup) stopped on disconnect/logout

✅ **Backend Cleanup:**
- Empty rooms removed from hash table
- Room timers cancelled
- Message queues cleared

✅ **No Performance Regression:**
- Performance tests still achieve ≥55 FPS minimum

## Files

- **backend/src/websocket.lisp** - Backend disconnect and room cleanup (lines 150-200)
- **frontend/src/websocket.js** - Frontend WebSocket cleanup (lines 267-298)
- **frontend/src/canvas.js** - Remote cursor and periodic cleanup (lines 873-979)
- **frontend/src/main.js** - Periodic cleanup initialization and logout cleanup (lines 176-179, 406-414)
