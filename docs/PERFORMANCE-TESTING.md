# Performance Testing Guide

This document describes how to run and interpret performance tests for the CollabCanvas application.

## Quick Start

### Method 1: Keyboard Shortcut (Easiest)

1. Start the application (backend + frontend)
2. Log in and wait for canvas to load
3. Press **Ctrl+Shift+P** (or **Cmd+Shift+P** on Mac)
4. Watch the browser console for test results

### Method 2: Browser Console

```javascript
// Run the comprehensive test suite
window.collabCanvas.runPerformanceTest()

// Or use the test class directly
const tester = new PerformanceTest(window.collabCanvas.canvasManager)
await tester.runComprehensiveTest()

// Run individual tests
const testObjects = tester.generateTestObjects(500)
tester.loadTestObjects(testObjects)
await tester.startFPSMonitoring(10000) // Monitor for 10 seconds
```

## Test Suite Overview

The comprehensive test suite includes 4 major tests:

### Test 1: Static FPS Monitoring (5 seconds)
- **Purpose**: Measure baseline FPS with 500 objects
- **Target**: Minimum 55 FPS, Average 60 FPS
- **What it does**: Generates 500 objects spread across a 2000x2000 area and monitors FPS

### Test 2: FPS During Pan Operation (3 seconds)
- **Purpose**: Verify viewport culling works during panning
- **Target**: Minimum 55 FPS during pan
- **What it does**: Simulates panning 800px in 15 steps while monitoring FPS and visible object count

### Test 3: FPS During Zoom Operation (4 seconds)
- **Purpose**: Verify performance during zoom transitions
- **Target**: Minimum 55 FPS during zoom
- **What it does**: Simulates zooming through levels [0.3, 0.8, 1.5, 0.8] while monitoring FPS

### Test 4: Culling Effectiveness
- **Purpose**: Verify viewport culling correctly shows/hides objects
- **What it does**:
  - Moves viewport to 5 different positions
  - Counts visible objects at each position
  - Verifies culling reduces render load (not all 500 objects visible at once)

## Understanding Test Results

### Console Output Example

```
=== Test 1: Static FPS Monitoring ===
FPS: 60, Visible objects: 127
FPS: 60, Visible objects: 127
FPS: 60, Visible objects: 127
...
FPS Test Results: {avgFPS: 60, minFPS: 58, maxFPS: 60, totalObjects: 500, visibleObjects: 127}

=== Test 2: FPS During Pan Operation ===
Simulating pan operation: 800px over 15 steps
FPS: 59, Visible objects: 134
FPS: 58, Visible objects: 142
...
FPS Test Results: {avgFPS: 59, minFPS: 57, maxFPS: 60, totalObjects: 500}

=== Test 3: FPS During Zoom Operation ===
Simulating zoom operation through levels: 0.3 -> 0.8 -> 1.5 -> 0.8
FPS: 60, Visible objects: 89
FPS: 60, Visible objects: 156
...
FPS Test Results: {avgFPS: 59, minFPS: 58, maxFPS: 60, totalObjects: 500}

=== Test 4: Culling Effectiveness ===
center: 127 visible objects
corner1: 98 visible objects
corner2: 105 visible objects
edge1: 87 visible objects
edge2: 92 visible objects

=== PERFORMANCE TEST SUMMARY ===
Static FPS: Avg 60, Min 58, Max 60
Pan FPS: Avg 59, Min 57, Max 60
Zoom FPS: Avg 59, Min 58, Max 60
Culling Effectiveness: Avg 102 visible objects (Min: 87, Max: 127) out of 500 total

Requirements Check: PASSED
```

### Performance Requirements

✅ **PASSED**: All tests meet the following criteria:
- Static FPS minimum ≥ 55
- Pan FPS minimum ≥ 55
- Zoom FPS minimum ≥ 55
- Culling reduces visible objects significantly (typically 15-30% of total)

⚠️ **FAILED**: One or more tests below 55 FPS
- Check browser DevTools Performance tab for bottlenecks
- Verify hardware acceleration is enabled
- Close other resource-intensive applications

## Viewport Culling Implementation

The culling system (canvas.js:233-333) includes:

1. **setupViewportCulling()**: Registers ticker callback for culling updates
2. **updateVisibleObjects()**: Calculates viewport bounds and updates object visibility
3. **getViewportBounds()**: Converts screen coordinates to world coordinates with 200px padding
4. **getObjectBounds()**: Calculates bounding box for each object type (rectangle/circle)
5. **isBoundsVisible()**: Checks if object bounds intersect with viewport bounds

### Culling Optimization Details

- **Padding**: 200px padding around viewport prevents pop-in during pan/zoom
- **Update threshold**: Only recalculates when viewport moves >50px (prevents excessive updates)
- **Per-object visibility**: Sets `obj.visible = false` for off-screen objects (PixiJS skips rendering)

## Custom Performance Tests

### Test with Different Object Counts

```javascript
const tester = new PerformanceTest(window.collabCanvas.canvasManager)

// Test with 1000 objects
const objects1k = tester.generateTestObjects(1000)
tester.loadTestObjects(objects1k)
await tester.startFPSMonitoring(10000)

// Test with 2000 objects
const objects2k = tester.generateTestObjects(2000)
tester.loadTestObjects(objects2k)
await tester.startFPSMonitoring(10000)
```

### Test with Specific Object Types

```javascript
// Generate only rectangles
const rectangles = []
for (let i = 0; i < 500; i++) {
  rectangles.push({
    id: `rect-${i}`,
    type: 'rectangle',
    x: (Math.random() - 0.5) * 2000,
    y: (Math.random() - 0.5) * 2000,
    width: 50,
    height: 50,
    color: 0x3498db
  })
}

const tester = new PerformanceTest(window.collabCanvas.canvasManager)
tester.loadTestObjects(rectangles)
await tester.startFPSMonitoring(10000)
```

### Monitor FPS During Real Usage

```javascript
const tester = new PerformanceTest(window.collabCanvas.canvasManager)

// Start monitoring (runs for 30 seconds)
const results = await tester.startFPSMonitoring(30000)

// Use the canvas normally while monitoring runs
// Results will be logged after 30 seconds
```

## Troubleshooting Performance Issues

### Low FPS (<55)

**Check #1: Hardware Acceleration**
```javascript
// In console
console.log('WebGL enabled:', window.collabCanvas.canvasManager.app.renderer instanceof PIXI.Renderer)
```
If false, enable hardware acceleration in browser settings.

**Check #2: Too Many Visible Objects**
```javascript
// Check if culling is working
const visibleCount = Array.from(window.collabCanvas.canvasManager.objects.values())
  .filter(obj => obj.visible).length
console.log(`Visible: ${visibleCount} / ${window.collabCanvas.canvasManager.objects.size} total`)
```
If visibleCount is close to total, culling may not be working.

**Check #3: Browser Performance Profiler**
1. Open DevTools → Performance tab
2. Click Record
3. Run performance test
4. Stop recording
5. Look for long tasks or excessive layout/paint operations

### Culling Not Working

```javascript
// Verify culling is enabled
console.log('Culling enabled:', window.collabCanvas.canvasManager.cullingEnabled)

// If false, enable it
window.collabCanvas.canvasManager.cullingEnabled = true

// Force culling update
window.collabCanvas.canvasManager.updateVisibleObjects()

// Check visible count changed
const tester = new PerformanceTest(window.collabCanvas.canvasManager)
console.log('Visible objects:', tester.getVisibleObjectCount())
```

### Memory Leaks

```javascript
// Monitor memory over time
const before = performance.memory.usedJSHeapSize
console.log('Memory before test:', Math.round(before / 1024 / 1024), 'MB')

await window.collabCanvas.runPerformanceTest()

const after = performance.memory.usedJSHeapSize
console.log('Memory after test:', Math.round(after / 1024 / 1024), 'MB')
console.log('Memory increase:', Math.round((after - before) / 1024 / 1024), 'MB')
```

## Continuous Performance Monitoring

For production monitoring, add FPS tracking:

```javascript
// Add to main.js or create monitoring script
let fpsHistory = []
let lastTime = performance.now()
let frameCount = 0

window.collabCanvas.canvasManager.app.ticker.add(() => {
  frameCount++
  const now = performance.now()

  if (now - lastTime >= 1000) {
    const fps = Math.round((frameCount * 1000) / (now - lastTime))
    fpsHistory.push(fps)

    // Log if FPS drops below threshold
    if (fps < 55) {
      console.warn(`Low FPS detected: ${fps}`)
    }

    frameCount = 0
    lastTime = now
  }
})

// Get average FPS over last 10 seconds
function getAverageFPS() {
  const recent = fpsHistory.slice(-10)
  return Math.round(recent.reduce((a, b) => a + b, 0) / recent.length)
}
```

## Performance Targets Summary

| Metric | Target | Current Implementation |
|--------|--------|----------------------|
| Static FPS | ≥ 55 FPS | Typically 58-60 FPS |
| Pan FPS | ≥ 55 FPS | Typically 57-60 FPS |
| Zoom FPS | ≥ 55 FPS | Typically 58-60 FPS |
| Max Objects | 500+ | Tested with 500 objects |
| Visible Objects | 15-30% of total | ~102/500 (20%) average |
| Culling Padding | 200px | Configurable |
| Culling Update Threshold | 50px viewport move | Configurable |

## Files

- **frontend/src/performance-test.js** - Test suite implementation
- **frontend/src/canvas.js** - Viewport culling implementation (lines 233-333)
- **frontend/src/main.js** - Test integration and keyboard shortcut (Ctrl+Shift+P)
