# Latency Monitoring Guide

This document describes how to use and interpret the latency monitoring system for CollabCanvas WebSocket messaging.

## Overview

The latency monitoring system tracks round-trip latency for WebSocket messages, providing real-time performance insights. It includes:

1. **Message Tracking**: Tracks when messages are sent and when responses are received
2. **Automatic Warnings**: Logs warnings when latency exceeds 100ms
3. **Statistical Analysis**: Provides percentiles (P50, P95, P99) and average latency
4. **Per-Message-Type Stats**: Track latency for different message types separately

## Quick Access

### Browser Console Commands

```javascript
// Get overall latency statistics
getLatencyStats()

// Log detailed latency statistics
logLatencyStats()

// Get statistics for a specific message type
getLatencyStatsByType('object-create')
getLatencyStatsByType('object-update')
getLatencyStatsByType('object-delete')
```

## Understanding Latency Stats

### Overall Statistics

```javascript
getLatencyStats()
```

**Output:**
```javascript
{
  totalMessages: 150,           // Total tracked messages
  averageLatency: 45.23,        // Average round-trip time (ms)
  minLatency: 12.50,            // Fastest message (ms)
  maxLatency: 125.00,           // Slowest message (ms)
  p50: 42.00,                   // Median latency (ms)
  p95: 85.00,                   // 95th percentile (ms)
  p99: 115.00,                  // 99th percentile (ms)
  warningCount: 5,              // Messages >100ms
  historySize: 150,             // Samples in history
  pendingMessages: 2            // Messages awaiting response
}
```

### Interpreting Percentiles

- **P50 (Median)**: 50% of messages have latency below this value
- **P95**: 95% of messages have latency below this value (good indicator of typical worst-case)
- **P99**: 99% of messages have latency below this value (worst-case outliers)

### Target Metrics

✅ **Good Performance:**
- P50 (median) < 50ms
- P95 < 100ms
- P99 < 150ms
- Warning count < 5% of total messages

⚠️ **Needs Investigation:**
- P50 > 75ms
- P95 > 150ms
- P99 > 250ms
- Warning count > 10% of total messages

## Per-Message-Type Analysis

### Available Message Types

- `object-create`: Creating new objects
- `object-update`: Updating existing objects
- `object-delete`: Deleting objects

### Example Usage

```javascript
// Check object creation latency
const createStats = getLatencyStatsByType('object-create')
console.log('Object creation latency:', createStats)

// Output:
{
  messageType: 'object-create',
  count: 50,
  averageLatency: 38.45,
  minLatency: 15.20,
  maxLatency: 95.00,
  p50: 35.00,
  p95: 75.00,
  p99: 90.00
}
```

## Real-Time Monitoring

### Automatic High Latency Warnings

When a message takes >100ms, you'll see a warning in the console:

```
⚠️ High latency detected: 125ms for object-update message
```

### Continuous Monitoring

```javascript
// Monitor latency every 10 seconds
setInterval(() => {
  const stats = getLatencyStats()
  if (stats.p95 > 100) {
    console.warn(`High P95 latency: ${stats.p95}ms`)
  }
  if (stats.warningCount > stats.totalMessages * 0.1) {
    console.error(`Too many slow messages: ${stats.warningCount}/${stats.totalMessages}`)
  }
}, 10000)
```

## Detailed Examples

### Example 1: Track Object Creation Performance

```javascript
// Create 10 objects and measure latency
for (let i = 0; i < 10; i++) {
  window.collabCanvas.canvasManager.createRectangle({
    x: i * 100,
    y: 100,
    width: 80,
    height: 80
  })
}

// Wait a few seconds for responses, then check stats
setTimeout(() => {
  const createStats = getLatencyStatsByType('object-create')
  console.log('Object creation performance:', createStats)
}, 3000)
```

**Expected output:**
```javascript
Object creation performance: {
  messageType: 'object-create',
  count: 10,
  averageLatency: 42.30,
  minLatency: 28.50,
  maxLatency: 85.00,
  p50: 40.00,
  p95: 75.00,
  p99: 85.00
}
```

### Example 2: Compare Update vs. Create Latency

```javascript
function compareOperationLatency() {
  const createStats = getLatencyStatsByType('object-create')
  const updateStats = getLatencyStatsByType('object-update')

  console.log('=== Operation Latency Comparison ===')
  console.log('Create operations:')
  console.log(`  Count: ${createStats?.count || 0}`)
  console.log(`  Average: ${createStats?.averageLatency || 0}ms`)
  console.log(`  P95: ${createStats?.p95 || 0}ms`)

  console.log('\nUpdate operations:')
  console.log(`  Count: ${updateStats?.count || 0}`)
  console.log(`  Average: ${updateStats?.averageLatency || 0}ms`)
  console.log(`  P95: ${updateStats?.p95 || 0}ms`)

  if (createStats && updateStats) {
    const speedup = createStats.averageLatency / updateStats.averageLatency
    console.log(`\nUpdates are ${speedup.toFixed(2)}x faster than creates`)
  }
}

// Run the comparison
compareOperationLatency()
```

### Example 3: Stress Test with Latency Tracking

```javascript
async function stressTestWithLatency() {
  console.log('Starting stress test...')

  // Create 100 objects rapidly
  for (let i = 0; i < 100; i++) {
    window.collabCanvas.canvasManager.createRectangle({
      x: (i % 10) * 100,
      y: Math.floor(i / 10) * 100,
      width: 80,
      height: 80
    })
    await new Promise(resolve => setTimeout(resolve, 50)) // 50ms between creates
  }

  // Wait for all responses
  await new Promise(resolve => setTimeout(resolve, 5000))

  // Analyze results
  const stats = getLatencyStats()
  console.log('\n=== Stress Test Results ===')
  console.log(`Total operations: ${stats.totalMessages}`)
  console.log(`Average latency: ${stats.averageLatency}ms`)
  console.log(`P95 latency: ${stats.p95}ms`)
  console.log(`High latency warnings: ${stats.warningCount}`)

  if (stats.p95 < 100) {
    console.log('✅ Performance under load: GOOD')
  } else {
    console.warn('⚠️ Performance under load: NEEDS IMPROVEMENT')
  }
}

// Run the stress test
stressTestWithLatency()
```

## Performance Optimization Tips

### If You See High Latency (>100ms):

1. **Check Network Conditions:**
   ```javascript
   // Use browser DevTools → Network → WS tab
   // Look for slow message round-trips
   ```

2. **Check Server Load:**
   - High server CPU usage
   - Database bottlenecks
   - Too many concurrent connections

3. **Check Message Size:**
   ```javascript
   // Large object payloads increase latency
   console.log('Bandwidth stats:', window.collabCanvas.wsClient.getBandwidthStats())
   ```

4. **Check Client-Side Performance:**
   ```javascript
   // Run FPS test
   window.collabCanvas.runPerformanceTest()
   ```

### Optimization Strategies

**Backend (Common Lisp):**
- Use connection pooling (Task 10 ✅)
- Implement rate limiting (Task 9 ✅)
- Add database indexes (Task 10 ✅)
- Optimize canvas state saves with transactions (Task 10 ✅)

**Frontend (JavaScript):**
- Batch cursor updates (Task 1 ✅)
- Delta compression for objects (Task 2 ✅)
- Priority queue for messages (Task 3 ✅)
- Viewport culling to reduce load (Task 4 ✅)

## Integration with Other Monitoring

### Combined Performance Dashboard

```javascript
function showPerformanceDashboard() {
  console.log('=== CollabCanvas Performance Dashboard ===\n')

  // Latency Stats
  const latency = getLatencyStats()
  console.log('📊 Latency Metrics:')
  console.log(`  Total messages: ${latency.totalMessages}`)
  console.log(`  Average: ${latency.averageLatency}ms`)
  console.log(`  P50: ${latency.p50}ms | P95: ${latency.p95}ms | P99: ${latency.p99}ms`)
  console.log(`  Warnings: ${latency.warningCount}\n`)

  // FPS Stats
  const fps = getPerformanceStats()
  if (fps) {
    console.log('🎮 Rendering Performance:')
    console.log(`  Current FPS: ${fps.currentFPS}`)
    console.log(`  Average FPS: ${fps.averageFPS}`)
    console.log(`  Min FPS: ${fps.minFPS}\n`)
  }

  // Bandwidth Stats
  const bandwidth = window.collabCanvas.wsClient.getBandwidthStats()
  console.log('📡 Bandwidth Usage:')
  console.log(`  Total received: ${bandwidth.totalBytes} bytes`)
  console.log(`  Messages: ${bandwidth.messageCount}`)
  console.log(`  Avg per message: ${bandwidth.averageBytesPerMessage} bytes\n`)

  // Health Check
  const isHealthy = latency.p95 < 100 && (!fps || fps.averageFPS > 55)
  console.log(isHealthy ? '✅ System Health: GOOD' : '⚠️ System Health: NEEDS ATTENTION')
}

// Run the dashboard
showPerformanceDashboard()

// Or set up auto-refresh
setInterval(showPerformanceDashboard, 30000) // Every 30 seconds
```

## Advanced Usage

### Export Latency Data for Analysis

```javascript
function exportLatencyData() {
  const stats = getLatencyStats()
  const types = ['object-create', 'object-update', 'object-delete']

  const data = {
    timestamp: new Date().toISOString(),
    overall: stats,
    byType: {}
  }

  types.forEach(type => {
    const typeStats = getLatencyStatsByType(type)
    if (typeStats) {
      data.byType[type] = typeStats
    }
  })

  // Export to JSON
  const json = JSON.stringify(data, null, 2)
  console.log('Latency data export:')
  console.log(json)

  // Copy to clipboard
  navigator.clipboard.writeText(json)
  console.log('✅ Data copied to clipboard')
}

// Export current data
exportLatencyData()
```

### Reset Latency Stats

```javascript
// Reset stats (useful for testing specific scenarios)
window.collabCanvas.wsClient.latencyMonitor.reset()
console.log('Latency stats reset')
```

## Troubleshooting

### No Latency Data

**Symptom:** `getLatencyStats()` returns `totalMessages: 0`

**Solution:**
- Latency tracking only works for operations that expect responses
- Cursor updates are not tracked (too frequent)
- Ensure you're creating/updating/deleting objects to generate tracked messages

### Consistently High Latency

**Symptom:** P50 > 100ms, many warnings

**Checks:**
1. Network latency: `ping` your server
2. Server logs: Check for errors or slow queries
3. Database performance: Run `DATABASE-OPTIMIZATION-TESTING.md` tests
4. Client performance: Run `PERFORMANCE-TESTING.md` tests

### Latency Spikes

**Symptom:** P50 good, but P99 very high

**Possible Causes:**
- Garbage collection pauses
- Network packet loss
- Server under occasional heavy load
- Database lock contention

## Implementation Details

### Files Modified

- **frontend/src/websocket.js**:
  - Added `LatencyMonitor` class (lines 3-151)
  - Added message ID counter to `WebSocketClient`
  - Modified `send()` to track outgoing messages
  - Modified `handleMessage()` to track responses
  - Added `getLatencyStats()`, `getLatencyStatsByType()`, `logLatencyStats()` methods

- **frontend/src/main.js**:
  - Added global `getLatencyStats()` function
  - Added global `logLatencyStats()` function
  - Added global `getLatencyStatsByType()` function

### How It Works

1. **Message Sent**: When `send(data, trackLatency=true)` is called:
   - Generates unique message ID
   - Adds `messageId` to message payload
   - Records send timestamp in `pendingMessages` Map

2. **Response Received**: When `handleMessage(data)` receives a message with `messageId`:
   - Looks up send timestamp
   - Calculates latency: `performance.now() - sentTime`
   - Records in history array
   - Updates statistics (min, max, average)
   - Warns if >100ms

3. **Statistics Calculation**:
   - Percentiles: Sort latencies, calculate index
   - Average: Sum of all latencies / count
   - Per-type: Filter by message type, recalculate

### Performance Impact

- **Memory**: ~1KB per 1000 messages (with max history of 1000)
- **CPU**: Negligible (<0.1% overhead)
- **Network**: +8 bytes per tracked message (message ID field)

## References

- **Task 11**: Implement Performance Monitoring for Latency
- **Dependencies**: Tasks 1, 2, 3 (cursor batching, delta compression, priority queue)
- **Related**: `PERFORMANCE-TESTING.md`, `DATABASE-OPTIMIZATION-TESTING.md`
