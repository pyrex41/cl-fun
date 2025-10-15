# PRD: SQLite Message Queue for Distributed Messaging

**Feature ID:** SCALE-002
**Priority:** P1 (High - Enables Multi-Instance Deployment)
**Version:** 1.0
**Last Updated:** October 2025
**Status:** Planning

---

## Executive Summary

Replace in-memory WebSocket broadcasting with a persistent SQLite-based message queue to enable horizontal scaling across multiple server instances. This simulates a distributed message bus (like Redis Pub/Sub) without adding external dependencies, maintaining the simplicity of a single-database deployment.

### Key Benefits
- **Multi-instance deployment**: Run multiple server processes sharing state via database
- **Message persistence**: Messages survive server restarts
- **Deployment simplicity**: No Redis/RabbitMQ/Kafka required
- **Zero infrastructure cost**: One SQLite file handles everything
- **Distributed by design**: Foundation for cloud deployment with read replicas

### Trade-offs
- **Polling latency**: 50-100ms delay vs. instant in-memory broadcast
- **Database load**: Increased writes (INSERT per message) and reads (polling queries)
- **Not truly distributed**: SQLite limits to single-write-at-a-time
- **Eventual consistency**: Messages may arrive out of order across instances

---

## Problem Statement

### Current Architecture Limitations

**In-Memory Broadcasting:**
```lisp
(defun broadcast-to-room (room message)
  "Broadcast to all clients in THIS PROCESS ONLY"
  (bt:with-lock-held ((room-lock room))
    (loop for client being the hash-values of (room-clients room)
          do (send-text-message (client-websocket client) message))))
```

**Limitations:**
1. **Single-server bound**: Cannot scale horizontally
2. **No failover**: Server crash loses all active sessions
3. **Memory constraints**: One server = max ~1000 connections
4. **No load balancing**: Nginx can't distribute WebSocket connections

### Target Architecture

**Database-Backed Message Queue:**
```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  Server A   │     │  Server B   │     │  Server C   │
│  (Process)  │     │  (Process)  │     │  (Process)  │
│  Clients:   │     │  Clients:   │     │  Clients:   │
│  WS1, WS2   │     │  WS3, WS4   │     │  WS5, WS6   │
└──────┬──────┘     └──────┬──────┘     └──────┬──────┘
       │                   │                   │
       │    ┌──────────────┴──────────────┐    │
       └────┤   SQLite Message Queue      ├────┘
            │  (Shared Database File)     │
            └─────────────────────────────┘

Process:
1. Client on Server A creates object
2. Server A writes message to database
3. Servers B & C poll and find new message
4. Servers B & C broadcast to their local clients
```

---

## Technical Design

### Database Schema

#### Message Queue Table

```sql
CREATE TABLE IF NOT EXISTS message_queue (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    canvas_id TEXT NOT NULL,
    message_type TEXT NOT NULL,  -- 'cursor', 'object-create', 'object-update', etc.
    payload TEXT NOT NULL,        -- JSON message body
    created_at TEXT DEFAULT (datetime('now')),
    expires_at TEXT NOT NULL      -- Auto-cleanup timestamp
);

-- Critical indexes for performance
CREATE INDEX IF NOT EXISTS idx_message_queue_canvas_id_id
    ON message_queue(canvas_id, id);

CREATE INDEX IF NOT EXISTS idx_message_queue_expires_at
    ON message_queue(expires_at);

-- Partial index for high-priority messages (optimization)
CREATE INDEX IF NOT EXISTS idx_message_queue_priority
    ON message_queue(canvas_id, id)
    WHERE message_type IN ('cursor', 'presence', 'user-connected', 'user-disconnected');
```

#### Canvas Watermarks Table

```sql
-- Track last processed message per canvas per server instance
CREATE TABLE IF NOT EXISTS canvas_watermarks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    canvas_id TEXT NOT NULL,
    server_instance_id TEXT NOT NULL,  -- UUID for this server process
    last_processed_id INTEGER NOT NULL DEFAULT 0,
    updated_at TEXT DEFAULT (datetime('now')),
    UNIQUE(canvas_id, server_instance_id)
);

CREATE INDEX IF NOT EXISTS idx_canvas_watermarks_canvas_id
    ON canvas_watermarks(canvas_id);
```

### Core Components

#### 1. Message Queue Writer (`src/message-queue.lisp`)

**Purpose:** Write messages to database instead of broadcasting

```lisp
(defun enqueue-message (canvas-id message-type payload)
  "Write message to database queue"
  (let* ((expires-at (format-timestamp
                      (timestamp+ (now) 60 :sec))) ; 1 minute TTL
         (payload-json (to-json-string payload)))

    (with-db-transaction ()
      (execute-db
       "INSERT INTO message_queue (canvas_id, message_type, payload, expires_at)
        VALUES (?, ?, ?, ?)"
       canvas-id message-type payload-json expires-at))

    (log-debug "Enqueued message: ~A type=~A" canvas-id message-type)))

(defun broadcast-to-room (room message)
  "NEW: Write to queue instead of direct broadcast"
  (let ((canvas-id (room-id room))
        (msg-type (getf message :type)))

    ;; Write to database queue
    (enqueue-message canvas-id msg-type message)

    ;; Optional: Also broadcast locally for zero-latency in same process
    (when *enable-local-broadcast*
      (broadcast-to-local-clients room message))))
```

**Optimizations:**
- Batch INSERT for multiple messages (up to 10 at once)
- Use prepared statements to reduce parsing overhead
- WAL mode for concurrent reads during writes
- Async writes via `bt:make-thread` if latency critical

#### 2. Message Queue Polling Thread (`src/polling-thread.lisp`)

**Purpose:** Each canvas room spawns a background thread to poll for messages

```lisp
(defstruct poll-state
  "State for polling thread"
  canvas-id
  last-processed-id  ; Watermark
  poll-interval      ; Milliseconds (default 50ms)
  active-p           ; Control flag for shutdown
  thread)            ; Thread object

(defun start-polling-thread (room)
  "Start background thread to poll message queue for this canvas"
  (let* ((canvas-id (room-id room))
         (state (make-poll-state
                 :canvas-id canvas-id
                 :last-processed-id (get-watermark canvas-id)
                 :poll-interval *queue-poll-interval* ; 50ms default
                 :active-p t)))

    ;; Spawn thread
    (setf (poll-state-thread state)
          (bt:make-thread
           (lambda ()
             (poll-loop room state))
           :name (format nil "poll-~A" canvas-id)))

    ;; Store in room for cleanup
    (setf (room-poll-state room) state)

    (log-info "Started polling thread for canvas ~A" canvas-id)))

(defun poll-loop (room state)
  "Main polling loop - runs until canvas room closes"
  (loop while (poll-state-active-p state)
        do
    (handler-case
        (let* ((canvas-id (poll-state-canvas-id state))
               (last-id (poll-state-last-processed-id state))
               (new-messages (fetch-new-messages canvas-id last-id)))

          ;; Process new messages
          (when new-messages
            (dolist (msg new-messages)
              (let ((payload (parse-json (getf msg :payload))))
                ;; Broadcast to local WebSocket clients
                (broadcast-to-local-clients room payload)

                ;; Update watermark
                (setf (poll-state-last-processed-id state)
                      (getf msg :id)))))

          ;; Update watermark in database (every 10 polls)
          (when (zerop (mod (get-internal-real-time) 10))
            (update-watermark canvas-id (poll-state-last-processed-id state))))

      (error (e)
        (log-error "Poll error for canvas ~A: ~A" canvas-id e)))

    ;; Sleep before next poll
    (sleep (/ (poll-state-poll-interval state) 1000.0))))

(defun fetch-new-messages (canvas-id after-id)
  "Fetch messages from queue with ID > after-id"
  (query-db
   "SELECT id, message_type, payload
    FROM message_queue
    WHERE canvas_id = ? AND id > ?
    ORDER BY id ASC
    LIMIT 100"  ; Batch limit to prevent overload
   canvas-id after-id))
```

**Polling Optimization:**
- Adaptive polling: 50ms default, increase to 200ms when idle
- Batch processing: Handle up to 100 messages per poll
- Exponential backoff on errors
- Immediate wake on room activity (optional signal mechanism)

#### 3. Cleanup Thread (`src/queue-cleanup.lisp`)

**Purpose:** Remove old messages to prevent database bloat

```lisp
(defparameter *cleanup-thread* nil
  "Global cleanup thread for expired messages")

(defun start-cleanup-thread ()
  "Start global cleanup thread (one per server instance)"
  (setf *cleanup-thread*
        (bt:make-thread
         (lambda ()
           (cleanup-loop))
         :name "message-queue-cleanup")))

(defun cleanup-loop ()
  "Delete expired messages every 30 seconds"
  (loop
    (handler-case
        (let ((deleted-count (cleanup-expired-messages)))
          (when (> deleted-count 0)
            (log-info "Cleaned up ~A expired messages" deleted-count)))
      (error (e)
        (log-error "Cleanup error: ~A" e)))

    ;; Run every 30 seconds
    (sleep 30)))

(defun cleanup-expired-messages ()
  "Delete messages older than their expiry time"
  (with-db-transaction ()
    (let ((result (execute-db
                   "DELETE FROM message_queue
                    WHERE datetime(expires_at) < datetime('now')")))
      (sqlite:sqlite-changes *db*))))

(defun stop-cleanup-thread ()
  "Stop the cleanup thread gracefully"
  (when *cleanup-thread*
    (bt:destroy-thread *cleanup-thread*)
    (setf *cleanup-thread* nil)))
```

**Cleanup Strategy:**
- Messages expire after 60 seconds by default
- Configurable via `*message-ttl*` parameter
- Alternative: Delete messages older than highest watermark across all instances
- Run VACUUM periodically (weekly) to reclaim disk space

#### 4. Watermark Management

**Purpose:** Track which messages each server instance has processed

```lisp
(defparameter *server-instance-id* (generate-uuid)
  "Unique ID for this server process")

(defun get-watermark (canvas-id)
  "Get last processed message ID for this canvas/server"
  (or
   (query-db-value
    "SELECT last_processed_id FROM canvas_watermarks
     WHERE canvas_id = ? AND server_instance_id = ?"
    canvas-id *server-instance-id*)
   0)) ; Start from 0 if new

(defun update-watermark (canvas-id message-id)
  "Update watermark for this canvas/server"
  (with-db-transaction ()
    (execute-db
     "INSERT INTO canvas_watermarks (canvas_id, server_instance_id, last_processed_id)
      VALUES (?, ?, ?)
      ON CONFLICT(canvas_id, server_instance_id)
      DO UPDATE SET last_processed_id = ?, updated_at = datetime('now')"
     canvas-id *server-instance-id* message-id message-id)))

(defun get-lowest-watermark (canvas-id)
  "Get lowest watermark across all active servers"
  (query-db-value
   "SELECT MIN(last_processed_id) FROM canvas_watermarks
    WHERE canvas_id = ?
    AND datetime(updated_at) > datetime('now', '-5 minutes')"
   canvas-id))
```

---

## Message Flow Examples

### Example 1: Object Creation

```
┌──────────────────────────────────────────────────────────┐
│ User drags to create rectangle on Server A              │
└──────────────┬───────────────────────────────────────────┘
               │
               ▼
┌──────────────────────────────────────────────────────────┐
│ Server A: handle-object-create                           │
│  1. Validate object data                                 │
│  2. enqueue-message(canvas-id, "object-create", object)  │
│  3. Return success to client                             │
└──────────────┬───────────────────────────────────────────┘
               │
               ▼
┌──────────────────────────────────────────────────────────┐
│ Database: INSERT INTO message_queue                      │
│  - id: 12345                                             │
│  - canvas_id: "canvas-abc"                               │
│  - message_type: "object-create"                         │
│  - payload: {"id": "obj-1", "type": "rect", ...}         │
│  - expires_at: "2025-10-15 10:01:00"                     │
└──────────────┬───────────────────────────────────────────┘
               │
       ┌───────┴────────┬─────────────────────┬─────────────┐
       ▼                ▼                     ▼             ▼
   Server A        Server B              Server C      (Future servers)
   Poll: 50ms      Poll: 50ms            Poll: 50ms
   │               │                     │
   ▼               ▼                     ▼
   Fetch messages  Fetch messages        Fetch messages
   WHERE id > 12340 WHERE id > 12330     WHERE id > 12320
   │               │                     │
   ▼               ▼                     ▼
   [12345]         [12335, 12345]        [12321, 12335, 12345]
   │               │                     │
   ▼               ▼                     ▼
   Broadcast to    Broadcast to          Broadcast to
   local clients   local clients         local clients
   (WS1, WS2)      (WS3, WS4)           (WS5, WS6)
```

**Latency:**
- Server A (same process): 0ms (optional local broadcast)
- Server B/C: 0-100ms (depends on poll timing)
- Trade-off: Consistency over immediate delivery

### Example 2: Cursor Updates (High Frequency)

**Problem:** Cursors update 30 times per second → 30 DB writes per user

**Solution: Client-side throttling + Server-side batching**

```lisp
(defparameter *cursor-batch* nil
  "Batch of cursor updates to write together")

(defparameter *cursor-batch-lock*
  (bt:make-lock "cursor-batch"))

(defun enqueue-cursor-update (canvas-id user-id username color x y)
  "Add cursor update to batch (non-blocking)"
  (bt:with-lock-held (*cursor-batch-lock*)
    (push `(:canvas-id ,canvas-id
            :user-id ,user-id
            :username ,username
            :color ,color
            :x ,x
            :y ,y
            :timestamp ,(get-universal-time))
          *cursor-batch*)))

(defun flush-cursor-batch ()
  "Write all batched cursors at once (called every 200ms)"
  (let ((batch nil))
    (bt:with-lock-held (*cursor-batch-lock*)
      (setf batch *cursor-batch*)
      (setf *cursor-batch* nil))

    (when batch
      ;; Group by canvas
      (let ((by-canvas (group-by-canvas batch)))
        (loop for (canvas-id . cursors) in by-canvas
              do
          ;; Single INSERT with JSON array
          (enqueue-message canvas-id "cursor-batch"
                          `(:cursors ,cursors)))))))
```

**Optimization Results:**
- Before: 30 writes/sec/user × 100 users = 3000 writes/sec
- After: 5 batch writes/sec × 100 canvases = 500 writes/sec
- Reduction: 6x fewer database writes

---

## Performance Analysis

### Database Write Load

**Assumptions:**
- 100 concurrent users across 10 canvases (10 users per canvas)
- Each user generates:
  - 30 cursor updates/sec (throttled to 5 batched writes/sec)
  - 1 object update every 5 seconds

**Writes per second:**
- Cursor batches: 10 canvases × 5/sec = 50 writes/sec
- Object updates: 100 users × 0.2/sec = 20 writes/sec
- **Total: 70 writes/sec**

**Database capacity:**
- SQLite WAL mode: 5,000-10,000 writes/sec on SSD
- Headroom: 70x before saturation
- **Verdict: Plenty of capacity**

### Database Read Load

**Polling queries:**
- 10 active canvases × 20 polls/sec = 200 reads/sec
- Each query: `SELECT ... WHERE canvas_id = ? AND id > ?`
- Index hit rate: >99% (canvas_id + id index)
- Query time: <1ms per query

**Database capacity:**
- SQLite: 100,000+ SELECT queries/sec on indexed columns
- **Verdict: No concern**

### Latency Analysis

**End-to-end message latency:**
1. Client sends WebSocket message: 0ms (start)
2. Server receives and validates: 1ms
3. `enqueue-message` writes to DB: 5ms (WAL mode)
4. Other servers poll (worst case): 50ms
5. Broadcast to local clients: 1ms
6. **Total: 57ms (worst case)**

**Best case (same server):**
- Optional local broadcast: 1ms

**Comparison to in-memory:**
- In-memory: 1-2ms
- Database queue: 57ms
- **Trade-off: 28x slower, but enables horizontal scaling**

---

## Configuration

### Tunable Parameters

```lisp
(defparameter *queue-poll-interval* 50
  "Milliseconds between polls (50ms = 20 polls/sec)")

(defparameter *message-ttl* 60
  "Seconds before messages expire (60s default)")

(defparameter *cursor-batch-interval* 200
  "Milliseconds between cursor batch flushes (200ms = 5/sec)")

(defparameter *enable-local-broadcast* t
  "If T, broadcast to local clients immediately (0ms latency)")

(defparameter *max-messages-per-poll* 100
  "Maximum messages to fetch per poll")

(defparameter *cleanup-interval* 30
  "Seconds between cleanup runs (30s default)")
```

### Performance Tuning

**For low latency (trading database load):**
```lisp
(setf *queue-poll-interval* 20)      ; 50 polls/sec
(setf *enable-local-broadcast* t)    ; Instant for same-server
(setf *cursor-batch-interval* 100)   ; 10 batches/sec
```

**For high throughput (trading latency):**
```lisp
(setf *queue-poll-interval* 200)     ; 5 polls/sec
(setf *enable-local-broadcast* nil)  ; Force queue for all
(setf *cursor-batch-interval* 500)   ; 2 batches/sec
```

---

## Testing Strategy

### Unit Tests

**Test: Message Enqueue/Dequeue**
```lisp
(deftest test-message-queue
  (let ((canvas-id "test-canvas"))
    ;; Enqueue message
    (enqueue-message canvas-id "test" '(:data "hello"))

    ;; Fetch message
    (let ((messages (fetch-new-messages canvas-id 0)))
      (ok (= 1 (length messages)))
      (ok (string= "hello"
                   (getf (parse-json (getf (first messages) :payload))
                         :data))))))
```

**Test: Watermark Management**
```lisp
(deftest test-watermarks
  (let ((canvas-id "test-canvas"))
    ;; Initial watermark is 0
    (ok (= 0 (get-watermark canvas-id)))

    ;; Update watermark
    (update-watermark canvas-id 100)
    (ok (= 100 (get-watermark canvas-id)))

    ;; Survives server restart (in database)
    (let ((*server-instance-id* *server-instance-id*))
      (ok (= 100 (get-watermark canvas-id))))))
```

### Integration Tests

**Test: Multi-Server Message Propagation**
```lisp
(deftest test-multi-server-broadcast
  ;; Start two "servers" (threads simulating different instances)
  (let ((server-a-id (generate-uuid))
        (server-b-id (generate-uuid))
        (canvas-id "test-canvas")
        (received-a nil)
        (received-b nil))

    ;; Server A: Write message
    (let ((*server-instance-id* server-a-id))
      (enqueue-message canvas-id "test" '(:msg "hello")))

    ;; Server B: Poll and receive
    (let ((*server-instance-id* server-b-id))
      (sleep 0.1) ; Wait for poll
      (let ((messages (fetch-new-messages canvas-id 0)))
        (setf received-b messages)))

    ;; Verify Server B received the message
    (ok (= 1 (length received-b)))
    (ok (string= "hello"
                 (getf (parse-json (getf (first received-b) :payload))
                       :msg)))))
```

**Test: Cleanup Thread**
```lisp
(deftest test-message-cleanup
  ;; Create expired message
  (with-db-transaction ()
    (execute-db
     "INSERT INTO message_queue (canvas_id, message_type, payload, expires_at)
      VALUES (?, ?, ?, datetime('now', '-2 minutes'))"
     "test-canvas" "test" "{}"))

  ;; Run cleanup
  (let ((deleted (cleanup-expired-messages)))
    (ok (>= deleted 1) "At least 1 message deleted")))
```

### Load Tests

**Test: High Message Throughput**
```bash
# Run 10 concurrent threads, each writing 100 messages/sec
./test-scripts/load-test-queue.lisp \
  --threads 10 \
  --messages-per-sec 100 \
  --duration 60

# Expected: 60,000 messages written in 60 seconds
# Target: <100ms P95 write latency
```

**Test: Polling Performance**
```bash
# Simulate 50 canvases polling simultaneously
./test-scripts/load-test-polling.lisp \
  --canvases 50 \
  --poll-interval 50 \
  --duration 60

# Expected: 60,000 poll queries (50 canvases × 20/sec × 60s)
# Target: <5ms P95 query latency
```

---

## Migration Plan

### Phase 1: Database Schema (Hour 0-1)

1. Add message queue tables to `schema.sql`
2. Run migration on development database:
   ```bash
   sqlite3 backend/data/canvas.db < backend/db/migrations/001_message_queue.sql
   ```
3. Verify tables exist and indexes are created

### Phase 2: Message Queue Implementation (Hours 1-4)

1. Create `src/message-queue.lisp` with `enqueue-message`
2. Create `src/polling-thread.lisp` with polling logic
3. Create `src/queue-cleanup.lisp` with cleanup thread
4. Update `src/websocket.lisp` to use `enqueue-message`

### Phase 3: Testing (Hours 4-6)

1. Unit tests for all new functions
2. Integration test with two server instances
3. Load test with high message volume
4. Verify cleanup thread works correctly

### Phase 4: Deployment (Hours 6-8)

1. Deploy to staging with 2 instances behind Nginx
2. Monitor metrics (message queue size, latency, errors)
3. Tune polling interval based on load
4. Deploy to production

---

## Monitoring & Metrics

### Key Metrics

```lisp
(defun get-queue-metrics ()
  "Get message queue health metrics"
  `(:queue-size ,(queue-size)
    :oldest-message-age-seconds ,(oldest-message-age)
    :messages-per-second ,(messages-per-second)
    :average-poll-latency-ms ,(average-poll-latency)
    :active-polling-threads ,(active-polling-threads)))

(defun queue-size ()
  "Total messages in queue"
  (query-db-value
   "SELECT COUNT(*) FROM message_queue"))

(defun oldest-message-age ()
  "Age of oldest message in seconds"
  (query-db-value
   "SELECT CAST((julianday('now') - julianday(created_at)) * 86400 AS INTEGER)
    FROM message_queue
    ORDER BY id ASC
    LIMIT 1"))
```

### Health Checks

**Message Queue Health:**
- ✅ Healthy: <1000 messages, oldest <10s
- ⚠️ Warning: 1000-5000 messages, oldest 10-30s
- 🔴 Critical: >5000 messages, oldest >30s

**Action on Critical:**
1. Check if cleanup thread is running
2. Check if polling threads are stuck
3. Increase `*queue-poll-interval*` to reduce load
4. Consider purging old messages manually

---

## Rollback Plan

### Quick Rollback (< 5 minutes)

If issues arise in production:

1. Feature flag to disable queue:
   ```lisp
   (setf *use-message-queue* nil)  ; Revert to in-memory
   ```

2. Restart server with old code:
   ```bash
   git checkout main~1  # Previous commit
   ./restart.sh
   ```

3. Database rollback:
   ```sql
   -- Drop message queue tables (data preserved)
   DROP TABLE message_queue;
   DROP TABLE canvas_watermarks;
   ```

### Full Rollback (< 30 minutes)

1. Revert code to previous version
2. Redeploy all server instances
3. Drop message queue tables
4. Monitor for 1 hour to ensure stability

---

## Success Criteria

### Must Have (MVP)
- ✅ Messages written to database successfully
- ✅ Polling threads fetch and broadcast messages
- ✅ Cleanup thread removes expired messages
- ✅ Two server instances can communicate via queue
- ✅ Message latency <100ms P95

### Nice to Have (Post-MVP)
- Adaptive polling (faster when active, slower when idle)
- Binary message format (reduce payload size)
- Message compression (gzip for large payloads)
- Read replicas for scaling reads

### Performance Targets
- Write throughput: 1000+ messages/sec
- Read throughput: 10,000+ queries/sec
- P95 latency: <100ms end-to-end
- Memory overhead: <10MB for queue state

---

## Future Enhancements

### Redis Migration Path

When SQLite becomes a bottleneck:

```lisp
(defun enqueue-message (canvas-id message-type payload)
  "Write to Redis Pub/Sub instead of SQLite"
  (redis:publish (format nil "canvas:~A" canvas-id)
                 (to-json-string payload)))

(defun start-redis-subscriber (room)
  "Subscribe to Redis channel instead of polling SQLite"
  (redis:subscribe (format nil "canvas:~A" (room-id room))
                   (lambda (message)
                     (broadcast-to-local-clients room (parse-json message)))))
```

Benefits:
- Sub-millisecond latency
- True Pub/Sub (no polling)
- Horizontal scaling without coordination

---

**Document Version:** 1.0
**Last Reviewed:** October 2025
**Next Review:** November 2025
**Owner:** Backend Team
