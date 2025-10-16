# PRD: Migration to Clack/Woo Async Architecture

**Feature ID:** ARCH-001
**Priority:** P0 (Critical - Foundation for Scalability)
**Version:** 1.0
**Last Updated:** October 2025
**Status:** Planning

---

## Executive Summary

Migrate CollabCanvas backend from Hunchentoot (thread-per-connection model) to Clack/Woo (event-driven asynchronous architecture) to achieve high-performance, scalable WebSocket handling capable of supporting thousands of concurrent connections with minimal resource overhead.

### Key Benefits
- **10x+ connection capacity**: Handle 1000+ concurrent WebSocket connections per server instance
- **Reduced memory footprint**: Event-driven architecture eliminates thread-per-connection overhead
- **Foundation for scalability**: Enables horizontal scaling with load balancer
- **Modern architecture**: Aligns with Node.js, Go, and Rust async patterns
- **Framework flexibility**: Clack provides portability across different servers

---

## Problem Statement

### Current Limitations

**Hunchentoot Architecture:**
- Thread-per-connection model limits concurrent connections to ~100-200
- Each WebSocket connection consumes ~1-2MB of memory for thread stack
- Context switching overhead degrades performance under high concurrency
- Difficult to horizontally scale without complex load balancing

**Observed Issues:**
- Memory usage spikes with 50+ concurrent users
- CPU time wasted on thread context switching
- WebSocket message latency increases under load
- Cannot support viral growth scenarios (100+ users on one canvas)

### Target Use Cases
1. **Viral Canvas Sessions**: 100+ designers collaborating on trending canvas
2. **Enterprise Deployments**: Multiple teams sharing single server instance
3. **Educational Use**: Classroom sessions with 30-50 students
4. **Public Demos**: High concurrent usage during product showcases

---

## Technical Architecture

### High-Level Design

```
┌─────────────────────────────────────────────────────────┐
│                    Clack Application                     │
│  (Platform-agnostic middleware/routing layer)            │
├─────────────────────────────────────────────────────────┤
│                     Woo Server                           │
│  (libev-based event loop, async I/O)                    │
├─────────────────────────────────────────────────────────┤
│                 Operating System                         │
│  (epoll/kqueue for socket events)                       │
└─────────────────────────────────────────────────────────┘

HTTP Requests → Clack → Routes → Handlers → JSON Response
WebSocket      → Clack → WS Upgrade → Event Handlers → Broadcast
```

### Component Architecture

#### 1. Clack Application Layer (`src/app.lisp`)

**Purpose:** Platform-agnostic request handling

```lisp
(defpackage #:collabcanvas.app
  (:use #:cl #:lack.component)
  (:export #:*app* #:make-app))

(defun make-app ()
  "Create Clack application with middleware stack"
  (lack:builder
   ;; Middleware stack (order matters!)
   (:static :path "/assets/" :root *frontend-path*)
   :accesslog
   (:cors :allow-origin "*"
          :allow-methods '(:GET :POST :OPTIONS)
          :allow-headers '("Content-Type" "Authorization"))

   ;; WebSocket upgrade middleware
   (:websocket :path "/ws/:canvas-id"
              :on-upgrade #'handle-websocket-upgrade
              :on-message #'handle-websocket-message
              :on-close #'handle-websocket-close)

   ;; Main application router
   #'route-handler))

(defun route-handler (env)
  "Main HTTP request router (Clack env format)"
  (let* ((path (getf env :path-info))
         (method (getf env :request-method)))
    (cond
      ;; API routes
      ((string= path "/api/register")
       (handle-register env))
      ((string= path "/api/login")
       (handle-login env))
      ((string= path "/health")
       (handle-health env))

      ;; Catch-all for SPA
      (t (serve-index-html)))))
```

**Key Changes from Hunchentoot:**
- Request data in `env` plist instead of special variables
- Must explicitly return `(values status headers body)`
- Middleware composition via `lack:builder`
- WebSocket handling via middleware, not inheritance

#### 2. WebSocket Adapter (`src/websocket-adapter.lisp`)

**Purpose:** Bridge Clack WebSocket API to canvas room system

```lisp
(defstruct ws-connection
  "Wrapper for Clack WebSocket connection"
  socket       ; Underlying WebSocket socket
  canvas-id    ; Canvas this connection belongs to
  user-id      ; Authenticated user ID
  username     ; Username for display
  color        ; Cursor color
  connected-at ; Timestamp
  )

(defun handle-websocket-upgrade (env)
  "Handle WebSocket upgrade handshake"
  (let* ((canvas-id (getf (getf env :path-params) :canvas-id))
         (ws-socket (getf env :websocket)))

    ;; Validate canvas ID
    (unless (valid-canvas-id-p canvas-id)
      (return-from handle-websocket-upgrade
        '(400 nil ("Invalid canvas ID"))))

    ;; Create connection wrapper
    (let ((conn (make-ws-connection
                 :socket ws-socket
                 :canvas-id canvas-id
                 :connected-at (get-universal-time))))

      ;; Store in global registry (pending auth)
      (register-pending-connection ws-socket conn)

      ;; Return 101 Switching Protocols
      '(101 nil nil))))

(defun handle-websocket-message (ws-socket message)
  "Handle incoming WebSocket message"
  (let ((conn (get-connection-by-socket ws-socket)))
    (unless conn
      (return-from handle-websocket-message))

    (handler-case
        (let* ((data (parse-json message))
               (msg-type (getf data :type)))

          (cond
            ;; Authentication must come first
            ((string= msg-type "auth")
             (handle-auth-message conn data))

            ;; Authenticated messages
            ((ws-connection-user-id conn)
             (dispatch-message conn data))

            ;; Not authenticated - reject
            (t
             (send-to-socket ws-socket
                           '(:type "error"
                             :message "Not authenticated")))))
      (error (e)
        (log-error "WebSocket message error: ~A" e)
        (send-to-socket ws-socket
                       `(:type "error"
                         :message ,(format nil "~A" e)))))))

(defun handle-websocket-close (ws-socket)
  "Handle WebSocket disconnection"
  (when-let ((conn (get-connection-by-socket ws-socket)))
    (let ((room (get-room (ws-connection-canvas-id conn))))
      (when room
        (remove-connection-from-room room conn)

        ;; Broadcast disconnection if authenticated
        (when (ws-connection-user-id conn)
          (broadcast-to-room room
                           `(:type "user-disconnected"
                             :user-id ,(ws-connection-user-id conn)
                             :username ,(ws-connection-username conn))))

        ;; Cleanup empty rooms
        (when (= 0 (room-connection-count room))
          (cleanup-room (ws-connection-canvas-id conn))))))

  ;; Remove from global registry
  (unregister-connection ws-socket))
```

**Async Considerations:**
- All I/O operations must be non-blocking
- Use Woo's native send functions, not Hunchensocket
- Connection lifecycle managed by Woo event loop
- No thread synchronization needed within single connection

#### 3. Server Lifecycle (`src/server.lisp`)

**Purpose:** Start/stop/restart Woo server

```lisp
(defparameter *server-handler* nil
  "Handle to running Woo server")

(defun start-server (&key (port *port*) (host *host*))
  "Start Woo server with Clack application"
  (format t "~%=== Starting CollabCanvas (Clack/Woo) ===~%")

  ;; Initialize database
  (init-database)
  (init-database-pool)

  ;; Create Clack application
  (let ((app (make-app)))

    ;; Start Woo server
    (setf *server-handler*
          (clack:clackup app
                        :server :woo
                        :address host
                        :port port
                        :debug *debug-mode*
                        :use-thread nil  ; Single event loop
                        :worker-num 1))  ; Single process for SQLite

    (format t "✓ Server started on ~A:~A~%" host port)
    (format t "  Architecture: Async (Woo + libev)~%")
    (format t "  WebSocket: ws://~A:~A/ws/<canvas-id>~%~%" host port)

    t))

(defun stop-server ()
  "Stop Woo server gracefully"
  (format t "~%Stopping server...~%")

  (when *server-handler*
    ;; Flush all canvas states
    (persist-all-canvas-states)

    ;; Close all WebSocket connections
    (close-all-connections)

    ;; Stop Woo
    (clack:stop *server-handler*)
    (setf *server-handler* nil))

  ;; Close database pool
  (close-database-pool)

  (format t "Server stopped.~%"))

(defun restart-server ()
  "Restart server with new code"
  (stop-server)
  (sleep 0.5)
  (start-server))
```

---

## Migration Strategy

### Phase 1: Preparation (Hours 0-2)

**Goal:** Set up Clack/Woo infrastructure without breaking existing code

**Tasks:**
1. Add dependencies to `collabcanvas.asd`:
   ```lisp
   :depends-on (:clack
                :woo
                :lack
                :lack-middleware-static
                :lack-middleware-cors
                :websocket-driver  ; For WebSocket protocol
                ...existing deps...)
   ```

2. Install and test dependencies:
   ```bash
   ros -e '(ql:quickload :woo)'
   ros -e '(ql:quickload :clack)'
   ros -e '(ql:quickload :lack)'
   ```

3. Create new files (keep old ones):
   - `src/app.lisp` - Clack application
   - `src/websocket-adapter.lisp` - WebSocket bridge
   - `src/server.lisp` - Woo server lifecycle

4. Verify Woo starts with minimal "hello world" app

### Phase 2: HTTP Routes Migration (Hours 2-4)

**Goal:** Port HTTP API endpoints to Clack

**Tasks:**
1. Convert `handle-register` to accept `env`:
   ```lisp
   ;; Before (Hunchentoot)
   (defun handle-register ()
     (let ((body (get-json-body)))
       ...))

   ;; After (Clack)
   (defun handle-register (env)
     (let ((body (parse-request-body env)))
       ...
       ;; Return (status headers body)
       (values 200
               '(:content-type "application/json")
               (to-json result))))
   ```

2. Port all HTTP handlers:
   - `handle-login`
   - `handle-logout`
   - `handle-session-check`
   - `handle-canvas-state`
   - `handle-health`

3. Create utility functions:
   ```lisp
   (defun parse-request-body (env)
     "Parse JSON request body from Clack env"
     (let* ((content-length (getf env :content-length))
            (input (getf env :raw-body)))
       (when (and input (> content-length 0))
         (parse-json (read-stream-to-string input)))))

   (defun json-response (data &optional (status 200))
     "Create JSON HTTP response"
     (values status
             '(:content-type "application/json")
             (to-json-string data)))
   ```

4. Test all HTTP endpoints with curl

### Phase 3: WebSocket Migration (Hours 4-8)

**Goal:** Replace Hunchensocket with Clack WebSocket middleware

**Tasks:**
1. Implement WebSocket upgrade handler
2. Convert `canvas-room` to use new connection objects
3. Port message handlers to new API:
   - `handle-auth-message`
   - `handle-cursor-update`
   - `handle-object-create`
   - `handle-object-update`
   - `handle-object-delete`

4. Update broadcasting to use Woo's send API:
   ```lisp
   (defun send-to-socket (ws-socket data)
     "Send JSON message to WebSocket"
     (let ((json (to-json-string data)))
       (websocket-driver:send ws-socket json)))

   (defun broadcast-to-room (room message)
     "Broadcast to all connections in room"
     (let ((json (to-json-string message)))
       (dolist (conn (room-connections room))
         (websocket-driver:send (ws-connection-socket conn) json))))
   ```

5. Test with frontend (ensure messages flow both ways)

### Phase 4: Performance Optimization (Hours 8-10)

**Goal:** Tune Woo for production workload

**Tasks:**
1. Configure Woo parameters:
   ```lisp
   (clackup app
            :server :woo
            :worker-num 1         ; Single process for SQLite
            :backlog 4096         ; Socket backlog
            :max-request-per-child nil  ; No restart
            :use-thread nil)      ; Pure async
   ```

2. Implement connection pooling for database:
   - Reuse existing `*database-pool*` from current implementation
   - Ensure thread-safe access from event loop

3. Add metrics:
   - Active connection count
   - Messages per second
   - Memory usage monitoring

4. Load test with 100+ concurrent connections

### Phase 5: Cutover (Hours 10-12)

**Goal:** Remove Hunchentoot, make Clack/Woo default

**Tasks:**
1. Update `main.lisp` to call new `start-server`
2. Remove Hunchentoot code:
   - Delete old `setup-routes`
   - Remove `hunchensocket` classes
   - Clean up `collabcanvas.asd` dependencies

3. Update documentation:
   - README installation instructions
   - Architecture diagrams
   - API documentation

4. Test full application end-to-end
5. Deploy to staging environment
6. Monitor for 24 hours before production

---

## API Changes

### Request Handler Signature

**Before (Hunchentoot):**
```lisp
(defun handle-login ()
  (let ((data (get-json-body))
        (method (hunchentoot:request-method*)))
    ...
    (json-response result)))
```

**After (Clack):**
```lisp
(defun handle-login (env)
  (let ((data (parse-request-body env))
        (method (getf env :request-method)))
    ...
    (values 200
            '(:content-type "application/json")
            (to-json-string result))))
```

### WebSocket Handler Signature

**Before (Hunchensocket):**
```lisp
(defmethod hunchensocket:text-message-received
    ((resource canvas-websocket-resource) websocket message)
  (let ((data (parse-json message)))
    ...
    (hunchensocket:send-text-message websocket response)))
```

**After (Clack + WebSocket Driver):**
```lisp
(defun handle-websocket-message (ws-socket message)
  (let ((data (parse-json message)))
    ...
    (websocket-driver:send ws-socket response)))
```

### Environment Access

| Hunchentoot | Clack (env plist) |
|-------------|-------------------|
| `(hunchentoot:script-name*)` | `(getf env :path-info)` |
| `(hunchentoot:request-method*)` | `(getf env :request-method)` |
| `(hunchentoot:header-in* :authorization)` | `(getf (getf env :headers) "authorization")` |
| `(hunchentoot:cookie-in "session")` | `(getf (getf env :cookies) "session")` |
| `(hunchentoot:raw-post-data)` | `(getf env :raw-body)` |

---

## Testing Strategy

### Unit Tests

**Test: Clack Application Creation**
```lisp
(deftest test-make-app
  (let ((app (make-app)))
    (ok (functionp app) "App is a function")
    (ok (not (null *frontend-path*)) "Frontend path is set")))
```

**Test: Request Routing**
```lisp
(deftest test-route-handler
  (let ((env '(:path-info "/health" :request-method :get)))
    (multiple-value-bind (status headers body)
        (funcall (make-app) env)
      (ok (= status 200))
      (ok (search "healthy" body)))))
```

**Test: WebSocket Upgrade**
```lisp
(deftest test-websocket-upgrade
  (let ((env '(:path-info "/ws/test-canvas"
               :path-params (:canvas-id "test-canvas"))))
    (multiple-value-bind (status headers body)
        (handle-websocket-upgrade env)
      (ok (= status 101) "Returns 101 Switching Protocols"))))
```

### Integration Tests

**Test: Full HTTP Flow**
1. Start Clack server on test port
2. Send POST /api/register with curl
3. Verify 200 response with session
4. Send POST /api/login
5. Stop server

**Test: WebSocket Connection**
1. Start server
2. Connect WebSocket client (Node.js script)
3. Send auth message
4. Verify auth-success response
5. Send cursor update
6. Verify broadcast to second client
7. Disconnect and verify cleanup

### Performance Tests

**Test: Concurrent Connections**
```bash
# Use wrk for HTTP load testing
wrk -t4 -c100 -d30s http://localhost:8080/health

# Use thor for WebSocket testing
thor ws://localhost:8080/ws/test-canvas --amount 100 --messages 1000
```

**Expected Results:**
- HTTP: 5000+ req/sec with 100 concurrent connections
- WebSocket: 100+ concurrent connections with <5ms latency
- Memory: <500MB for 100 connections (<5MB per connection)

---

## Performance Targets

| Metric | Current (Hunchentoot) | Target (Woo) | Measurement |
|--------|----------------------|--------------|-------------|
| Max concurrent WebSocket connections | ~100 | 1000+ | Load test |
| Memory per connection | ~2MB | <500KB | Process monitor |
| Message latency (P95) | ~50ms | <10ms | WebSocket ping |
| CPU usage (100 users) | ~80% | <30% | `top` command |
| Messages per second | ~1000 | 10,000+ | Custom counter |

---

## Dependencies

### New Quicklisp Libraries

```lisp
(ql:quickload :clack)        ; Web application framework
(ql:quickload :woo)           ; Fast HTTP server
(ql:quickload :lack)          ; Middleware library
(ql:quickload :websocket-driver)  ; WebSocket protocol
(ql:quickload :fast-http)     ; HTTP parser (Woo dependency)
(ql:quickload :quri)          ; URI parsing
```

### System Requirements
- SBCL 2.0+
- libev (system library, `apt-get install libev-dev`)
- Linux or macOS (Windows support limited)

---

## Risks & Mitigations

### Risk: Breaking Changes During Migration

**Probability:** High
**Impact:** High
**Mitigation:**
- Feature flag to switch between Hunchentoot and Woo
- Parallel development (keep both implementations)
- Incremental rollout (HTTP first, WebSocket second)
- Comprehensive integration tests before cutover

### Risk: Woo Stability Issues

**Probability:** Medium
**Impact:** High
**Mitigation:**
- Woo is production-proven (used by Japanese companies)
- Monitor error logs closely during first week
- Keep Hunchentoot code for 1 month as rollback option
- Document all Woo-specific issues and workarounds

### Risk: Single-threaded Event Loop Bottleneck

**Probability:** Low
**Impact:** Medium
**Mitigation:**
- Profile with `sb-sprof` to identify blocking operations
- Move heavy computation to background threads
- Use `bt:make-thread` for database writes if needed
- Consider multi-process architecture with Nginx load balancer

### Risk: SQLite Locking with Async I/O

**Probability:** Medium
**Impact:** Medium
**Mitigation:**
- Use WAL mode (`PRAGMA journal_mode=WAL`)
- Connection pool with exclusive writes
- Queue writes to background thread if contention detected
- Monitor SQLite busy errors and retry

---

## Success Criteria

### Must Have (MVP)
- ✅ All HTTP endpoints working with Clack
- ✅ WebSocket connections stable (auth, messages, disconnect)
- ✅ Can handle 100+ concurrent WebSocket connections
- ✅ Message latency <50ms at 100 users
- ✅ Memory usage <500MB at 100 connections
- ✅ Zero regression in existing functionality

### Nice to Have (Post-MVP)
- Multi-process Woo workers with Nginx load balancer
- Binary WebSocket protocol for bandwidth savings
- WebSocket compression (permessage-deflate)
- HTTP/2 support for frontend assets
- Prometheus metrics endpoint

### Quality Gates
1. **Performance**: Load test shows 10x improvement in concurrent connections
2. **Stability**: No crashes or memory leaks in 24-hour soak test
3. **Compatibility**: Works with existing frontend (no changes required)
4. **Documentation**: Architecture docs updated, migration guide written
5. **Monitoring**: Metrics dashboard shows connection count, message rate, latency

---

## Documentation Updates

### Files to Update
1. `README.md` - Installation instructions, dependencies
2. `COLLABCANVAS_ARCHITECTURE.md` - Server architecture section
3. `QUICKSTART_GUIDE.md` - Development setup
4. `CLAUDE.md` - Server startup commands

### New Documentation
1. `docs/clack-migration-guide.md` - For future contributors
2. `docs/websocket-protocol.md` - Updated with Clack details
3. `docs/performance-tuning.md` - Woo configuration tips

---

## Rollout Plan

### Week 1: Development
- Mon-Wed: HTTP routes migration
- Thu-Fri: WebSocket migration
- Sat-Sun: Testing and bug fixes

### Week 2: Staging
- Deploy to staging environment
- Invite team for testing (20-30 users)
- Monitor metrics, fix issues

### Week 3: Production
- Deploy to production during low-traffic window
- Monitor closely for 48 hours
- Keep Hunchentoot branch for quick rollback
- Document any issues and resolutions

---

## Appendix

### Woo Configuration Options

```lisp
(clack:clackup app
  :server :woo
  :address "0.0.0.0"
  :port 8080
  :debug nil                    ; Disable debug mode in production
  :worker-num 1                 ; Single process (SQLite limitation)
  :backlog 4096                 ; OS socket backlog
  :max-request-per-child nil    ; Don't restart worker
  :use-thread nil)              ; Pure async (no thread pool)
```

### Performance Tuning

**Linux Kernel Settings:**
```bash
# Increase file descriptor limit
ulimit -n 65535

# TCP tuning
sysctl -w net.core.somaxconn=4096
sysctl -w net.ipv4.tcp_max_syn_backlog=4096
```

**SBCL Tuning:**
```lisp
;; Increase heap size
(sb-ext:gc :full t)
(setf (sb-ext:bytes-consed-between-gcs) 67108864) ; 64MB

;; Enable deadlock detection
(setf sb-thread:*deadlock-detection* t)
```

### Monitoring Queries

```lisp
;; Check active connections
(defun get-connection-stats ()
  `(:total-connections ,(hash-table-count *ws-connections*)
    :total-rooms ,(hash-table-count *canvas-rooms*)
    :memory-mb ,(/ (sb-ext:dynamic-space-size) 1048576)))

;; Log stats every minute
(bt:make-thread
  (lambda ()
    (loop
      (format t "Stats: ~A~%" (get-connection-stats))
      (sleep 60))))
```

---

**Document Version:** 1.0
**Last Reviewed:** October 2025
**Next Review:** November 2025
**Owner:** Backend Team
