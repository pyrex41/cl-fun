# Bug Fixes - October 16, 2025

## Object Persistence & User Presence Fixes

### Issues Fixed

1. **Object Creation Not Persisting** - New objects would appear but disappear after page reload
2. **Object Updates Not Persisting** - Moving/dragging objects wouldn't save to database
3. **User Presence Showing Duplicates** - Users appeared multiple times in the online users list

---

## 1. Object Persistence Fix

### Problem
Objects created in the canvas were not persisting to the database after page reload. Backend was receiving object-create messages but the `object-id` was coming back as `NIL`.

### Root Cause
Jonathan JSON parser behavior with nested objects:
- Top-level keys are parsed as uppercase keywords (`:TYPE`, `:OBJECT`)
- **Nested object keys are parsed as STRINGS** (`"id"`, `"type"`, `"color"`, `"x"`, `"y"`)

The code was looking for keyword `:id` but the actual key was the string `"id"`.

### Solution
Changed key lookup to use string-based comparison:

```lisp
;; Before (incorrect - looking for keyword)
(object-id (cdr (assoc :id object-data)))

;; After (correct - using string key with string comparison)
(object-id (cdr (assoc "id" object-data :test #'string=)))
```

Also updated key conversion for database storage:

```lisp
;; Convert string keys to keyword keys for database
(mapcar (lambda (pair)
          (cons (intern (string-upcase (car pair)) :keyword)
                (cdr pair)))
        object-data)
```

### Files Modified
- `backend/src/websocket-adapter.lisp`
  - `handle-object-create-message` (lines 283-319)
  - `handle-object-update-message` (lines 321-363)

---

## 2. User Presence Deduplication Fix

### Problem
When users reload the page or have multiple tabs open, they appear multiple times in the "Online Users" list.

### Root Cause
The `get-room-users` function was iterating through all WebSocket connections and adding every connection to the user list. Since users can have multiple connections (multiple tabs, reconnections after page reload), the same user would appear multiple times.

### Solution
Added deduplication by tracking seen user IDs using a hash table:

```lisp
(defun get-room-users (canvas-id)
  "Get all authenticated users in a canvas room.
   Returns list of alists with user-id, username, and color.
   Deduplicates users by user-id (users may have multiple connections/tabs)."
  (let ((conn-ids (get-room-connections canvas-id))
        (seen-user-ids (make-hash-table :test 'equal))
        (users nil))
    (dolist (conn-id conn-ids)
      (let ((conn (get-ws-connection conn-id)))
        (when (and conn
                   (ws-connection-user-id conn)
                   (ws-connection-username conn))
          (let ((user-id (ws-connection-user-id conn)))
            ;; Only add user if we haven't seen this user-id yet
            (unless (gethash user-id seen-user-ids)
              (setf (gethash user-id seen-user-ids) t)
              (push `((:user-id . ,user-id)
                      (:username . ,(ws-connection-username conn))
                      (:color . ,(generate-user-color user-id)))
                    users))))))
    (nreverse users)))
```

### Files Modified
- `backend/src/websocket-adapter.lisp`
  - `get-room-users` (lines 121-141)

---

## 3. User Presence Implementation

### Features Added
- **Real-time presence tracking** - Shows all currently connected users
- **Consistent user colors** - Each user gets a consistent color based on their user ID
- **Automatic updates** - Presence list updates when users connect/disconnect
- **Deduplication** - Each user appears only once regardless of number of connections

### Backend Implementation

#### Helper Functions

```lisp
(defun get-room-users (canvas-id)
  "Get all authenticated users with deduplication by user-id"
  ;; Returns: (((user-id . 5) (username . "test1") (color . "#3498db")) ...)
)

(defun generate-user-color (user-id)
  "Generate consistent color from predefined palette based on user-id"
  (let* ((colors '("#3498db" "#e74c3c" "#2ecc71" "#f39c12" "#9b59b6"
                   "#1abc9c" "#e67e22" "#34495e" "#16a085" "#c0392b"))
         (index (mod user-id (length colors))))
    (nth index colors)))
```

#### Presence Broadcasting

**On User Authentication:**
```lisp
;; Broadcast user-connected to others (excluding the connecting user)
(broadcast-to-canvas-room canvas-id
  (to-json-string
   `((:type . "user-connected")
     (:user-id . ,user-id)
     (:username . ,username)
     (:color . ,(generate-user-color user-id))))
  conn-id)

;; Send presence update to ALL users (including the connecting user)
(let ((users (get-room-users canvas-id)))
  (broadcast-to-canvas-room canvas-id
    (to-json-string
     `((:type . "presence")
       (:users . ,users)))
    nil))  ; nil = send to everyone
```

**On User Disconnect:**
```lisp
;; Broadcast user-disconnected
(broadcast-to-canvas-room canvas-id
  (to-json-string
   `((:type . "user-disconnected")
     (:user-id . ,user-id)
     (:username . ,username)))
  nil)

;; Send updated presence list after user leaves
(let ((users (get-room-users canvas-id)))
  (broadcast-to-canvas-room canvas-id
    (to-json-string
     `((:type . "presence")
       (:users . ,users)))
    nil))
```

### Frontend Integration

The frontend already had presence UI components and handlers in place. No frontend changes were needed.

---

## Testing

### Test Scenarios

1. **Object Creation Persistence**
   - Create objects (rectangles, circles)
   - Reload page
   - ✅ Objects should persist

2. **Object Update Persistence**
   - Drag/move objects
   - Reload page
   - ✅ New positions should persist

3. **Single User Presence**
   - Login and connect
   - ✅ Should see own username in presence list

4. **Multi-User Presence**
   - Open multiple tabs with different users
   - ✅ Each user should appear once in the list
   - ✅ Each user should have a colored indicator

5. **Presence Deduplication**
   - Open multiple tabs with same user
   - Reload pages multiple times
   - ✅ User should appear only once in the list

---

## Performance Considerations

### Hash Table for Deduplication
Using Common Lisp's built-in hash table provides O(1) lookup time for checking if a user-id has already been seen. For typical canvas rooms with 10-100 users, this is highly efficient.

### Memory Impact
Minimal - the hash table is created on each `get-room-users` call and immediately garbage collected after the function returns.

---

## Debugging Tips

### Enable Debug Logging
The presence implementation includes debug logging:

```
[WS AUTH DEBUG] Room users for default-canvas: (((USER-ID . 5) (USERNAME . test1) (COLOR . #1abc9c)))
```

### Check Broadcast Count
Look for these log lines to verify messages are being sent:

```
[WS] Broadcast to canvas default-canvas: 1/2 connections
```

Format: `sent_count/total_connections`

---

## Related Files

### Backend
- `backend/src/websocket-adapter.lisp` - WebSocket message handlers, presence logic
- `backend/src/canvas-state.lisp` - Canvas state persistence
- `backend/src/database.lisp` - Database operations

### Frontend
- `frontend/src/main.js` - Presence UI updates (`updatePresenceList`)
- `frontend/src/websocket.js` - WebSocket client, message handling

---

## Future Enhancements

1. **User Status Indicators** - Show user activity (idle, typing, etc.)
2. **User Avatars** - Display user profile pictures
3. **Cursor Following** - Click a user to follow their cursor
4. **User Filtering** - Filter presence list by activity or name
5. **Presence Timeouts** - Mark users as "away" after inactivity

---

## Commit History

- `fix: resolve JSON key parsing for object creation and updates` - Object persistence fix
- `fix: deduplicate users in presence list to prevent duplicate entries` - Presence deduplication fix
