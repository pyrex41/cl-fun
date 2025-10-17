;;;; websocket-adapter.lisp - WebSocket connection adapter for Clack/Woo
;;;; This file provides WebSocket connection management and message routing

(in-package #:collabcanvas)

;;; WebSocket Connection Structure

(defstruct ws-connection
  "WebSocket connection data structure.
   Stores connection metadata and state for real-time communication."
  (id nil :type (or null string))
  (websocket nil :type t)
  (user-id nil :type (or null integer))
  (username nil :type (or null string))
  (canvas-id nil :type (or null string))
  (session-id nil :type (or null string))
  (connected-at nil :type (or null integer)))

;;; Connection Registry

(defvar *ws-connections* (make-hash-table :test 'equal)
  "Global registry of active WebSocket connections.
   Key: connection-id (string), Value: ws-connection struct")

(defvar *ws-connections-lock* (bt:make-lock "ws-connections-lock")
  "Lock for thread-safe access to *ws-connections*")

(defvar *canvas-rooms* (make-hash-table :test 'equal)
  "Registry of canvas rooms.
   Key: canvas-id (string), Value: list of connection-ids")

(defvar *canvas-rooms-lock* (bt:make-lock "canvas-rooms-lock")
  "Lock for thread-safe access to *canvas-rooms*")

(defvar *stats-timer* nil
  "Background timer for stats logging")

;;; Connection Statistics

(defun get-connection-stats ()
  "Get current connection statistics for monitoring."
  `((:timestamp . ,(get-universal-time))
    (:total-connections . ,(bt:with-lock-held (*ws-connections-lock*)
                             (hash-table-count *ws-connections*)))
    (:total-rooms . ,(bt:with-lock-held (*canvas-rooms-lock*)
                       (hash-table-count *canvas-rooms*)))
    (:memory-mb . ,(/ (sb-ext:dynamic-space-size) 1048576))
    (:threads . ,(length (bt:all-threads)))))

(defun start-stats-logging ()
  "Start background stats logging every 60 seconds."
  (when *stats-timer*
    (format t "[WARN] Stats logging already running~%")
    (return-from start-stats-logging nil))

  (setf *stats-timer*
        (bt:make-thread
         (lambda ()
           (loop
             (sleep 60)
             (let ((stats (get-connection-stats)))
               (format t "[STATS] ~A~%" stats))))
         :name "stats-logger"))
  (format t "[INFO] Stats logging started~%"))

(defun stop-stats-logging ()
  "Stop background stats logging."
  (when *stats-timer*
    (bt:destroy-thread *stats-timer*)
    (setf *stats-timer* nil)
    (format t "[INFO] Stats logging stopped~%")))

;;; Connection Management Functions

(defun register-ws-connection (conn)
  "Register a WebSocket connection in the global registry."
  (bt:with-lock-held (*ws-connections-lock*)
    (setf (gethash (ws-connection-id conn) *ws-connections*) conn))

  ;; Add to canvas room if canvas-id is set
  (when (ws-connection-canvas-id conn)
    (add-connection-to-room conn (ws-connection-canvas-id conn))))

(defun unregister-ws-connection (conn-id)
  "Remove a WebSocket connection from the global registry."
  (let ((conn (get-ws-connection conn-id)))
    (when conn
      ;; Remove from canvas room
      (when (ws-connection-canvas-id conn)
        (remove-connection-from-room conn-id (ws-connection-canvas-id conn)))

      ;; Remove from global registry
      (bt:with-lock-held (*ws-connections-lock*)
        (remhash conn-id *ws-connections*)))))

(defun get-ws-connection (conn-id)
  "Retrieve a WebSocket connection by ID."
  (bt:with-lock-held (*ws-connections-lock*)
    (gethash conn-id *ws-connections*)))

(defun add-connection-to-room (conn canvas-id)
  "Add a connection to a canvas room."
  (bt:with-lock-held (*canvas-rooms-lock*)
    (let ((room (gethash canvas-id *canvas-rooms*)))
      (unless (member (ws-connection-id conn) room :test #'string=)
        (setf (gethash canvas-id *canvas-rooms*)
              (cons (ws-connection-id conn) room))))))

(defun remove-connection-from-room (conn-id canvas-id)
  "Remove a connection from a canvas room."
  (bt:with-lock-held (*canvas-rooms-lock*)
    (let ((room (gethash canvas-id *canvas-rooms*)))
      (setf (gethash canvas-id *canvas-rooms*)
            (remove conn-id room :test #'string=)))))

(defun get-room-connections (canvas-id)
  "Get all connection IDs in a canvas room."
  (bt:with-lock-held (*canvas-rooms-lock*)
    (copy-list (gethash canvas-id *canvas-rooms*))))

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

(defun generate-user-color (user-id)
  "Generate a consistent color for a user based on their ID."
  (let* ((colors '("#3498db" "#e74c3c" "#2ecc71" "#f39c12" "#9b59b6"
                   "#1abc9c" "#e67e22" "#34495e" "#16a085" "#c0392b"))
         (index (mod user-id (length colors))))
    (nth index colors)))

;;; Message Handling Stubs

(defun handle-ws-connect (websocket)
  "Handle new WebSocket connection.
   TODO: Implement in Task 6 (Port WebSocket Logic)"
  (let ((conn-id (format nil "ws-~A" (get-universal-time))))
    (format t "[WS] New connection: ~A~%" conn-id)

    (let ((conn (make-ws-connection
                 :id conn-id
                 :websocket websocket
                 :connected-at (get-universal-time))))
      (register-ws-connection conn)
      conn-id)))

(defun handle-ws-message (conn-id message)
  "Handle incoming WebSocket message - parses JSON and dispatches to handlers."
  (handler-case
      (let* ((parsed (jonathan:parse message :as :alist))
             ;; Convert string keys to keyword keys (same as HTTP body parsing)
             (data (mapcar (lambda (pair)
                            (cons (intern (string-upcase (string (car pair))) :keyword)
                                  (cdr pair)))
                          parsed))
             (msg-type (cdr (assoc :type data)))
             (conn (get-ws-connection conn-id)))

        (unless conn
          (format t "[WS WARN] Message from unknown connection: ~A~%" conn-id)
          (return-from handle-ws-message nil))

        (format t "[WS] Message type: ~A from conn: ~A~%" msg-type conn-id)

        ;; Dispatch based on message type
        (cond
          ;; Authentication
          ((string= msg-type "auth")
           (handle-auth-message conn-id data))

          ;; Cursor movement
          ((string= msg-type "cursor")
           (handle-cursor-message conn-id data))

          ;; Object operations
          ((string= msg-type "object-create")
           (handle-object-create-message conn-id data))

          ((string= msg-type "object-update")
           (handle-object-update-message conn-id data))

          ((string= msg-type "object-delete")
           (handle-object-delete-message conn-id data))

          ;; AI command
          ((string= msg-type "ai-command")
           (handle-ai-command-message conn-id data))

          ;; Physics control
          ((string= msg-type "physics-control")
           (handle-physics-control-message conn-id data))

          ;; Unknown message type
          (t
           (format t "[WS WARN] Unknown message type: ~A~%" msg-type))))

    (error (e)
      (format t "[WS ERROR] Failed to process message: ~A~%" e))))

(defun handle-ws-disconnect (conn-id)
  "Handle WebSocket disconnection."
  (let* ((conn (get-ws-connection conn-id))
         (user-id (when conn (ws-connection-user-id conn)))
         (username (when conn (ws-connection-username conn)))
         (canvas-id (when conn (ws-connection-canvas-id conn))))

    (format t "[WS] Connection closed: ~A~%" conn-id)

    ;; Unregister connection first
    (unregister-ws-connection conn-id)

    ;; If user was authenticated, notify others
    (when (and user-id username canvas-id)
      ;; Broadcast user disconnected
      (broadcast-to-canvas-room canvas-id
        (to-json-string
         `((:type . "user-disconnected")
           (:user-id . ,user-id)
           (:username . ,username)))
        nil)  ; nil = send to everyone

      ;; Send updated presence list
      (let ((users (get-room-users canvas-id)))
        (broadcast-to-canvas-room canvas-id
          (to-json-string
           `((:type . "presence")
             (:users . ,users)))
          nil))

      (format t "[WS] User ~A disconnected from canvas ~A~%" username canvas-id))))

(defun send-ws-message (conn-id message)
  "Send a message to a specific WebSocket connection using websocket-driver."
  (let ((conn (get-ws-connection conn-id)))
    (if conn
        (let ((ws (ws-connection-websocket conn)))
          (when ws
            (handler-case
                (progn
                  (websocket-driver:send ws message)
                  t)
              (error (e)
                (format t "[WS ERROR] Failed to send message to ~A: ~A~%" conn-id e)
                nil))))
        (progn
          (format t "[WS WARN] Connection ~A not found~%" conn-id)
          nil))))

(defun broadcast-to-canvas-room (canvas-id message &optional exclude-conn-id)
  "Broadcast a message to all connections in a canvas room."
  (let ((conn-ids (get-room-connections canvas-id))
        (sent-count 0))
    (dolist (conn-id conn-ids)
      (unless (and exclude-conn-id (string= conn-id exclude-conn-id))
        (when (send-ws-message conn-id message)
          (incf sent-count))))
    (format t "[WS] Broadcast to canvas ~A: ~A/~A connections~%"
            canvas-id sent-count (length conn-ids))
    sent-count))

;;; WebSocket Message Handlers

(defun handle-auth-message (conn-id data)
  "Handle WebSocket authentication message."
  (format t "[WS AUTH DEBUG] Received data keys: ~A~%" (mapcar #'car data))
  (let* ((conn (get-ws-connection conn-id))
         (session-id (or (cdr (assoc :session-id data))
                         (cdr (assoc :sessionid data))))  ; Try both formats
         (canvas-id (ws-connection-canvas-id conn)))
    (format t "[WS AUTH DEBUG] Extracted session-id: ~A~%" session-id)

    (if-let ((session (validate-session session-id)))
      (let ((user-id (cdr (assoc :user-id session)))
            (username (cdr (assoc :username session))))

        ;; Update connection with auth info
        (setf (ws-connection-user-id conn) user-id)
        (setf (ws-connection-username conn) username)
        (setf (ws-connection-session-id conn) session-id)

        ;; Send auth success with canvas state
        (let ((canvas-objects (get-canvas-objects canvas-id)))
          (format t "[WS AUTH] Loading canvas state for ~A: ~A objects~%"
                  canvas-id (length canvas-objects))
          (format t "[WS AUTH] Canvas objects: ~A~%" canvas-objects)

          ;; Sync all canvas objects to physics engine if not already loaded
          (handler-case
              (progn
                (format t "[WS AUTH] Syncing ~A objects to physics engine~%" (length canvas-objects))
                (dolist (obj canvas-objects)
                  (handler-case
                      (sync-canvas-object-to-physics obj)
                    (error (e)
                      (format t "[WS AUTH WARN] Failed to sync object ~A to physics: ~A~%"
                              (cdr (assoc :id obj)) e)))))
            (error (e)
              (format t "[WS AUTH WARN] Failed to sync canvas to physics: ~A~%" e)))

          (send-ws-message conn-id
            (to-json-string
             `((:type . "auth-success")
               (:user-id . ,user-id)
               (:username . ,username)
               (:canvas-state . ,canvas-objects)))))

        ;; Broadcast user connected
        (broadcast-to-canvas-room canvas-id
          (to-json-string
           `((:type . "user-connected")
             (:user-id . ,user-id)
             (:username . ,username)
             (:color . ,(generate-user-color user-id))))
          conn-id)

        ;; Send presence update to ALL users in the room (including this user)
        (let ((users (get-room-users canvas-id)))
          (format t "[WS AUTH DEBUG] Room users for ~A: ~A~%" canvas-id users)
          (broadcast-to-canvas-room canvas-id
            (to-json-string
             `((:type . "presence")
               (:users . ,users)))
            nil))  ; nil = send to everyone, don't exclude anyone

        (format t "[WS] User ~A authenticated on canvas ~A~%" username canvas-id))

      ;; Auth failed
      (send-ws-message conn-id
        (to-json-string
         '((:type . "auth-failed")
           (:message . "Invalid or expired session")))))))

(defun handle-cursor-message (conn-id data)
  "Handle cursor position update."
  (let* ((conn (get-ws-connection conn-id))
         (x (cdr (assoc :x data)))
         (y (cdr (assoc :y data)))
         (user-id (ws-connection-user-id conn))
         (username (ws-connection-username conn))
         (canvas-id (ws-connection-canvas-id conn)))

    (when (and user-id username)
      (broadcast-to-canvas-room canvas-id
        (to-json-string
         `((:type . "cursor")
           (:user-id . ,user-id)
           (:username . ,username)
           (:x . ,x)
           (:y . ,y)))
        conn-id))))

(defun handle-object-create-message (conn-id data)
  "Handle object creation."
  (let* ((conn (get-ws-connection conn-id))
         (object-data (cdr (assoc :object data))))

    ;; Debug: print what we received
    (format t "[WS DEBUG] object-data: ~A~%" object-data)
    (format t "[WS DEBUG] object-data keys: ~A~%" (mapcar #'car object-data))

    (let* ((object-id (cdr (assoc "id" object-data :test #'string=)))
           (user-id (ws-connection-user-id conn))
           (username (ws-connection-username conn))
           (canvas-id (ws-connection-canvas-id conn)))

      (format t "[WS DEBUG] object-id: ~A, user-id: ~A~%" object-id user-id)

      (when (and user-id object-id)
      ;; Convert object-data keys to keywords to match database format
      ;; Jonathan parses nested objects with string keys like "id", "type", "color"
      ;; but database expects keyword keys like :ID, :TYPE, :COLOR
      (let ((object-data-keywords (mapcar (lambda (pair)
                                            (cons (intern (string-upcase (car pair)) :keyword)
                                                  (cdr pair)))
                                          object-data)))
        ;; Save to canvas state with converted keys
        (update-canvas-object canvas-id object-id object-data-keywords user-id)

        ;; Sync to physics engine
        (handler-case
            (sync-canvas-object-to-physics object-data-keywords)
          (error (e)
            (format t "[WS WARN] Failed to sync object ~A to physics: ~A~%" object-id e)))

        ;; Broadcast to room (use converted keys for consistency)
        (broadcast-to-canvas-room canvas-id
          (to-json-string
           `((:type . "object-create")
             (:object . ,object-data-keywords)
             (:user-id . ,user-id)
             (:username . ,username)))
          conn-id)

        (format t "[WS] Object created: ~A by ~A~%" object-id username))))))

(defun handle-object-update-message (conn-id data)
  "Handle object update."
  (let* ((conn (get-ws-connection conn-id))
         (object-id (cdr (assoc :object-id data)))
         (updates (cdr (assoc :updates data)))
         (user-id (ws-connection-user-id conn))
         (username (ws-connection-username conn))
         (canvas-id (ws-connection-canvas-id conn)))

    (when (and user-id object-id updates)
      ;; Get current object and merge updates
      (let ((current-object (get-canvas-object canvas-id object-id)))
        (when current-object
          ;; Convert updates keys to keywords to match database format
          ;; Jonathan parses nested objects with string keys like "x", "y"
          ;; but database expects keyword keys like :X, :Y
          (let* ((updates-alist (mapcar (lambda (pair)
                                          (cons (intern (string-upcase (car pair)) :keyword)
                                                (cdr pair)))
                                        updates))
                 ;; Extract update keys for comparison
                 (update-keys (mapcar #'car updates-alist))
                 ;; Remove old values from current object that are being updated
                 (filtered-current (remove-if
                                    (lambda (pair) (member (car pair) update-keys))
                                    current-object))
                 ;; Merge: new updates + remaining current values
                 (updated-object (append updates-alist filtered-current)))

            ;; Save updated object to database
            (update-canvas-object canvas-id object-id updated-object user-id)

            ;; Sync position to physics engine if x/y changed (user drag)
            (when (or (assoc :X updates-alist) (assoc :Y updates-alist))
              (handler-case
                  (let ((x (or (cdr (assoc :X updates-alist))
                               (cdr (assoc :X current-object))))
                        (y (or (cdr (assoc :Y updates-alist))
                               (cdr (assoc :Y current-object)))))
                    (when (and x y)
                      (update-physics-object-position object-id x y)
                      (format t "[WS] Synced physics position for ~A: (~A, ~A)~%" object-id x y)))
                (error (e)
                  (format t "[WS WARN] Failed to sync position to physics: ~A~%" e))))

            ;; Broadcast update to other users in the room
            (broadcast-to-canvas-room canvas-id
              (to-json-string
               `((:type . "object-update")
                 (:object-id . ,object-id)
                 (:delta . ,updates-alist)
                 (:user-id . ,user-id)
                 (:username . ,username)))
              conn-id)

            (format t "[WS] Object updated: ~A by ~A~%" object-id username)))))))

(defun handle-object-delete-message (conn-id data)
  "Handle object deletion."
  (let* ((conn (get-ws-connection conn-id))
         (object-id (cdr (assoc :object-id data)))
         (user-id (ws-connection-user-id conn))
         (username (ws-connection-username conn))
         (canvas-id (ws-connection-canvas-id conn)))

    (when (and user-id object-id)
      ;; Delete from canvas state
      (when (delete-canvas-object canvas-id object-id user-id)
        ;; Remove from physics engine
        (handler-case
            (progn
              (remove-physics-object object-id)
              (format t "[WS] Removed object ~A from physics~%" object-id))
          (error (e)
            (format t "[WS WARN] Failed to remove object from physics: ~A~%" e)))

        ;; Broadcast deletion
        (broadcast-to-canvas-room canvas-id
          (to-json-string
           `((:type . "object-delete")
             (:object-id . ,object-id)
             (:user-id . ,user-id)
             (:username . ,username)))
          conn-id)

        (format t "[WS] Object deleted: ~A by ~A~%" object-id username)))))

(defun handle-ai-command-message (conn-id data)
  "Handle AI command message - processes natural language commands to generate UI components."
  (let* ((conn (get-ws-connection conn-id))
         (command (cdr (assoc :command data)))
         (user-id (ws-connection-user-id conn))
         (username (ws-connection-username conn))
         (canvas-id (ws-connection-canvas-id conn)))

    (unless (and user-id command)
      (format t "[WS WARN] AI command missing user-id or command~%")
      (return-from handle-ai-command-message nil))

    (format t "[WS AI] Processing command from ~A: ~A~%" username command)

    ;; Execute AI command in background thread (async)
    (bt:make-thread
     (lambda ()
       (handler-case
           (progn
             ;; Check rate limit
             (check-ai-rate-limit user-id)

             ;; Get canvas state for context
             (let* ((canvas-state (get-canvas-objects canvas-id))
                    (objects (execute-ai-command command canvas-id canvas-state user-id)))

               ;; Broadcast each object creation
               (dolist (obj objects)
                 (let ((obj-id (cdr (assoc :id obj))))
                   ;; Save to canvas state
                   (update-canvas-object canvas-id obj-id obj user-id)

                   ;; Broadcast to room
                   (broadcast-to-canvas-room canvas-id
                     (to-json-string
                      `((:type . "object-create")
                        (:object . ,obj)
                        (:user-id . ,user-id)
                        (:username . ,username)
                        (:ai-generated . t)))
                     nil)))  ; Send to everyone

               ;; Send success message
               (send-ws-message conn-id
                 (to-json-string
                  `((:type . "ai-command-success")
                    (:objects-created . ,(length objects))
                    (:command . ,command))))

               (format t "[WS AI] Generated ~A objects for command: ~A~%"
                      (length objects) command)))

         (error (e)
           (format t "[WS AI ERROR] AI command failed: ~A~%" e)
           (send-ws-message conn-id
             (to-json-string
              `((:type . "ai-command-error")
                (:error . ,(format nil "~A" e))
                (:command . ,command)))))))
     :name (format nil "ai-command-~A" (get-universal-time))))

(defun handle-physics-control-message (conn-id data)
  "Handle physics control message - processes simulation control commands."
  (let* ((conn (get-ws-connection conn-id))
         (action (cdr (assoc :action data)))
         (value (cdr (assoc :value data)))
         (user-id (ws-connection-user-id conn))
         (username (ws-connection-username conn))
         (canvas-id (ws-connection-canvas-id conn)))

    (unless (and user-id action)
      (format t "[WS WARN] Physics control missing user-id or action~%")
      (return-from handle-physics-control-message nil))

    (format t "[WS PHYSICS] Processing control from ~A: ~A~@[ (value: ~A)~]~%"
            username action value)

    (handler-case
        (cond
          ;; Play: Resume physics simulation
          ((string= action "play")
           (resume-physics)
           (format t "[WS PHYSICS] Physics resumed by ~A~%" username)
           ;; Broadcast state change to all clients in canvas
           (broadcast-to-canvas-room canvas-id
             (to-json-string
              `((:type . "physics-state-change")
                (:action . "play")
                (:user-id . ,user-id)
                (:username . ,username)))
             nil))

          ;; Pause: Pause physics simulation
          ((string= action "pause")
           (pause-physics)
           (format t "[WS PHYSICS] Physics paused by ~A~%" username)
           ;; Broadcast state change to all clients in canvas
           (broadcast-to-canvas-room canvas-id
             (to-json-string
              `((:type . "physics-state-change")
                (:action . "pause")
                (:user-id . ,user-id)
                (:username . ,username)))
             nil))

          ;; Reset: Clear all physics objects and reload from canvas
          ((string= action "reset")
           (format t "[WS PHYSICS] Resetting physics by ~A~%" username)
           ;; Clear physics engine
           (clear-all-physics-objects)
           ;; Reload objects from canvas
           (load-canvas-objects-into-physics canvas-id)
           ;; Reset velocities to zero
           (reset-all-velocities)
           ;; Broadcast state change to all clients in canvas
           (broadcast-to-canvas-room canvas-id
             (to-json-string
              `((:type . "physics-state-change")
                (:action . "reset")
                (:user-id . ,user-id)
                (:username . ,username)))
             nil))

          ;; Set Gravity: Update global gravity value
          ((string= action "set-gravity")
           (when value
             (let ((gravity-value (if (numberp value) value (parse-float value))))
               (set-global-gravity gravity-value)
               (format t "[WS PHYSICS] Gravity set to ~A by ~A~%" gravity-value username)
               ;; Broadcast state change to all clients in canvas
               (broadcast-to-canvas-room canvas-id
                 (to-json-string
                  `((:type . "physics-state-change")
                    (:action . "set-gravity")
                    (:value . ,gravity-value)
                    (:user-id . ,user-id)
                    (:username . ,username)))
                 nil))))

          ;; Set Boundary: Update boundary rule (contain/wrap)
          ((string= action "set-boundary")
           (when value
             (format t "[WS PHYSICS] Boundary rule set to ~A by ~A~%" value username)
             ;; TODO: Implement boundary rule setting in physics engine
             ;; For now, just broadcast the change
             (broadcast-to-canvas-room canvas-id
               (to-json-string
                `((:type . "physics-state-change")
                  (:action . "set-boundary")
                  (:value . ,value)
                  (:user-id . ,user-id)
                  (:username . ,username)))
               nil)))

          ;; Unknown action
          (t
           (format t "[WS PHYSICS WARN] Unknown physics control action: ~A~%" action)))

      (error (e)
        (format t "[WS PHYSICS ERROR] Failed to process physics control: ~A~%" e)
        (send-ws-message conn-id
          (to-json-string
           `((:type . "error")
             (:message . ,(format nil "Physics control failed: ~A" e))))))))))
