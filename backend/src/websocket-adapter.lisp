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

(defun get-all-connections ()
  "Get all active WebSocket connections."
  (bt:with-lock-held (*ws-connections-lock*)
    (alexandria:hash-table-values *ws-connections*)))

;;; Room Management

(defun add-connection-to-room (conn canvas-id)
  "Add a connection to a canvas room."
  (bt:with-lock-held (*canvas-rooms-lock*)
    (let ((room (gethash canvas-id *canvas-rooms*)))
      (unless room
        (setf room '()))
      (pushnew (ws-connection-id conn) room :test #'string=)
      (setf (gethash canvas-id *canvas-rooms*) room)))

  (format t "[WS] Added connection ~A to canvas ~A~%"
          (ws-connection-id conn) canvas-id))

(defun remove-connection-from-room (conn-id canvas-id)
  "Remove a connection from a canvas room."
  (bt:with-lock-held (*canvas-rooms-lock*)
    (let ((room (gethash canvas-id *canvas-rooms*)))
      (when room
        (setf (gethash canvas-id *canvas-rooms*)
              (remove conn-id room :test #'string=)))))

  (format t "[WS] Removed connection ~A from canvas ~A~%"
          conn-id canvas-id))

(defun get-room-connections (canvas-id)
  "Get all connection IDs in a canvas room."
  (bt:with-lock-held (*canvas-rooms-lock*)
    (gethash canvas-id *canvas-rooms*)))

(defun get-room-users (canvas-id)
  "Get list of users in a canvas room with their metadata."
  (let ((conn-ids (get-room-connections canvas-id)))
    (loop for conn-id in conn-ids
          for conn = (get-ws-connection conn-id)
          when (and conn (ws-connection-user-id conn))
          collect `((:user-id . ,(ws-connection-user-id conn))
                    (:username . ,(ws-connection-username conn))
                    (:color . ,(generate-user-color (ws-connection-user-id conn)))))))

;;; Message Handling

(defun handle-ws-message (conn-id message)
  "Route incoming WebSocket messages to appropriate handlers."
  (handler-case
      (let* ((parsed (jonathan:parse message :as :alist))
             ;; Convert string keys to keyword keys (same as HTTP body parsing)
             (data (mapcar (lambda (pair)
                            (cons (intern (string-upcase (string (car pair))) :keyword)
                                  (cdr pair)))
                          parsed))
             (type (cdr (assoc :type data))))

        (format t "[WS DEBUG] Raw message: ~A~%" message)
        (format t "[WS DEBUG] Parsed alist: ~A~%" parsed)
        (format t "[WS DEBUG] Converted data: ~A~%" data)
        (format t "[WS] Message received - type: ~A, conn: ~A~%"
                type conn-id)

        (unless type
          (format t "[WS WARN] Message missing 'type' field~%")
          (return-from handle-ws-message nil))

        ;; Dispatch based on message type
        (cond
          ((string= type "auth")
           (handle-auth-message conn-id data))

          ((string= type "cursor")
           (handle-cursor-message conn-id data))

          ((string= type "object-create")
           (handle-object-create-message conn-id data))

          ((string= type "object-update")
           (handle-object-update-message conn-id data))

          ((string= type "object-delete")
           (handle-object-delete-message conn-id data))

          ((string= type "ai-command")
           (handle-ai-command-message conn-id data))

          ((string= type "physics-spawn-ball")
           (handle-physics-spawn-ball-message conn-id data))

          ((string= type "physics-spawn-block")
           (handle-physics-spawn-block-message conn-id data))

          (t
           (format t "[WS WARN] Unknown message type: ~A~%" type))))

    (error (e)
      (format t "[WS ERROR] Failed to handle message: ~A~%" e))))

;;; Broadcasting

(defun broadcast-to-canvas-room (canvas-id message &optional exclude-conn-id)
  "Broadcast a message to all connections in a canvas room.

   Arguments:
     canvas-id - Canvas room identifier
     message - JSON string to broadcast
     exclude-conn-id - Optional connection ID to exclude from broadcast"

  (let ((conn-ids (get-room-connections canvas-id)))
    (dolist (conn-id conn-ids)
      (unless (and exclude-conn-id (string= conn-id exclude-conn-id))
        (send-ws-message conn-id message)))))

(defun send-ws-message (conn-id message)
  "Send a message to a specific WebSocket connection."
  (let ((conn (get-ws-connection conn-id)))
    (when conn
      (handler-case
          (progn
            (format t "[WS SEND] Sending to ~A: ~A~%" conn-id message)
            (websocket-driver:send (ws-connection-websocket conn) message)
            t)
        (error (e)
          (format t "[WS ERROR] Failed to send message to ~A: ~A~%"
                  conn-id e)
          nil)))))

;;; Utility Functions
;;; Note: to-json-string is defined in utils.lisp using hash tables for reliable
;;; JSON serialization with lowercase keys. It's used by both HTTP and WebSocket responses.

(defun generate-user-color (user-id)
  "Generate a consistent color for a user based on their ID."
  (let* ((hue (mod (* user-id 137) 360))
         (saturation 70)
         (lightness 50))
    (format nil "hsl(~A, ~A%, ~A%)" hue saturation lightness)))

;;; Message Handlers

(defun handle-auth-message (conn-id data)
  "Handle WebSocket authentication message."
  (format t "[WS AUTH DEBUG] Received data keys: ~A~%" (mapcar #'car data))
  (let* ((conn (get-ws-connection conn-id))
         (session-id (or (cdr (assoc :session-id data))
                         (cdr (assoc :sessionid data))
                         (cdr (assoc :SESSIONID data))))  ; Try uppercase (from JSON parser)
         (canvas-id (ws-connection-canvas-id conn)))
    (format t "[WS AUTH DEBUG] Extracted session-id: ~A~%" session-id)

    (if-let ((session (validate-session session-id)))
      (let ((user-id (cdr (assoc :user-id session)))
            (username (cdr (assoc :username session))))

        ;; Update connection with auth info
        (setf (ws-connection-user-id conn) user-id)
        (setf (ws-connection-username conn) username)
        (setf (ws-connection-session-id conn) session-id)

        ;; Initialize canvas physics if not already running
        (unless (canvas-physics-active-p canvas-id)
          (format t "[WS AUTH] Initializing physics for canvas ~A~%" canvas-id)
          (init-canvas-physics canvas-id)
          (start-physics-loop canvas-id))

        ;; Send auth success with canvas state
        (let ((canvas-objects (get-canvas-objects canvas-id)))
          (format t "[WS AUTH] Loading canvas state for ~A: ~A objects~%"
                  canvas-id (length canvas-objects))
          (format t "[WS AUTH] Canvas objects: ~A~%" canvas-objects)
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
         (canvas-id (ws-connection-canvas-id conn))
         (user-id (ws-connection-user-id conn))
         (username (ws-connection-username conn)))

    (when (and user-id x y)
      ;; Broadcast cursor position to room
      (broadcast-to-canvas-room canvas-id
        (to-json-string
         `((:type . "cursor")
           (:x . ,x)
           (:y . ,y)
           (:user-id . ,user-id)
           (:username . ,username)))
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
          ;; Merge updates into current object
          (dolist (update updates)
            (let ((key (car update))
                  (value (cdr update)))
              (setf (cdr (assoc key current-object)) value)))

          ;; Save updated object
          (update-canvas-object canvas-id object-id current-object user-id)

          ;; Broadcast update to room
          (broadcast-to-canvas-room canvas-id
            (to-json-string
             `((:type . "object-update")
               (:object-id . ,object-id)
               (:updates . ,updates)
               (:user-id . ,user-id)
               (:username . ,username)))
            conn-id)

          (format t "[WS] Object updated: ~A by ~A~%" object-id username))))))

(defun handle-object-delete-message (conn-id data)
  "Handle object deletion."
  (let* ((conn (get-ws-connection conn-id))
         (object-id (cdr (assoc :object-id data)))
         (user-id (ws-connection-user-id conn))
         (username (ws-connection-username conn))
         (canvas-id (ws-connection-canvas-id conn)))

    (when (and user-id object-id)
      ;; Delete from canvas state
      (delete-canvas-object canvas-id object-id)

      ;; Broadcast deletion to room
      (broadcast-to-canvas-room canvas-id
        (to-json-string
         `((:type . "object-delete")
           (:object-id . ,object-id)
           (:user-id . ,user-id)
           (:username . ,username)))
        conn-id)

      (format t "[WS] Object deleted: ~A by ~A~%" object-id username))))

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
     :name (format nil "ai-command-~A" (get-universal-time)))))

;;; Physics Message Handlers

(defun handle-physics-spawn-ball-message (conn-id data)
  "Handle physics-spawn-ball message from client.
   Creates a physics ball entity and broadcasts to all clients."
  (let* ((conn (get-ws-connection conn-id))
         (canvas-id (ws-connection-canvas-id conn))
         (user-id (ws-connection-user-id conn))
         (username (ws-connection-username conn))
         (x (cdr (assoc :x data)))
         (y (cdr (assoc :y data)))
         (vx (cdr (assoc :vx data)))
         (vy (cdr (assoc :vy data)))
         (ghost-id (cdr (assoc :ghost-id data))))  ; Client-side prediction ID

    (unless (and user-id x y)
      (format t "[PHYS WS WARN] Spawn ball missing required fields~%")
      (return-from handle-physics-spawn-ball-message nil))

    ;; Spawn ball entity in physics ECS
    (let ((entity-id (spawn-physics-ball canvas-id
                                         (coerce x 'single-float)
                                         (coerce y 'single-float)
                                         (coerce (or vx 0.0) 'single-float)
                                         (coerce (or vy 0.0) 'single-float))))

      (when entity-id
        ;; Broadcast ball spawned to all clients
        (broadcast-to-canvas-room canvas-id
          (to-json-string
           `((:type . "physics-ball-spawned")
             (:entity-id . ,entity-id)
             (:x . ,x)
             (:y . ,y)
             (:vx . ,(or vx 0.0))
             (:vy . ,(or vy 0.0))
             (:radius . 10.0)  ; Default radius
             (:ghost-id . ,ghost-id)  ; For client-side prediction reconciliation
             (:user-id . ,user-id)
             (:username . ,username)))
          nil)  ; Send to everyone

        (format t "[PHYS WS] Ball entity ~A spawned at (~,1F, ~,1F) by ~A~%"
                entity-id x y username)))))

(defun handle-physics-spawn-block-message (conn-id data)
  "Handle physics-spawn-block message from client.
   Creates a static block entity and broadcasts to all clients."
  (let* ((conn (get-ws-connection conn-id))
         (canvas-id (ws-connection-canvas-id conn))
         (user-id (ws-connection-user-id conn))
         (username (ws-connection-username conn))
         (x (cdr (assoc :x data)))
         (y (cdr (assoc :y data)))
         (width (cdr (assoc :width data)))
         (height (cdr (assoc :height data))))

    (unless (and user-id x y width height)
      (format t "[PHYS WS WARN] Spawn block missing required fields~%")
      (return-from handle-physics-spawn-block-message nil))

    ;; Spawn block entity in physics ECS
    (let ((entity-id (spawn-physics-block canvas-id
                                          (coerce x 'single-float)
                                          (coerce y 'single-float)
                                          (coerce width 'single-float)
                                          (coerce height 'single-float))))

      (when entity-id
        ;; Broadcast block spawned to all clients
        (broadcast-to-canvas-room canvas-id
          (to-json-string
           `((:type . "physics-block-spawned")
             (:entity-id . ,entity-id)
             (:x . ,x)
             (:y . ,y)
             (:width . ,width)
             (:height . ,height)
             (:user-id . ,user-id)
             (:username . ,username)))
          nil)  ; Send to everyone

        (format t "[PHYS WS] Block entity ~A spawned at (~,1F, ~,1F) size=(~,1Fx~,1F) by ~A~%"
                entity-id x y width height username)))))
