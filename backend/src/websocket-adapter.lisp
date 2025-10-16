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

          ;; Unknown message type
          (t
           (format t "[WS WARN] Unknown message type: ~A~%" msg-type))))

    (error (e)
      (format t "[WS ERROR] Failed to process message: ~A~%" e))))

(defun handle-ws-disconnect (conn-id)
  "Handle WebSocket disconnection.
   TODO: Implement in Task 6 (Port WebSocket Logic)"
  (format t "[WS] Connection closed: ~A~%" conn-id)
  (unregister-ws-connection conn-id))

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
             (:username . ,username)))
          conn-id)

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
        ;; Broadcast deletion
        (broadcast-to-canvas-room canvas-id
          (to-json-string
           `((:type . "object-delete")
             (:object-id . ,object-id)
             (:user-id . ,user-id)
             (:username . ,username)))
          conn-id)

        (format t "[WS] Object deleted: ~A by ~A~%" object-id username)))))
