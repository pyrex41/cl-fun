;;;; websocket.lisp - WebSocket handlers for real-time collaboration

(in-package #:collabcanvas)

;;; Room management
(defparameter *canvas-rooms* (make-hash-table :test 'equal)
  "Hash table of canvas-id -> canvas-room")

(defparameter *rooms-lock* (bt:make-lock "rooms-lock")
  "Lock for room operations")

(defclass canvas-room ()
  ((id :initarg :id :reader room-id)
   (clients :initform (make-hash-table :test 'eq) :accessor room-clients)
   (lock :initform (bt:make-lock "room-lock") :reader room-lock))
  (:documentation "A canvas room containing connected clients"))

(defclass canvas-client ()
  ((websocket :initarg :websocket :reader client-websocket)
   (user-id :initarg :user-id :accessor client-user-id)
   (username :initarg :username :accessor client-username)
   (canvas-id :initarg :canvas-id :reader client-canvas-id)
   (color :initarg :color :accessor client-color))
  (:documentation "A connected WebSocket client"))

(defclass canvas-websocket-resource (hunchensocket:websocket-resource)
  ((canvas-id :initarg :canvas-id :reader resource-canvas-id))
  (:documentation "WebSocket resource for a specific canvas"))

;;; Room management functions
(defun get-or-create-room (canvas-id)
  "Get or create a room for a canvas"
  (bt:with-lock-held (*rooms-lock*)
    (or (gethash canvas-id *canvas-rooms*)
        (setf (gethash canvas-id *canvas-rooms*)
              (make-instance 'canvas-room :id canvas-id)))))

(defun add-client-to-room (room client)
  "Add a client to a room"
  (bt:with-lock-held ((room-lock room))
    (setf (gethash (client-websocket client) (room-clients room)) client)))

(defun remove-client-from-room (room websocket)
  "Remove a client from a room"
  (bt:with-lock-held ((room-lock room))
    (remhash websocket (room-clients room))))

(defun get-room-client-list (room)
  "Get list of clients in a room"
  (bt:with-lock-held ((room-lock room))
    (loop for client being the hash-values of (room-clients room)
          collect `((:user-id . ,(client-user-id client))
                   (:username . ,(client-username client))
                   (:color . ,(client-color client))))))

(defun broadcast-to-room (room message &optional exclude-websocket)
  "Broadcast a message to all clients in a room except the sender"
  (let ((json-message (to-json-string message)))
    (bt:with-lock-held ((room-lock room))
      (loop for client being the hash-values of (room-clients room)
            unless (eq (client-websocket client) exclude-websocket)
            do (handler-case
                   (hunchensocket:send-text-message
                    (client-websocket client) json-message)
                 (error (e)
                   (format t "Error sending to client: ~A~%" e)))))))

(defun broadcast-to-all (room message)
  "Broadcast a message to all clients in a room"
  (broadcast-to-room room message nil))

;;; Color generation for users
(defparameter *user-colors*
  '("#FF6B6B" "#4ECDC4" "#45B7D1" "#96CEB4" "#FECA57"
    "#FF9FF3" "#54A0FF" "#48DBFB" "#FD79A8" "#A29BFE")
  "Predefined colors for user cursors")

(defun get-user-color (user-id)
  "Get a consistent color for a user"
  (nth (mod user-id (length *user-colors*)) *user-colors*))

;;; WebSocket event handlers
(defmethod hunchensocket:client-connected ((resource canvas-websocket-resource) websocket)
  "Handle new WebSocket connection"
  (format t "Client connected to canvas: ~A~%" (resource-canvas-id resource))
  ;; Client will send auth message to complete connection
  nil)

(defmethod hunchensocket:client-disconnected ((resource canvas-websocket-resource) websocket)
  "Handle WebSocket disconnection"
  (let* ((canvas-id (resource-canvas-id resource))
         (room (gethash canvas-id *canvas-rooms*)))
    (when room
      (let ((client (gethash websocket (room-clients room))))
        (when client
          (format t "Client ~A disconnected from canvas ~A~%"
                  (client-username client) canvas-id)
          ;; Notify others of disconnection
          (broadcast-to-room room
                           `((:type . "user-disconnected")
                             (:user-id . ,(client-user-id client))
                             (:username . ,(client-username client)))
                           websocket)
          ;; Remove from room
          (remove-client-from-room room websocket))))))

(defmethod hunchensocket:text-message-received ((resource canvas-websocket-resource) websocket message)
  "Handle incoming WebSocket message"
  (handler-case
      (let* ((data (parse-json message))
             (msg-type (cdr (assoc :type data)))
             (canvas-id (resource-canvas-id resource))
             (room (get-or-create-room canvas-id)))

        (cond
          ;; Authentication message
          ((string= msg-type "auth")
           (handle-websocket-auth resource websocket data room))

          ;; Cursor movement
          ((string= msg-type "cursor")
           (handle-cursor-update resource websocket data room))

          ;; Object creation
          ((string= msg-type "object-create")
           (handle-object-create resource websocket data room))

          ;; Object update
          ((string= msg-type "object-update")
           (handle-object-update resource websocket data room))

          ;; Object deletion
          ((string= msg-type "object-delete")
           (handle-object-delete resource websocket data room))

          ;; Unknown message type
          (t
           (format t "Unknown message type: ~A~%" msg-type))))

    (error (e)
      (format t "Error processing WebSocket message: ~A~%" e)
      (hunchensocket:send-text-message
       websocket
       (to-json-string `((:type . "error")
                        (:message . ,(format nil "~A" e))))))))

;;; Message handlers
(defun handle-websocket-auth (resource websocket data room)
  "Handle authentication message"
  (let ((session-id (cdr (assoc :session-id data)))
        (canvas-id (resource-canvas-id resource)))

    (if-let ((session (validate-session session-id)))
      (let* ((user-id (cdr (assoc :user-id session)))
             (username (cdr (assoc :username session)))
             (color (get-user-color user-id))
             (client (make-instance 'canvas-client
                                  :websocket websocket
                                  :user-id user-id
                                  :username username
                                  :canvas-id canvas-id
                                  :color color)))

        ;; Add client to room
        (add-client-to-room room client)

        ;; Send success response with initial state
        (let ((canvas-state (get-canvas-objects canvas-id)))
          (hunchensocket:send-text-message
           websocket
           (to-json-string `((:type . "auth-success")
                           (:user-id . ,user-id)
                           (:username . ,username)
                           (:color . ,color)
                           (:canvas-state . ,canvas-state)))))

        ;; Notify others of new user
        (broadcast-to-room room
                         `((:type . "user-connected")
                           (:user-id . ,user-id)
                           (:username . ,username)
                           (:color . ,color))
                         websocket)

        ;; Send current presence list
        (hunchensocket:send-text-message
         websocket
         (to-json-string `((:type . "presence")
                          (:users . ,(get-room-client-list room)))))

        (format t "User ~A authenticated for canvas ~A~%" username canvas-id))

      ;; Authentication failed
      (hunchensocket:send-text-message
       websocket
       (to-json-string '((:type . "auth-failed")
                        (:message . "Invalid or expired session")))))))

(defun handle-cursor-update (resource websocket data room)
  "Handle cursor position update"
  (let ((client (gethash websocket (room-clients room))))
    (when client
      (let ((x (cdr (assoc :x data)))
            (y (cdr (assoc :y data))))
        ;; Broadcast cursor position to others
        (broadcast-to-room room
                         `((:type . "cursor")
                           (:user-id . ,(client-user-id client))
                           (:username . ,(client-username client))
                           (:color . ,(client-color client))
                           (:x . ,x)
                           (:y . ,y))
                         websocket)))))

(defun handle-object-create (resource websocket data room)
  "Handle object creation"
  (let ((client (gethash websocket (room-clients room))))
    (when client
      (let* ((object-data (cdr (assoc :object data)))
             (object-id (cdr (assoc :id object-data)))
             (canvas-id (resource-canvas-id resource)))

        ;; Update canvas state
        (update-canvas-object canvas-id object-id object-data
                            (client-user-id client))

        ;; Broadcast to all other clients
        (broadcast-to-room room
                         `((:type . "object-create")
                           (:object . ,object-data)
                           (:user-id . ,(client-user-id client))
                           (:username . ,(client-username client)))
                         websocket)))))

(defun handle-object-update (resource websocket data room)
  "Handle object update"
  (let ((client (gethash websocket (room-clients room))))
    (when client
      (let* ((object-id (cdr (assoc :object-id data)))
             (updates (cdr (assoc :updates data)))
             (canvas-id (resource-canvas-id resource)))

        ;; Get current object and apply updates
        (let ((current-object (get-canvas-object canvas-id object-id)))
          (when current-object
            ;; Merge updates into current object
            (let ((updated-object (append updates current-object)))
              ;; Update canvas state
              (update-canvas-object canvas-id object-id updated-object
                                  (client-user-id client))

              ;; Broadcast to all other clients
              (broadcast-to-room room
                               `((:type . "object-update")
                                 (:object-id . ,object-id)
                                 (:updates . ,updates)
                                 (:user-id . ,(client-user-id client))
                                 (:username . ,(client-username client)))
                               websocket))))))))

(defun handle-object-delete (resource websocket data room)
  "Handle object deletion"
  (let ((client (gethash websocket (room-clients room))))
    (when client
      (let* ((object-id (cdr (assoc :object-id data)))
             (canvas-id (resource-canvas-id resource)))

        ;; Delete from canvas state
        (when (delete-canvas-object canvas-id object-id
                                   (client-user-id client))
          ;; Broadcast to all other clients
          (broadcast-to-room room
                           `((:type . "object-delete")
                             (:object-id . ,object-id)
                             (:user-id . ,(client-user-id client))
                             (:username . ,(client-username client)))
                           websocket))))))