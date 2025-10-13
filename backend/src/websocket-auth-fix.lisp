;;;; Temporary fix for handle-websocket-auth function

(in-package #:collabcanvas)

(defun handle-websocket-auth (resource websocket data room)
  "Handle authentication message - simplified version"
  (let ((session-id (cdr (assoc :session-id data)))
        (canvas-id (resource-canvas-id resource)))

    (when-let ((session (validate-session session-id)))
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

        ;; Send success response
        (hunchensocket:send-text-message
         websocket
         (to-json-string `((:type . "auth-success")
                           (:user-id . ,user-id)
                           (:username . ,username)
                           (:color . ,color))))

        ;; Notify others
        (broadcast-to-room room
                           `((:type . "user-connected")
                             (:user-id . ,user-id)
                             (:username . ,username)
                             (:color . ,color))
                           websocket)

        (format t "User ~A authenticated for canvas ~A~%" username canvas-id)))))
