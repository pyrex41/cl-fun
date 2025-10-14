(defun handle-object-update (resource websocket data room)
  "Handle object update"
  (let ((client (gethash websocket (room-clients room))))
    (when client
      (let* ((object-id (cdr (assoc :object-id data)))
             (updates (cdr (assoc :updates data)))
             (canvas-id (resource-canvas-id resource)))

        (format t "~%=== OBJECT UPDATE RECEIVED ===~%")
        (format t "Canvas ID: ~A~%" canvas-id)
        (format t "Object ID: ~A (type: ~A)~%" object-id (type-of object-id))
        (format t "Updates: ~A~%" (to-json-string updates))

        ;; Get current object and apply updates
        (let ((current-object (get-canvas-object canvas-id object-id)))
          (format t "Current object found: ~A~%" (if current-object "YES" "NO"))
          (when current-object
            (format t "Current object data: ~A~%" (to-json-string current-object))
            ;; Merge updates into current object
            (let ((updated-object (append updates current-object)))
              (format t "Updated object data: ~A~%" (to-json-string updated-object))
              ;; Update canvas state
              (update-canvas-object canvas-id object-id updated-object
                                  (client-user-id client))
              (format t "Object updated in canvas state~%")

              ;; Broadcast to all other clients
              (broadcast-to-room room
                               `((:type . "object-update")
                                 (:object-id . ,object-id)
                                 (:updates . ,updates)
                                 (:user-id . ,(client-user-id client))
                                 (:username . ,(client-username client)))
                               websocket)
              (format t "Broadcast to room complete~%")))
          (unless current-object
            (format t "WARNING: Object ~A not found in canvas ~A~%" object-id canvas-id))
          (format t "=== END OBJECT UPDATE ===~%~%"))))))
