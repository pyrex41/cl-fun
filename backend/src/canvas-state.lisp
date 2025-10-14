;;;; canvas-state.lisp - Canvas state management for CollabCanvas

(in-package #:collabcanvas)

;;; Canvas state cache
(defparameter *canvas-states* (make-hash-table :test 'equal)
  "In-memory cache of canvas states")

(defparameter *canvas-locks* (make-hash-table :test 'equal)
  "Locks for each canvas to ensure thread-safe updates")

(defparameter *save-timers* (make-hash-table :test 'equal)
  "Debounce timers for saving canvas states")

;;; Canvas state structure
(defstruct canvas-state
  id
  objects
  version
  last-updated
  dirty-p)

(defun get-or-create-canvas-lock (canvas-id)
  "Get or create a lock for a canvas"
  (or (gethash canvas-id *canvas-locks*)
      (setf (gethash canvas-id *canvas-locks*)
            (bt:make-lock (format nil "canvas-~A" canvas-id)))))

(defun get-or-load-canvas-state (canvas-id)
  "Get canvas state from cache or load from database"
  (or (gethash canvas-id *canvas-states*)
      (let ((db-state (load-canvas-state canvas-id)))
        (if db-state
            (let* ((json-state (cdr (assoc :state db-state)))
                   (parsed-state (parse-json json-state))
                   (objects-hash (make-hash-table :test 'equal)))
              ;; Convert alist to hash table
              (when parsed-state
                (cond
                  ;; If it's an alist (list of pairs)
                  ((and (listp parsed-state) (consp (first parsed-state)))
                   (dolist (pair parsed-state)
                     (when (consp pair)
                       ;; Convert keyword keys to strings for consistency
                       (let ((key (car pair)))
                         (when (keywordp key)
                           (setq key (string-downcase (symbol-name key))))
                         (setf (gethash key objects-hash) (cdr pair))))))
                  ;; If it's a hash table already
                  ((hash-table-p parsed-state)
                   (maphash (lambda (k v)
                             (setf (gethash k objects-hash) v))
                            parsed-state))))
              (let ((state (make-canvas-state
                           :id canvas-id
                           :objects objects-hash
                           :version (cdr (assoc :version db-state))
                           :last-updated (cdr (assoc :updated-at db-state))
                           :dirty-p nil)))
                (setf (gethash canvas-id *canvas-states*) state)))
            ;; Create new canvas state
            (let ((state (make-canvas-state
                         :id canvas-id
                         :objects (make-hash-table :test 'equal)
                         :version 1
                         :last-updated (current-timestamp)
                         :dirty-p nil)))
              (setf (gethash canvas-id *canvas-states*) state))))))

(defun mark-canvas-dirty (canvas-id)
  "Mark canvas as dirty and schedule save"
  (let ((state (gethash canvas-id *canvas-states*)))
    (when state
      (setf (canvas-state-dirty-p state) t)
      (schedule-canvas-save canvas-id))))

(defun schedule-canvas-save (canvas-id)
  "Schedule a debounced save for canvas state"
  ;; Cancel existing timer if any
  (when-let ((timer (gethash canvas-id *save-timers*)))
    (bt:destroy-thread timer))

  ;; Create new timer
  (setf (gethash canvas-id *save-timers*)
        (bt:make-thread
         (lambda ()
           (sleep *state-save-debounce*)
           (persist-canvas-state canvas-id)
           (remhash canvas-id *save-timers*))
         :name (format nil "save-timer-~A" canvas-id))))

(defun persist-canvas-state (canvas-id)
  "Persist canvas state to database"
  (let ((state (gethash canvas-id *canvas-states*)))
    (when (and state (canvas-state-dirty-p state))
      (let ((lock (get-or-create-canvas-lock canvas-id)))
        (bt:with-lock-held (lock)
          (save-canvas-state canvas-id
                            (to-json-string (canvas-state-objects state)))
          (setf (canvas-state-dirty-p state) nil)
          (incf (canvas-state-version state))
          (setf (canvas-state-last-updated state) (current-timestamp)))))))

;;; Canvas operations
(defun update-canvas-object (canvas-id object-id object-data &optional user-id)
  "Update or create an object in the canvas"
  (let* ((lock (get-or-create-canvas-lock canvas-id))
         (state (get-or-load-canvas-state canvas-id)))
    (bt:with-lock-held (lock)
      (let ((objects (canvas-state-objects state)))
        ;; Determine if this is create or update
        (let ((action-type (if (gethash object-id objects) "update" "create")))
          ;; Update object
          (setf (gethash object-id objects) object-data)
          ;; Add to history if user-id provided
          (when user-id
            (add-canvas-history canvas-id user-id action-type
                               (to-json-string object-data)))
          ;; Mark as dirty
          (mark-canvas-dirty canvas-id)
          ;; Return the action type
          action-type)))))

(defun delete-canvas-object (canvas-id object-id &optional user-id)
  "Delete an object from the canvas"
  (let* ((lock (get-or-create-canvas-lock canvas-id))
         (state (get-or-load-canvas-state canvas-id)))
    (bt:with-lock-held (lock)
      (let ((objects (canvas-state-objects state)))
        (format t "DEBUG: Trying to delete object ~A~%" object-id)
        (format t "DEBUG: Object ID type: ~A~%" (type-of object-id))
        (format t "DEBUG: Current objects in hash: ~A~%"
                (hash-table-count objects))
        (format t "DEBUG: All keys in hash: ~{~A ~}~%"
                (loop for key being the hash-keys of objects collect key))
        (when-let ((object-data (gethash object-id objects)))
          ;; Remove object
          (remhash object-id objects)
          ;; Add to history if user-id provided
          (when user-id
            (add-canvas-history canvas-id user-id "delete"
                               (to-json-string object-data)))
          ;; Mark as dirty
          (mark-canvas-dirty canvas-id)
          t)))))

(defun get-canvas-objects (canvas-id)
  "Get all objects in a canvas as a list"
  (let ((state (get-or-load-canvas-state canvas-id)))
    ;; Convert hash table to list of objects for JSON serialization
    (let ((objects '()))
      (maphash (lambda (id data)
                 ;; Ensure the object data includes the id field
                 ;; If data is an alist, check if :id or :ID exists
                 (let ((has-id (or (assoc :id data) (assoc :ID data))))
                   (if has-id
                       ;; ID already in data, just push it
                       (push data objects)
                       ;; ID not in data, add it
                       (push (cons (cons :id id) data) objects))))
               (canvas-state-objects state))
      objects)))

(defun get-canvas-object (canvas-id object-id)
  "Get a specific object from canvas"
  (let ((state (get-or-load-canvas-state canvas-id)))
    (gethash object-id (canvas-state-objects state))))

;;; HTTP Handlers for canvas state
(defun handle-get-canvas-state ()
  "Handle GET request for canvas state"
  (set-cors-headers)
  (require-auth)

  (let ((canvas-id (hunchentoot:get-parameter "canvas_id")))
    (unless canvas-id
      (return-from handle-get-canvas-state
        (error-response "Canvas ID required")))

    (let ((state (get-or-load-canvas-state canvas-id)))
      (success-response
       `((:canvas-id . ,canvas-id)
         (:objects . ,(canvas-state-objects state))
         (:version . ,(canvas-state-version state))
         (:last-updated . ,(canvas-state-last-updated state)))))))

(defun handle-save-canvas-state ()
  "Handle POST request to save canvas state"
  (set-cors-headers)
  (require-auth)

  (let ((data (get-json-body)))
    (unless data
      (return-from handle-save-canvas-state
        (error-response "Invalid request body")))

    (let ((canvas-id (cdr (assoc :canvas-id data)))
          (objects (cdr (assoc :objects data))))

      (unless (and canvas-id objects)
        (return-from handle-save-canvas-state
          (error-response "Canvas ID and objects required")))

      ;; Update entire canvas state
      (let* ((lock (get-or-create-canvas-lock canvas-id))
             (state (get-or-load-canvas-state canvas-id)))
        (bt:with-lock-held (lock)
          (setf (canvas-state-objects state) objects)
          (mark-canvas-dirty canvas-id)))

      (success-response
       '((:message . "Canvas state saved successfully"))))))