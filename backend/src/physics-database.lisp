;;;; physics-database.lisp - Database operations for physics engine

(in-package #:collabcanvas)

;;; Physics schema initialization

(defun init-physics-schema ()
  "Initialize physics tables by executing physics-schema.sql"
  (ensure-database-directory)
  ;; Create a temporary connection for schema initialization
  (let ((conn (create-database-connection)))
    (unwind-protect
         (let ((schema-file (merge-pathnames "db/physics-schema.sql"
                                             (asdf:system-source-directory :collabcanvas))))
           (when (probe-file schema-file)
             (let ((schema (alexandria:read-file-into-string schema-file)))
               ;; Split schema into individual statements
               (dolist (statement (cl-ppcre:split ";\\s*" schema))
                 (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) statement)))
                   (when (and trimmed (> (length trimmed) 0))
                     (handler-case
                         (sqlite:execute-non-query conn
                                                  (concatenate 'string trimmed ";"))
                       (sqlite:sqlite-error (e)
                         (format t "Warning: ~A~%" e))))))))
           (format t "Physics schema initialized successfully~%"))
      ;; Always disconnect the temporary connection
      (sqlite:disconnect conn))))

;;; Physics canvas settings operations

(defun save-physics-settings (canvas-id &key (gravity-x 0.0) (gravity-y 9.8)
                                             (simulation-rate 60) (max-objects 2000))
  "Save or update physics settings for a canvas"
  (with-db-connection (conn)
    (sqlite:execute-non-query conn "BEGIN TRANSACTION")
    (handler-case
        (progn
          ;; Use INSERT OR REPLACE for efficiency
          (sqlite:execute-non-query conn
                                   "INSERT OR REPLACE INTO physics_canvas_settings
                                    (canvas_id, gravity_x, gravity_y, simulation_rate, max_objects,
                                     created_at, updated_at)
                                    VALUES (?, ?, ?, ?, ?,
                                            COALESCE((SELECT created_at FROM physics_canvas_settings WHERE canvas_id = ?), datetime('now')),
                                            datetime('now'))"
                                   canvas-id gravity-x gravity-y simulation-rate max-objects canvas-id)
          (sqlite:execute-non-query conn "COMMIT")
          (format t "Saved physics settings for canvas ~A~%" canvas-id)
          t)
      (error (e)
        (sqlite:execute-non-query conn "ROLLBACK")
        (error "Failed to save physics settings: ~A" e)))))

(defun load-physics-settings (canvas-id)
  "Load physics settings for a canvas, returns default values if not found"
  (let ((row (execute-single
              "SELECT gravity_x, gravity_y, simulation_rate, max_objects, updated_at
               FROM physics_canvas_settings WHERE canvas_id = ?"
              canvas-id)))
    (if row
        `((:gravity-x . ,(first row))
          (:gravity-y . ,(second row))
          (:simulation-rate . ,(third row))
          (:max-objects . ,(fourth row))
          (:updated-at . ,(fifth row)))
        ;; Return default values if no settings exist
        `((:gravity-x . 0.0)
          (:gravity-y . 9.8)
          (:simulation-rate . 60)
          (:max-objects . 2000)
          (:updated-at . nil)))))

(defun delete-physics-settings (canvas-id)
  "Delete physics settings for a canvas"
  (execute-non-query
   "DELETE FROM physics_canvas_settings WHERE canvas_id = ?"
   canvas-id))

;;; Physics component operations

(defun save-physics-component (component-id canvas-id component-type properties created-by)
  "Save a physics component with its properties as JSON
   component-type: 'ball', 'fan', 'block', 'emitter', 'magnet'
   properties: alist or plist that will be serialized to JSON"
  (let ((properties-json (to-json-string properties)))
    (execute-non-query
     "INSERT OR REPLACE INTO physics_components
      (id, canvas_id, component_type, properties_json, created_by, created_at)
      VALUES (?, ?, ?, ?, ?,
              COALESCE((SELECT created_at FROM physics_components WHERE id = ?), datetime('now')))"
     component-id canvas-id component-type properties-json created-by component-id)))

(defun load-physics-component (component-id)
  "Load a single physics component by ID"
  (let ((row (execute-single
              "SELECT id, canvas_id, component_type, properties_json, created_by, created_at
               FROM physics_components WHERE id = ?"
              component-id)))
    (when row
      `((:id . ,(first row))
        (:canvas-id . ,(second row))
        (:component-type . ,(third row))
        (:properties . ,(parse-json (fourth row)))
        (:created-by . ,(fifth row))
        (:created-at . ,(sixth row))))))

(defun load-physics-components (canvas-id &key component-type)
  "Load all physics components for a canvas, optionally filtered by type"
  (let* ((query (if component-type
                    "SELECT id, canvas_id, component_type, properties_json, created_by, created_at
                     FROM physics_components
                     WHERE canvas_id = ? AND component_type = ?
                     ORDER BY created_at ASC"
                    "SELECT id, canvas_id, component_type, properties_json, created_by, created_at
                     FROM physics_components
                     WHERE canvas_id = ?
                     ORDER BY created_at ASC"))
         (rows (if component-type
                   (execute-query query canvas-id component-type)
                   (execute-query query canvas-id))))
    (mapcar (lambda (row)
              `((:id . ,(first row))
                (:canvas-id . ,(second row))
                (:component-type . ,(third row))
                (:properties . ,(parse-json (fourth row)))
                (:created-by . ,(fifth row))
                (:created-at . ,(sixth row))))
            rows)))

(defun delete-physics-component (component-id)
  "Delete a physics component (cascade will remove associated physics_bodies)"
  (execute-non-query
   "DELETE FROM physics_components WHERE id = ?"
   component-id))

(defun delete-canvas-physics-components (canvas-id)
  "Delete all physics components for a canvas"
  (execute-non-query
   "DELETE FROM physics_components WHERE canvas_id = ?"
   canvas-id))

(defun count-physics-components (canvas-id)
  "Count physics components for a canvas"
  (let ((row (execute-single
              "SELECT COUNT(*) FROM physics_components WHERE canvas_id = ?"
              canvas-id)))
    (if row (first row) 0)))

;;; Physics body operations (runtime state persistence)

(defun save-physics-body (body-id component-id position-x position-y
                          &key (velocity-x 0.0) (velocity-y 0.0) (rotation 0.0) (is-sleeping 0))
  "Save or update physics body runtime state"
  (execute-non-query
   "INSERT OR REPLACE INTO physics_bodies
    (id, component_id, position_x, position_y, velocity_x, velocity_y, rotation, is_sleeping, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))"
   body-id component-id position-x position-y velocity-x velocity-y rotation is-sleeping))

(defun load-physics-body (body-id)
  "Load physics body runtime state"
  (let ((row (execute-single
              "SELECT id, component_id, position_x, position_y, velocity_x, velocity_y,
                      rotation, is_sleeping, updated_at
               FROM physics_bodies WHERE id = ?"
              body-id)))
    (when row
      `((:id . ,(first row))
        (:component-id . ,(second row))
        (:position-x . ,(third row))
        (:position-y . ,(fourth row))
        (:velocity-x . ,(fifth row))
        (:velocity-y . ,(sixth row))
        (:rotation . ,(seventh row))
        (:is-sleeping . ,(eighth row))
        (:updated-at . ,(ninth row))))))

(defun load-physics-bodies-by-component (component-id)
  "Load all physics bodies for a component"
  (let ((rows (execute-query
               "SELECT id, component_id, position_x, position_y, velocity_x, velocity_y,
                       rotation, is_sleeping, updated_at
                FROM physics_bodies WHERE component_id = ?"
               component-id)))
    (mapcar (lambda (row)
              `((:id . ,(first row))
                (:component-id . ,(second row))
                (:position-x . ,(third row))
                (:position-y . ,(fourth row))
                (:velocity-x . ,(fifth row))
                (:velocity-y . ,(sixth row))
                (:rotation . ,(seventh row))
                (:is-sleeping . ,(eighth row))
                (:updated-at . ,(ninth row))))
            rows)))

(defun delete-physics-body (body-id)
  "Delete a physics body"
  (execute-non-query
   "DELETE FROM physics_bodies WHERE id = ?"
   body-id))

(defun delete-component-physics-bodies (component-id)
  "Delete all physics bodies for a component"
  (execute-non-query
   "DELETE FROM physics_bodies WHERE component_id = ?"
   component-id))

(defun cleanup-stale-physics-bodies (&optional (hours-old 24))
  "Clean up physics bodies that haven't been updated recently"
  (execute-non-query
   "DELETE FROM physics_bodies
    WHERE datetime(updated_at) < datetime('now', ?)"
   (format nil "-~A hours" hours-old)))

;;; Batch operations for performance

(defun batch-save-physics-components (components)
  "Batch save multiple physics components in a transaction
   components: list of plists with :id :canvas-id :component-type :properties :created-by"
  (with-db-connection (conn)
    (sqlite:execute-non-query conn "BEGIN TRANSACTION")
    (handler-case
        (progn
          (dolist (component components)
            (let ((component-id (getf component :id))
                  (canvas-id (getf component :canvas-id))
                  (component-type (getf component :component-type))
                  (properties (getf component :properties))
                  (created-by (getf component :created-by)))
              (let ((properties-json (to-json-string properties)))
                (sqlite:execute-non-query conn
                                         "INSERT OR REPLACE INTO physics_components
                                          (id, canvas_id, component_type, properties_json, created_by, created_at)
                                          VALUES (?, ?, ?, ?, ?,
                                                  COALESCE((SELECT created_at FROM physics_components WHERE id = ?), datetime('now')))"
                                         component-id canvas-id component-type properties-json created-by component-id))))
          (sqlite:execute-non-query conn "COMMIT")
          (format t "Batch saved ~A physics components~%" (length components))
          t)
      (error (e)
        (sqlite:execute-non-query conn "ROLLBACK")
        (error "Failed to batch save physics components: ~A" e)))))

(defun batch-save-physics-bodies (bodies)
  "Batch save multiple physics bodies in a transaction
   bodies: list of plists with :id :component-id :position-x :position-y :velocity-x :velocity-y :rotation :is-sleeping"
  (with-db-connection (conn)
    (sqlite:execute-non-query conn "BEGIN TRANSACTION")
    (handler-case
        (progn
          (dolist (body bodies)
            (let ((body-id (getf body :id))
                  (component-id (getf body :component-id))
                  (position-x (getf body :position-x))
                  (position-y (getf body :position-y))
                  (velocity-x (getf body :velocity-x 0.0))
                  (velocity-y (getf body :velocity-y 0.0))
                  (rotation (getf body :rotation 0.0))
                  (is-sleeping (getf body :is-sleeping 0)))
              (sqlite:execute-non-query conn
                                       "INSERT OR REPLACE INTO physics_bodies
                                        (id, component_id, position_x, position_y, velocity_x, velocity_y,
                                         rotation, is_sleeping, updated_at)
                                        VALUES (?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))"
                                       body-id component-id position-x position-y
                                       velocity-x velocity-y rotation is-sleeping)))
          (sqlite:execute-non-query conn "COMMIT")
          (format t "Batch saved ~A physics bodies~%" (length bodies))
          t)
      (error (e)
        (sqlite:execute-non-query conn "ROLLBACK")
        (error "Failed to batch save physics bodies: ~A" e)))))

;;; Validation and statistics

(defun validate-component-type (component-type)
  "Validate that component-type is one of the allowed types"
  (let ((valid-types '("ball" "fan" "block" "emitter" "magnet")))
    (member component-type valid-types :test #'string=)))

(defun get-physics-stats (canvas-id)
  "Get statistics about physics components for a canvas"
  (let ((rows (execute-query
               "SELECT component_type, COUNT(*) as count
                FROM physics_components
                WHERE canvas_id = ?
                GROUP BY component_type"
               canvas-id)))
    (mapcar (lambda (row)
              `((:component-type . ,(first row))
                (:count . ,(second row))))
            rows)))

;;; Server lifecycle integration

(defun init-physics-db ()
  "Initialize physics database schema (called during server startup)"
  (format t "Initializing physics database...~%")
  (init-physics-schema)
  (format t "Physics database ready~%"))
