;;;; database.lisp - Database operations for CollabCanvas

(in-package #:collabcanvas)

;;; Database connection management
(defparameter *database-connection* nil
  "Global database connection")

(defun ensure-database-directory ()
  "Ensure the database directory exists"
  (let ((db-dir (directory-namestring (merge-pathnames *database-path*))))
    (ensure-directories-exist db-dir)))

(defun connect-database ()
  "Connect to the SQLite database"
  (ensure-database-directory)
  (setf *database-connection*
        (sqlite:connect (merge-pathnames *database-path*))))

(defun disconnect-database ()
  "Disconnect from the database"
  (when *database-connection*
    (sqlite:disconnect *database-connection*)
    (setf *database-connection* nil)))

(defmacro with-database (&body body)
  "Execute body with database connection"
  `(bt:with-lock-held (*database-lock*)
     (unless *database-connection*
       (connect-database))
     ,@body))

;;; Database initialization
(defun init-database ()
  "Initialize the database with schema"
  (ensure-database-directory)
  (with-database
    (let ((schema-file (merge-pathnames "db/schema.sql"
                                        (asdf:system-source-directory :collabcanvas))))
      (when (probe-file schema-file)
        (let ((schema (alexandria:read-file-into-string schema-file)))
          ;; Split schema into individual statements
          (dolist (statement (cl-ppcre:split ";\\s*" schema))
            (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) statement)))
              (when (and trimmed (> (length trimmed) 0))
                (handler-case
                    (sqlite:execute-non-query *database-connection*
                                             (concatenate 'string trimmed ";"))
                  (sqlite:sqlite-error (e)
                    (format t "Warning: ~A~%" e))))))))))
  (format t "Database initialized successfully~%"))

;;; Query execution utilities
(defun execute-query (query &rest params)
  "Execute a SELECT query and return results"
  (with-database
    (apply #'sqlite:execute-to-list *database-connection* query params)))

(defun execute-non-query (query &rest params)
  "Execute a non-SELECT query (INSERT, UPDATE, DELETE)"
  (with-database
    (apply #'sqlite:execute-non-query *database-connection* query params)
    (sqlite:last-insert-rowid *database-connection*)))

(defun execute-single (query &rest params)
  "Execute a query and return single row"
  (with-database
    (apply #'sqlite:execute-single *database-connection* query params)))

;;; User operations
(defun create-user (email username password-hash)
  "Create a new user"
  (execute-non-query
   "INSERT INTO users (email, username, password_hash) VALUES (?, ?, ?)"
   email username password-hash))

(defun get-user-by-email (email)
  "Get user by email"
  (let ((row (execute-single
              "SELECT id, email, username, password_hash, created_at
               FROM users WHERE email = ?"
              email)))
    (when row
      `((:id . ,(first row))
        (:email . ,(second row))
        (:username . ,(third row))
        (:password-hash . ,(fourth row))
        (:created-at . ,(fifth row))))))

(defun get-user-by-username (username)
  "Get user by username"
  (let ((row (execute-single
              "SELECT id, email, username, password_hash, created_at
               FROM users WHERE username = ?"
              username)))
    (when row
      `((:id . ,(first row))
        (:email . ,(second row))
        (:username . ,(third row))
        (:password-hash . ,(fourth row))
        (:created-at . ,(fifth row))))))

(defun get-user-by-id (user-id)
  "Get user by ID"
  (let ((row (execute-single
              "SELECT id, email, username, created_at
               FROM users WHERE id = ?"
              user-id)))
    (when row
      `((:id . ,(first row))
        (:email . ,(second row))
        (:username . ,(third row))
        (:created-at . ,(fourth row))))))

;;; Session operations
(defun create-session (user-id session-id expires-at)
  "Create a new session"
  (execute-non-query
   "INSERT INTO sessions (user_id, session_id, expires_at) VALUES (?, ?, ?)"
   user-id session-id expires-at))

(defun get-session (session-id)
  "Get session by ID"
  (let ((row (execute-single
              "SELECT s.id, s.user_id, s.session_id, s.expires_at,
                      u.email, u.username
               FROM sessions s
               JOIN users u ON s.user_id = u.id
               WHERE s.session_id = ?"
              session-id)))
    (when row
      `((:id . ,(first row))
        (:user-id . ,(second row))
        (:session-id . ,(third row))
        (:expires-at . ,(fourth row))
        (:email . ,(fifth row))
        (:username . ,(sixth row))))))

(defun delete-session (session-id)
  "Delete a session"
  (execute-non-query
   "DELETE FROM sessions WHERE session_id = ?"
   session-id))

(defun cleanup-expired-sessions ()
  "Delete expired sessions"
  (execute-non-query
   "DELETE FROM sessions WHERE datetime(expires_at) < datetime('now')"))

;;; Canvas state operations
(defun save-canvas-state (canvas-id state-json)
  "Save or update canvas state"
  (let ((existing (execute-single
                   "SELECT id FROM canvas_states WHERE canvas_id = ?"
                   canvas-id)))
    (if existing
        (execute-non-query
         "UPDATE canvas_states
          SET state_json = ?, version = version + 1, updated_at = datetime('now')
          WHERE canvas_id = ?"
         state-json canvas-id)
        (execute-non-query
         "INSERT INTO canvas_states (canvas_id, state_json) VALUES (?, ?)"
         canvas-id state-json))))

(defun load-canvas-state (canvas-id)
  "Load canvas state"
  (let ((row (execute-single
              "SELECT state_json, version, updated_at
               FROM canvas_states WHERE canvas_id = ?"
              canvas-id)))
    (when row
      `((:state . ,(first row))
        (:version . ,(second row))
        (:updated-at . ,(third row))))))

;;; Canvas history operations
(defun add-canvas-history (canvas-id user-id action-type object-data)
  "Add an entry to canvas history"
  (execute-non-query
   "INSERT INTO canvas_history (canvas_id, user_id, action_type, object_data)
    VALUES (?, ?, ?, ?)"
   canvas-id user-id action-type object-data))

(defun get-canvas-history (canvas-id &key (limit 100))
  "Get canvas history"
  (let ((rows (execute-query
               "SELECT user_id, action_type, object_data, timestamp
                FROM canvas_history
                WHERE canvas_id = ?
                ORDER BY timestamp DESC
                LIMIT ?"
               canvas-id limit)))
    (mapcar (lambda (row)
              `((:user-id . ,(first row))
                (:action-type . ,(second row))
                (:object-data . ,(third row))
                (:timestamp . ,(fourth row))))
            rows)))

;;; Collaborator operations
(defun add-collaborator (canvas-id user-id &optional (role "editor"))
  "Add a collaborator to a canvas"
  (execute-non-query
   "INSERT OR REPLACE INTO collaborators (canvas_id, user_id, role)
    VALUES (?, ?, ?)"
   canvas-id user-id role))

(defun get-canvas-collaborators (canvas-id)
  "Get all collaborators for a canvas"
  (let ((rows (execute-query
               "SELECT c.user_id, c.role, u.username, u.email
                FROM collaborators c
                JOIN users u ON c.user_id = u.id
                WHERE c.canvas_id = ?"
               canvas-id)))
    (mapcar (lambda (row)
              `((:user-id . ,(first row))
                (:role . ,(second row))
                (:username . ,(third row))
                (:email . ,(fourth row))))
            rows)))