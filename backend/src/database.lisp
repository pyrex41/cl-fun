;;;; database.lisp - Database operations for CollabCanvas

(in-package #:collabcanvas)

;;; Database connection pool management
(defstruct db-pool
  "Connection pool for database connections"
  (available nil :type list)  ; List of available connections
  (in-use nil :type list)     ; List of connections currently in use
  (lock (bt:make-lock "db-pool-lock"))
  (condition-variable (bt:make-condition-variable))
  (max-size 10 :type integer)
  (current-size 0 :type integer))

(defparameter *database-pool* nil
  "Global database connection pool")

(defun ensure-database-directory ()
  "Ensure the database directory exists"
  (let ((db-dir (directory-namestring (merge-pathnames *database-path*))))
    (ensure-directories-exist db-dir)))

(defun create-database-connection ()
  "Create a new database connection"
  (ensure-database-directory)
  (sqlite:connect (merge-pathnames *database-path*)))

(defun init-database-pool (&optional (size *database-pool-size*))
  "Initialize the database connection pool with SIZE connections"
  (setf *database-pool* (make-db-pool :max-size size))
  (dotimes (i size)
    (let ((conn (create-database-connection)))
      (push conn (db-pool-available *database-pool*))
      (incf (db-pool-current-size *database-pool*))))
  (format t "Initialized database pool with ~A connections~%" size))

(defun acquire-connection (&optional (timeout *database-connection-timeout*))
  "Acquire a connection from the pool, waiting up to TIMEOUT seconds if none available"
  (bt:with-lock-held ((db-pool-lock *database-pool*))
    (let ((start-time (get-universal-time)))
      (loop
        (when (db-pool-available *database-pool*)
          (let ((conn (pop (db-pool-available *database-pool*))))
            (push conn (db-pool-in-use *database-pool*))
            (return-from acquire-connection conn)))

        ;; Check timeout
        (when (> (- (get-universal-time) start-time) timeout)
          (error "Timeout waiting for database connection from pool"))

        ;; If pool not at max size, create new connection
        (when (< (db-pool-current-size *database-pool*)
                 (db-pool-max-size *database-pool*))
          (let ((conn (create-database-connection)))
            (push conn (db-pool-in-use *database-pool*))
            (incf (db-pool-current-size *database-pool*))
            (return-from acquire-connection conn)))

        ;; Wait for a connection to be released
        (bt:condition-wait (db-pool-condition-variable *database-pool*)
                          (db-pool-lock *database-pool*)
                          :timeout 1)))))

(defun release-connection (conn)
  "Release a connection back to the pool"
  (bt:with-lock-held ((db-pool-lock *database-pool*))
    (setf (db-pool-in-use *database-pool*)
          (remove conn (db-pool-in-use *database-pool*)))
    (push conn (db-pool-available *database-pool*))
    (bt:condition-notify (db-pool-condition-variable *database-pool*))))

(defun close-database-pool ()
  "Close all connections in the pool"
  (when *database-pool*
    (bt:with-lock-held ((db-pool-lock *database-pool*))
      ;; Close all available connections
      (dolist (conn (db-pool-available *database-pool*))
        (sqlite:disconnect conn))
      ;; Close all in-use connections (shouldn't normally happen)
      (dolist (conn (db-pool-in-use *database-pool*))
        (sqlite:disconnect conn))
      (setf (db-pool-available *database-pool*) nil)
      (setf (db-pool-in-use *database-pool*) nil)
      (setf (db-pool-current-size *database-pool*) 0))
    (format t "Closed database connection pool~%")))

(defmacro with-db-connection ((conn-var) &body body)
  "Execute body with a connection from the pool, ensuring it's released afterward"
  `(let ((,conn-var (acquire-connection)))
     (unwind-protect
          (progn ,@body)
       (release-connection ,conn-var))))

;;; Legacy compatibility - keep with-database for backward compatibility
(defmacro with-database (&body body)
  "Execute body with database connection (legacy compatibility, uses pool)"
  (let ((conn (gensym "CONN")))
    `(with-db-connection (,conn)
       (let ((*database-connection* ,conn))
         ,@body))))

;;; For backward compatibility with code expecting *database-connection*
(defparameter *database-connection* nil
  "Current database connection (for backward compatibility)")

(defun connect-database ()
  "Connect to the SQLite database (legacy compatibility)"
  (ensure-database-directory)
  (setf *database-connection*
        (sqlite:connect (merge-pathnames *database-path*))))

(defun disconnect-database ()
  "Disconnect from the database (legacy compatibility)"
  (when *database-connection*
    (sqlite:disconnect *database-connection*)
    (setf *database-connection* nil)))

;;; Database initialization
(defun init-database ()
  "Initialize the database with schema"
  (ensure-database-directory)
  ;; Create a temporary connection for schema initialization
  ;; Don't use the pool here since it hasn't been initialized yet
  (let ((conn (create-database-connection)))
    (unwind-protect
         (let ((schema-file (merge-pathnames "db/schema.sql"
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
           (format t "Database initialized successfully~%"))
      ;; Always disconnect the temporary connection
      (sqlite:disconnect conn))))

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
    (let ((rows (apply #'sqlite:execute-to-list *database-connection* query params)))
      (first rows))))

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
  "Save or update canvas state using INSERT OR REPLACE with transaction"
  (with-db-connection (conn)
    ;; Use transaction for atomic operation
    (sqlite:execute-non-query conn "BEGIN TRANSACTION")
    (handler-case
        (progn
          ;; Get current version if exists
          (let* ((current-version
                  (let ((rows (sqlite:execute-to-list conn
                                                      "SELECT version FROM canvas_states WHERE canvas_id = ?"
                                                      canvas-id)))
                    (if rows (first (first rows)) 0)))
                 (new-version (1+ current-version)))
            ;; Use INSERT OR REPLACE for efficiency
            (sqlite:execute-non-query conn
                                     "INSERT OR REPLACE INTO canvas_states (canvas_id, state_json, version, created_at, updated_at)
                                      VALUES (?, ?, ?,
                                              COALESCE((SELECT created_at FROM canvas_states WHERE canvas_id = ?), datetime('now')),
                                              datetime('now'))"
                                     canvas-id state-json new-version canvas-id)
            (sqlite:execute-non-query conn "COMMIT")
            new-version))
      (error (e)
        (sqlite:execute-non-query conn "ROLLBACK")
        (error "Failed to save canvas state: ~A" e)))))

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