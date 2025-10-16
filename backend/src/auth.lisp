;;;; auth.lisp - Authentication logic for CollabCanvas

(in-package #:collabcanvas)

;;; Password hashing
(defun hash-password (password)
  "Hash a password using SHA-256"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array password))))

(defun verify-password (password hash)
  "Verify a password against its hash"
  (string= (hash-password password) hash))

;;; User registration
(defun register-user (email username password)
  "Register a new user"
  ;; Validate input
  (unless (valid-email-p email)
    (error "Invalid email format"))
  (unless (valid-username-p username)
    (error "Invalid username format"))
  (unless (valid-password-p password)
    (error "Password must be at least 8 characters"))

  ;; Check if user already exists
  (when (get-user-by-email email)
    (error "Email already registered"))
  (when (get-user-by-username username)
    (error "Username already taken"))

  ;; Create user
  (let ((password-hash (hash-password password)))
    (handler-case
        (let ((user-id (create-user email username password-hash)))
          `((:success . t)
            (:user-id . ,user-id)
            (:username . ,username)
            (:email . ,email)))
      (error (e)
        (error (format nil "Registration failed: ~A" e))))))

;;; User login
(defun login-user (email-or-username password)
  "Login a user with email or username"
  (let ((user (or (get-user-by-email email-or-username)
                  (get-user-by-username email-or-username))))
    (unless user
      (error "Invalid credentials"))

    (unless (verify-password password (cdr (assoc :password-hash user)))
      (error "Invalid credentials"))

    ;; Create session
    (let* ((session-id (generate-session-id))
           (expires-at (local-time:format-timestring
                       nil
                       (local-time:timestamp+ (local-time:now)
                                              *session-timeout*
                                              :sec))))
      (create-session (cdr (assoc :id user)) session-id expires-at)
      `((:success . t)
        (:session-id . ,session-id)
        (:user-id . ,(cdr (assoc :id user)))
        (:username . ,(cdr (assoc :username user)))
        (:email . ,(cdr (assoc :email user)))
        (:expires-at . ,expires-at)))))

;;; Session validation
(defun validate-session (session-id)
  "Validate a session and return user info"
  (format t "[AUTH] validate-session called with session-id: ~A~%" session-id)
  (when session-id
    (cleanup-expired-sessions) ; Clean up expired sessions periodically
    (let ((session (get-session session-id)))
      (format t "[AUTH] get-session returned: ~A~%" session)
      (when (and session
                 (not (expired-p (cdr (assoc :expires-at session)))))
        (format t "[AUTH] Session is valid and not expired~%")
        `((:valid . t)
          (:user-id . ,(cdr (assoc :user-id session)))
          (:username . ,(cdr (assoc :username session)))
          (:email . ,(cdr (assoc :email session))))))))

;;; Logout
(defun logout-user (session-id)
  "Logout a user by deleting their session"
  (delete-session session-id)
  '((:success . t)))

;;; Note: HTTP handlers have been moved to app.lisp for Clack compatibility
;;; This file now contains only core authentication logic