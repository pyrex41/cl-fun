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
  (when session-id
    (cleanup-expired-sessions) ; Clean up expired sessions periodically
    (let ((session (get-session session-id)))
      (when (and session
                 (not (expired-p (cdr (assoc :expires-at session)))))
        `((:valid . t)
          (:user-id . ,(cdr (assoc :user-id session)))
          (:username . ,(cdr (assoc :username session)))
          (:email . ,(cdr (assoc :email session))))))))

;;; Logout
(defun logout-user (session-id)
  "Logout a user by deleting their session"
  (delete-session session-id)
  '((:success . t)))

;;; HTTP Handlers for authentication
(defun handle-register ()
  "Handle user registration endpoint"
  (set-cors-headers)
  (let ((data (get-json-body)))
    (unless data
      (return-from handle-register (error-response "Invalid request body")))

    (let ((email (cdr (assoc :email data)))
          (username (cdr (assoc :username data)))
          (password (cdr (assoc :password data))))

      (unless (and email username password)
        (return-from handle-register
          (error-response "Email, username, and password are required")))

      (handler-case
          (let ((result (register-user email username password)))
            (success-response result))
        (error (e)
          (error-response (format nil "~A" e)))))))

(defun handle-login ()
  "Handle user login endpoint"
  (set-cors-headers)
  (let ((data (get-json-body)))
    (unless data
      (return-from handle-login (error-response "Invalid request body")))

    (let ((email-or-username (or (cdr (assoc :email data))
                                 (cdr (assoc :username data))))
          (password (cdr (assoc :password data))))

      (unless (and email-or-username password)
        (return-from handle-login
          (error-response "Email/username and password are required")))

      (handler-case
          (let ((result (login-user email-or-username password)))
            ;; Set session cookie
            (hunchentoot:set-cookie *session-cookie-name*
                                   :value (cdr (assoc :session-id result))
                                   :path "/"
                                   :http-only t
                                   :secure nil ; Set to t in production with HTTPS
                                   :max-age *session-timeout*)
            (success-response result))
        (error (e)
          (error-response (format nil "~A" e)))))))

(defun handle-logout ()
  "Handle user logout endpoint"
  (set-cors-headers)
  (let ((session-id (or (hunchentoot:cookie-in *session-cookie-name*)
                        (hunchentoot:header-in* :authorization))))
    (if session-id
        (progn
          (logout-user session-id)
          ;; Clear session cookie
          (hunchentoot:set-cookie *session-cookie-name*
                                 :value ""
                                 :path "/"
                                 :max-age 0)
          (success-response '((:message . "Logged out successfully"))))
        (error-response "No active session" :status 401))))

(defun handle-session-check ()
  "Check if current session is valid"
  (set-cors-headers)
  (let ((session-id (or (hunchentoot:cookie-in *session-cookie-name*)
                        (hunchentoot:header-in* :authorization))))
    (if-let ((session (validate-session session-id)))
      (success-response session)
      (error-response "Invalid or expired session" :status 401))))

;;; Middleware for protected routes
(defun require-auth ()
  "Middleware to require authentication"
  (let ((session-id (or (hunchentoot:cookie-in *session-cookie-name*)
                        (hunchentoot:header-in* :authorization))))
    (unless (validate-session session-id)
      (setf (hunchentoot:return-code*) 401)
      (hunchentoot:abort-request-handler
       (error-response "Authentication required" :status 401)))))