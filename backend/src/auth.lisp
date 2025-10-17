;;;; auth.lisp - Authentication logic for CollabCanvas (Auth0-only)

(in-package #:collabcanvas)

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
          (:email . ,(cdr (assoc :email session)))
          (:preferred-color . ,(cdr (assoc :preferred-color session))))))))

;;; Logout
(defun logout-user (session-id)
  "Logout a user by deleting their session"
  (delete-session session-id)
  '((:success . t)))

;;; Note: All authentication is now handled through Auth0 OAuth flow
;;; See auth0-oauth.lisp for OAuth handlers
