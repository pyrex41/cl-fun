;;;; auth-metrics.lisp - Authentication metrics and monitoring

(in-package #:collabcanvas)

(defun count-total-users ()
  "Count total users in database"
  (let ((result (execute-single "SELECT COUNT(*) FROM users")))
    (if result (first result) 0)))

(defun count-auth0-users ()
  "Count users with Auth0 accounts (have auth0_sub)"
  (let ((result (execute-single "SELECT COUNT(*) FROM users WHERE auth0_sub IS NOT NULL")))
    (if result (first result) 0)))

(defun count-legacy-users ()
  "Count users with legacy auth (have password_hash but no auth0_sub)"
  (let ((result (execute-single 
                 "SELECT COUNT(*) FROM users WHERE password_hash IS NOT NULL AND auth0_sub IS NULL")))
    (if result (first result) 0)))

(defun count-logins-today ()
  "Count logins today (based on last_login_at)"
  (let ((result (execute-single 
                 "SELECT COUNT(*) FROM users 
                  WHERE last_login_at >= date('now') AND last_login_at < date('now', '+1 day')")))
    (if result (first result) 0)))

(defun count-failed-logins-today ()
  "Count failed login attempts today (would need tracking table)"
  ;; TODO: Implement failed login tracking
  0)

(defun count-oauth-errors-today ()
  "Count OAuth errors today (would need error log table)"
  ;; TODO: Implement OAuth error tracking
  0)

(defun get-auth-migration-stats ()
  "Get migration statistics"
  (let ((total (count-total-users))
        (auth0 (count-auth0-users))
        (legacy (count-legacy-users)))
    `((:total-users . ,total)
      (:auth0-users . ,auth0)
      (:legacy-users . ,legacy)
      (:migration-percentage . ,(if (> total 0)
                                   (round (* 100 (/ auth0 total)))
                                   0)))))

(defun get-auth-metrics ()
  "Get authentication metrics"
  `((:total-users . ,(count-total-users))
    (:auth0-users . ,(count-auth0-users))
    (:legacy-users . ,(count-legacy-users))
    (:logins-today . ,(count-logins-today))
    (:failed-logins-today . ,(count-failed-logins-today))
    (:oauth-errors-today . ,(count-oauth-errors-today))
    (:migration-stats . ,(get-auth-migration-stats))))

(defun handle-auth-metrics ()
  "HTTP handler for auth metrics endpoint"
  (set-cors-headers)
  (setf (hunchentoot:content-type*) "application/json")
  (success-response (get-auth-metrics)))

(defun log-oauth-error (error-type details)
  "Log OAuth error for monitoring"
  (format t "[AUTH-ERROR] ~A: ~A~%" error-type details)
  ;; TODO: Store in database for metrics
  )

(defun log-failed-login (email-or-username)
  "Log failed login attempt"
  (format t "[AUTH-FAIL] Failed login attempt for: ~A~%" email-or-username)
  ;; TODO: Store in database for metrics and rate limiting
  )
