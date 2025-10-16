;;;; auth0-config.lisp - Auth0 configuration and URL helpers

(in-package #:collabcanvas)

;;; Auth0 Environment Configuration
(defparameter *auth0-domain*
  (uiop:getenv "AUTH0_DOMAIN")
  "Auth0 domain (e.g., your-tenant.us.auth0.com)")

(defparameter *auth0-client-id*
  (uiop:getenv "AUTH0_CLIENT_ID")
  "Auth0 application client ID")

(defparameter *auth0-client-secret*
  (uiop:getenv "AUTH0_CLIENT_SECRET")
  "Auth0 application client secret")

(defparameter *auth0-audience*
  (uiop:getenv "AUTH0_AUDIENCE")
  "Auth0 API audience (optional)")

(defparameter *auth0-callback-url*
  (or (uiop:getenv "AUTH0_CALLBACK_URL")
      (format nil "http://~A:~A/auth0/callback" *host* *port*))
  "OAuth2 callback URL registered in Auth0")

(defun auth0-base-url ()
  "Return base URL for Auth0 tenant"
  (when *auth0-domain*
    (format nil "https://~A" *auth0-domain*)))

(defun %join-with (items sep)
  (with-output-to-string (out)
    (loop for i from 0 below (length items) do
      (when (> i 0) (princ sep out))
      (princ (elt items i) out))))

(defun auth0-authorize-url (&key state scope connection)
  "Build the Auth0 /authorize URL with required parameters"
  (let* ((base (format nil "~A/authorize" (auth0-base-url)))
         (params (remove nil
                         (list (cons "response_type" "code")
                               (cons "client_id" *auth0-client-id*)
                               (cons "redirect_uri" *auth0-callback-url*)
                               (cons "scope" (or scope "openid profile email"))
                               (cons "state" state)
                               (when *auth0-audience* (cons "audience" *auth0-audience*))
                               (when connection (cons "connection" connection)))))
         (encoded-params (mapcar (lambda (pair)
                                   (format nil "~A=~A"
                                           (quri:url-encode (car pair))
                                           (quri:url-encode (cdr pair))))
                                 params)))
    (concatenate 'string base "?" (%join-with encoded-params "&"))))

(defun auth0-token-url ()
  (format nil "~A/oauth/token" (auth0-base-url)))

(defun auth0-userinfo-url ()
  (format nil "~A/userinfo" (auth0-base-url)))

(defun auth0-logout-url (&key return-to)
  (let* ((base (format nil "~A/v2/logout" (auth0-base-url)))
         (params (remove nil
                         (list (format nil "client_id=~A" (quri:url-encode *auth0-client-id*))
                               (when return-to
                                 (format nil "returnTo=~A" (quri:url-encode return-to)))))))
    (concatenate 'string base "?" (%join-with params "&"))))

(defun ensure-auth0-config! ()
  "Validate that required Auth0 env vars are present; signal error if missing"
  (dolist (pair (list (cons "AUTH0_DOMAIN" *auth0-domain*)
                      (cons "AUTH0_CLIENT_ID" *auth0-client-id*)
                      (cons "AUTH0_CLIENT_SECRET" *auth0-client-secret*)))
    (unless (and (cdr pair) (> (length (cdr pair)) 0))
      (error "Missing required env var ~A for Auth0 configuration" (car pair))))
  t)


