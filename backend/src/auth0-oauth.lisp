;;;; auth0-oauth.lisp - OAuth2 flow handlers for Auth0

(in-package #:collabcanvas)

(defparameter *auth0-state-store* (make-hash-table :test 'equal))
(defparameter *auth0-state-ttl* 300) ; 5 minutes

(defun generate-oauth-state ()
  (generate-session-id))

(defun store-oauth-state (state)
  (setf (gethash state *auth0-state-store*) (get-universal-time))
  (bt:make-thread (lambda ()
                    (sleep *auth0-state-ttl*)
                    (remhash state *auth0-state-store*))
                  :name "auth0-state-expirer")
  state)

(defun valid-oauth-state-p (state)
  (let ((created (gethash state *auth0-state-store*)))
    (and created (<= (- (get-universal-time) created) *auth0-state-ttl*))))

(defun pop-oauth-state (state)
  (prog1 (valid-oauth-state-p state)
    (remhash state *auth0-state-store*)))

(defun http-post-form (url params)
  "Minimal urlencoded POST using dexador"
  (dexador:post url :content (dexador:urlencode params)
                    :headers '(("Content-Type" . "application/x-www-form-urlencoded"))))

(defun exchange-code-for-tokens (code)
  (ensure-auth0-config!)
  (let* ((params `(("grant_type" . "authorization_code")
                   ("client_id" . ,*auth0-client-id*)
                   ("client_secret" . ,*auth0-client-secret*)
                   ("code" . ,code)
                   ("redirect_uri" . ,*auth0-callback-url*)))
         (resp (http-post-form (auth0-token-url) params))
         (body (dexador:body-string resp))
         (json (parse-json body)))
    json))

(defun handle-auth0-login ()
  "Initiate Auth0 login by redirecting to /authorize with CSRF state"
  (set-cors-headers)
  (ensure-auth0-config!)
  (let* ((state (store-oauth-state (generate-oauth-state)))
         (url (auth0-authorize-url :state state)))
    (setf (hunchentoot:return-code*) 302)
    (setf (hunchentoot:header-out :location) url)
    ""))

(defun decode-jwt-segments (jwt)
  (let* ((parts (cl-ppcre:split "\\." jwt)))
    (when (= (length parts) 3)
      (list (first parts) (second parts) (third parts)))))

(defun base64url-to-octets (s)
  (let* ((pad (mod (- 4 (mod (length s) 4)) 4))
         (padded (concatenate 'string s (make-string pad :initial-element #\=)))
         (std (substitute #\+ #\- (substitute #\/ #\_ padded))))
    (cl-base64:base64-string-to-usb8-array std)))

(defun parse-jwt-claims (id-token)
  (let ((parts (decode-jwt-segments id-token)))
    (when parts
      (let* ((payload (second parts))
             (octets (base64url-to-octets payload))
             (json-str (babel:octets-to-string octets :encoding :utf-8)))
        (parse-json json-str))))

(defun handle-auth0-callback ()
  "Process OAuth callback, validate state, exchange code, create session"
  (set-cors-headers)
  (let* ((params (hunchentoot:get-parameters*))
         (code (cdr (assoc "code" params :test #'string=)))
         (state (cdr (assoc "state" params :test #'string=)))
         (error-param (cdr (assoc "error" params :test #'string=))))
    (when error-param
      (return-from handle-auth0-callback (error-response (format nil "OAuth error: ~A" error-param) :status 400)))
    (unless (and code state (pop-oauth-state state))
      (return-from handle-auth0-callback (error-response "Invalid or expired state" :status 400)))
    (handler-case
        (let* ((token-resp (exchange-code-for-tokens code))
               (id-token (cdr (assoc :id-token token-resp)))
               (access-token (cdr (assoc :access-token token-resp)))
               (claims (and id-token (parse-jwt-claims id-token)))
               (sub (and claims (cdr (assoc :sub claims))))
               (email (and claims (cdr (assoc :email claims))))
               (name (and claims (cdr (assoc :name claims))))
               (picture (and claims (cdr (assoc :picture claims)))))
          (unless sub
            (return-from handle-auth0-callback (error-response "Missing subject in ID token" :status 400)))
          ;; Ensure DB ready and find/create user
          (ensure-auth0-user-columns)
          (let ((user-id (find-or-create-user-from-oauth
                          :auth0-sub sub
                          :email email
                          :display-name name
                          :avatar-url picture
                          :email-verified (cdr (assoc :email-verified claims)))))
            ;; Create session
            (let* ((session-id (generate-session-id))
                   (expires-at (local-time:format-timestring
                               nil (local-time:timestamp+
                                    (local-time:now) *session-timeout* :sec))))
              (create-session user-id session-id expires-at)
              (hunchentoot:set-cookie *session-cookie-name*
                                     :value session-id
                                     :path "/"
                                     :http-only t
                                     :secure nil
                                     :max-age *session-timeout*)
              (success-response `((:user-id . ,user-id)
                                  (:session-id . ,session-id)
                                  (:access-token . ,access-token))))))
      (error (e)
        (error-response (format nil "Callback processing failed: ~A" e) :status 500)))))


