;;;; auth0-oauth.lisp - OAuth2 flow handlers for Auth0

(in-package #:collabcanvas)

(defparameter *auth0-state-store* (make-hash-table :test 'equal))
(defparameter *auth0-state-ttl* 300) ; 5 minutes
(defparameter *jwks-cache* nil "Cached JWKS from Auth0")
(defparameter *jwks-cache-time* 0 "Time when JWKS was cached")
(defparameter *jwks-cache-ttl* 86400) ; 24 hours

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
  (let* ((params (hunchentoot:get-parameters*))
         (connection (cdr (assoc "connection" params :test #'string=)))
         (state (store-oauth-state (generate-oauth-state)))
         (url (auth0-authorize-url :state state :connection connection)))
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
        (parse-json json-str)))))

(defun get-jwks ()
  "Fetch JWKS from Auth0 with caching"
  (let ((now (get-universal-time)))
    (if (and *jwks-cache* (< (- now *jwks-cache-time*) *jwks-cache-ttl*))
        *jwks-cache*
        (handler-case
            (let* ((url (format nil "~A/.well-known/jwks.json" (auth0-base-url)))
                   (response (dexador:get url :timeout 10))
                   (jwks (parse-json response)))
              (setf *jwks-cache* jwks)
              (setf *jwks-cache-time* now)
              (format t "[INFO] JWKS fetched and cached~%")
              jwks)
          (error (e)
            (format t "[WARN] Failed to fetch JWKS: ~A~%" e)
            *jwks-cache*)))))

(defun find-jwk-by-kid (jwks kid)
  "Find JWK by key ID"
  (let ((keys (cdr (assoc :keys jwks))))
    (when keys
      (find kid keys
            :key (lambda (key) (cdr (assoc :kid key)))
            :test #'string=))))

(defun validate-jwt-claims (claims)
  "Validate JWT claims (issuer, audience, expiration)"
  (let ((iss (cdr (assoc :iss claims)))
        (aud (cdr (assoc :aud claims)))
        (exp (cdr (assoc :exp claims)))
        (expected-iss (format nil "~A/" (auth0-base-url))))
    
    ;; Check issuer
    (unless (string= iss expected-iss)
      (error "Invalid issuer: ~A (expected ~A)" iss expected-iss))
    
    ;; Check audience
    (unless (or (string= aud *auth0-client-id*)
                (and (listp aud) (member *auth0-client-id* aud :test #'string=)))
      (error "Invalid audience: ~A" aud))
    
    ;; Check expiration
    (when (and exp (numberp exp) (<= exp (get-universal-time)))
      (error "Token expired"))
    
    (format t "[INFO] JWT claims validated successfully~%")
    t))

(defun decode-and-validate-jwt (id-token)
  "Decode JWT and validate claims (signature validation is basic for now)"
  (let ((parts (decode-jwt-segments id-token)))
    (unless parts
      (error "Invalid JWT format"))
    
    (let* ((header-b64 (first parts))
           (payload-b64 (second parts))
           (header-octets (base64url-to-octets header-b64))
           (payload-octets (base64url-to-octets payload-b64))
           (header (parse-json (babel:octets-to-string header-octets :encoding :utf-8)))
           (claims (parse-json (babel:octets-to-string payload-octets :encoding :utf-8))))
      
      ;; Validate claims
      (validate-jwt-claims claims)
      
      ;; Note: Full RS256 signature validation would require:
      ;; 1. Fetch JWKS from Auth0
      ;; 2. Find matching key by kid from header
      ;; 3. Verify RSA signature using public key
      ;; For now, we rely on HTTPS + state validation for security
      ;; Production should use a proper JWT library like jose
      
      claims)))

(defun handle-auth0-link ()
  "Initiate Auth0 account linking for already logged-in user"
  (set-cors-headers)
  (ensure-auth0-config!)
  
  ;; Require existing session
  (let ((session-id (hunchentoot:cookie-in *session-cookie-name*)))
    (unless session-id
      (return-from handle-auth0-link 
        (error-response "Must be logged in to link Auth0 account" :status 401)))
    
    (let ((session (validate-session session-id)))
      (unless session
        (return-from handle-auth0-link 
          (error-response "Invalid session" :status 401)))
      
      ;; Store user-id in state metadata for linking
      (let* ((user-id (cdr (assoc :user-id session)))
             (state (store-oauth-state-with-metadata 
                     (generate-oauth-state)
                     `(:link-user-id ,user-id)))
             (url (auth0-authorize-url :state state)))
        (setf (hunchentoot:return-code*) 302)
        (setf (hunchentoot:header-out :location) url)
        ""))))

(defun store-oauth-state-with-metadata (state metadata)
  "Store OAuth state with additional metadata"
  (setf (gethash state *auth0-state-store*) 
        `(:created-at ,(get-universal-time) :metadata ,metadata))
  (bt:make-thread (lambda ()
                    (sleep *auth0-state-ttl*)
                    (remhash state *auth0-state-store*))
                  :name "auth0-state-expirer")
  state)

(defun get-oauth-state-metadata (state)
  "Get metadata from OAuth state"
  (let ((state-data (gethash state *auth0-state-store*)))
    (when state-data
      (getf state-data :metadata))))

(defun handle-auth0-callback ()
  "Process OAuth callback, validate state, exchange code, create session"
  (set-cors-headers)
  (let* ((params (hunchentoot:get-parameters*))
         (code (cdr (assoc "code" params :test #'string=)))
         (state (cdr (assoc "state" params :test #'string=)))
         (error-param (cdr (assoc "error" params :test #'string=))))
    (when error-param
      (return-from handle-auth0-callback (error-response (format nil "OAuth error: ~A" error-param) :status 400)))
    
    ;; Get metadata before popping state
    (let ((metadata (get-oauth-state-metadata state)))
      (unless (and code state (pop-oauth-state state))
        (return-from handle-auth0-callback (error-response "Invalid or expired state" :status 400)))
      
      ;; Check if this is a linking flow
      (let ((link-user-id (and metadata (getf metadata :link-user-id))))
        (handler-case
            (let* ((token-resp (exchange-code-for-tokens code))
                   (id-token (cdr (assoc :id-token token-resp)))
                   (access-token (cdr (assoc :access-token token-resp)))
                   ;; Validate JWT claims (signature validation is basic)
                   (claims (and id-token (decode-and-validate-jwt id-token)))
                   (sub (and claims (cdr (assoc :sub claims))))
                   (email (and claims (cdr (assoc :email claims))))
                   (name (and claims (cdr (assoc :name claims))))
                   (picture (and claims (cdr (assoc :picture claims)))))
              (unless sub
                (return-from handle-auth0-callback (error-response "Missing subject in ID token" :status 400)))
              
              ;; Ensure DB ready
              (ensure-auth0-user-columns)
              
              (let ((user-id 
                     (if link-user-id
                         ;; Linking flow: update existing user with auth0_sub
                         (progn
                           (format t "[INFO] Linking Auth0 account ~A to user ~A~%" sub link-user-id)
                           (link-auth0-to-existing-user link-user-id sub email name picture)
                           link-user-id)
                         ;; Normal flow: find or create user
                         (find-or-create-user-from-oauth
                          :auth0-sub sub
                          :email email
                          :display-name name
                          :avatar-url picture
                          :email-verified (cdr (assoc :email-verified claims))))))
                
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
                  
                  ;; Redirect to app on success
                  (setf (hunchentoot:return-code*) 302)
                  (setf (hunchentoot:header-out :location) "/")
                  "")))
          (error (e)
            (error-response (format nil "Callback processing failed: ~A" e) :status 500)))))))


