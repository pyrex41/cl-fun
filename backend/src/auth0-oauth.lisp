;;;; auth0-oauth.lisp - OAuth2 flow handlers for Auth0

(in-package #:collabcanvas)

;;; Constants
(defconstant +state-token-ttl+ 300
  "State token time-to-live in seconds (5 minutes)")

(defconstant +jwks-cache-ttl+ 86400
  "JWKS cache time-to-live in seconds (24 hours)")

(defconstant +state-cleanup-interval+ 60
  "Interval between state cleanup runs in seconds (1 minute)")

(defconstant +http-timeout+ 10
  "HTTP request timeout in seconds")

(defconstant +rate-limit-window+ 3600
  "Rate limit time window in seconds (1 hour)")

(defconstant +rate-limit-max-requests+ 1000
  "Maximum requests per IP per window (increased for development)")

(defconstant +rate-limit-cleanup-interval+ 300
  "Interval between rate limit cleanup runs in seconds (5 minutes)")

;;; Helper Functions
(defun cors-headers ()
  "Return CORS headers as plist for Clack responses"
  (list :access-control-allow-origin *cors-origin*
        :access-control-allow-methods "GET, POST, PUT, DELETE, OPTIONS"
        :access-control-allow-headers "Content-Type, Authorization, X-Session-ID"
        :access-control-allow-credentials "true"))

(defun parse-query-parameters (env)
  "Parse query string from env into alist"
  (let ((query-string (getf env :query-string)))
    (when query-string
      (mapcar (lambda (pair)
                (let ((parts (cl-ppcre:split "=" pair)))
                  (cons (first parts) (second parts))))
              (cl-ppcre:split "&" query-string)))))

(defun get-cookie (env cookie-name)
  "Get cookie value from env by name"
  (let ((cookie-header (getf env :http-cookie)))
    (when cookie-header
      (let ((cookies (cl-ppcre:split "; ?" cookie-header)))
        (dolist (cookie cookies)
          (let ((parts (cl-ppcre:split "=" cookie)))
            (when (string= (first parts) cookie-name)
              (return (second parts)))))))))

;;; Global state variables
(defparameter *auth0-state-store* (make-hash-table :test 'equal)
  "Hash table storing OAuth state tokens with timestamps")

(defparameter *auth0-state-ttl* +state-token-ttl+
  "State token TTL (for backward compatibility)")

(defparameter *jwks-cache* nil
  "Cached JWKS from Auth0")

(defparameter *jwks-cache-time* 0
  "Time when JWKS was cached")

(defparameter *jwks-cache-ttl* +jwks-cache-ttl+
  "JWKS cache TTL (for backward compatibility)")

(defparameter *state-cleanup-thread* nil
  "Background thread for cleaning expired states")

(defparameter *use-secure-cookies*
  (let ((env (uiop:getenv "ENVIRONMENT")))
    (or (string-equal env "production")
        (string-equal env "prod")))
  "Whether to set Secure flag on cookies (true for production, false for development)")

(defparameter *rate-limit-store* (make-hash-table :test 'equal)
  "Hash table storing request timestamps per IP address for rate limiting")

(defparameter *rate-limit-lock* (bt:make-lock "rate-limit-lock")
  "Lock for thread-safe access to rate limit store")

(defparameter *rate-limit-cleanup-thread* nil
  "Background thread for cleaning expired rate limit entries")

(defun cleanup-expired-states ()
  "Remove expired state tokens from store in a thread-safe manner.
   Collects expired keys first, then removes them in a separate pass."
  (let ((now (get-universal-time))
        (expired-keys '()))
    ;; First pass: collect expired keys without modifying hash table
    (maphash (lambda (state data)
               (let ((created-at (if (listp data)
                                     (getf data :created-at)
                                     data)))
                 (when (and created-at
                           (> (- now created-at) *auth0-state-ttl*))
                   (push state expired-keys))))
             *auth0-state-store*)
    ;; Second pass: remove expired keys
    (dolist (key expired-keys)
      (remhash key *auth0-state-store*))
    (when expired-keys
      (format t "[INFO] Cleaned up ~A expired state token(s)~%" (length expired-keys)))))

(defun start-state-cleanup-thread ()
  "Start background thread to periodically clean expired states"
  (when (and *state-cleanup-thread* (bt:thread-alive-p *state-cleanup-thread*))
    (return-from start-state-cleanup-thread *state-cleanup-thread*))
  (setf *state-cleanup-thread*
        (bt:make-thread
         (lambda ()
           (loop
             (sleep +state-cleanup-interval+)
             (handler-case
                 (cleanup-expired-states)
               (error (e)
                 (format t "[ERROR] State cleanup failed: ~A~%" e)))))
         :name "auth0-state-cleanup")))

;;; Rate Limiting Functions
(defun get-client-ip (env)
  "Extract client IP address from request, considering proxy headers.
   Parameters:
     ENV - Clack request environment
   Returns IP address string (IPv4 or IPv6)."
  (or (gethash "x-forwarded-for" (getf env :headers))
      (gethash "x-real-ip" (getf env :headers))
      (getf env :remote-addr)))

(defun check-rate-limit (ip)
  "Check if IP address has exceeded rate limit using sliding window.
   Parameters:
     IP - IP address string
   Returns T if request is allowed, NIL if rate limit exceeded.
   Side effect: Records current request timestamp if allowed."
  (bt:with-lock-held (*rate-limit-lock*)
    (let* ((now (get-universal-time))
           (timestamps (gethash ip *rate-limit-store* '()))
           ;; Remove timestamps older than the window (sliding window)
           (recent-timestamps (remove-if (lambda (ts)
                                          (> (- now ts) +rate-limit-window+))
                                        timestamps)))
      (if (>= (length recent-timestamps) +rate-limit-max-requests+)
          ;; Rate limit exceeded
          (progn
            (setf (gethash ip *rate-limit-store*) recent-timestamps)
            nil)
          ;; Allow request and record timestamp
          (progn
            (setf (gethash ip *rate-limit-store*)
                  (cons now recent-timestamps))
            t)))))

(defun cleanup-expired-rate-limits ()
  "Remove rate limit entries for IPs with no recent requests.
   Helps prevent memory leaks from abandoned IP addresses."
  (bt:with-lock-held (*rate-limit-lock*)
    (let ((now (get-universal-time))
          (expired-ips '()))
      ;; Find IPs with no requests in the window
      (maphash (lambda (ip timestamps)
                 (when (every (lambda (ts)
                               (> (- now ts) +rate-limit-window+))
                             timestamps)
                   (push ip expired-ips)))
               *rate-limit-store*)
      ;; Remove expired entries
      (dolist (ip expired-ips)
        (remhash ip *rate-limit-store*))
      (when expired-ips
        (format t "[INFO] Cleaned up ~A expired rate limit entries~%" (length expired-ips))))))

(defun start-rate-limit-cleanup-thread ()
  "Start background thread to periodically clean expired rate limit entries"
  (when (and *rate-limit-cleanup-thread* (bt:thread-alive-p *rate-limit-cleanup-thread*))
    (return-from start-rate-limit-cleanup-thread *rate-limit-cleanup-thread*))
  (setf *rate-limit-cleanup-thread*
        (bt:make-thread
         (lambda ()
           (loop
             (sleep +rate-limit-cleanup-interval+)
             (handler-case
                 (cleanup-expired-rate-limits)
               (error (e)
                 (format t "[ERROR] Rate limit cleanup failed: ~A~%" e)))))
         :name "rate-limit-cleanup")))

(defun rate-limit-exceeded-response ()
  "Return HTTP 429 Too Many Requests response (Clack format)"
  (list 429
        (list :content-type "application/json")
        (list (format nil "{\"error\":\"Too many requests. Please try again later.\",\"retry_after\":~A}"
                      +rate-limit-window+))))

(defun generate-oauth-state ()
  "Generate a secure random state token for OAuth CSRF protection.
   Returns a new random session ID string."
  (generate-session-id))

(defun store-oauth-state (state)
  "Store OAuth state with creation timestamp.
   Cleanup is handled by the centralized cleanup thread."
  (setf (gethash state *auth0-state-store*) (get-universal-time))
  state)

(defun valid-oauth-state-p (state)
  "Check if an OAuth state token is valid and not expired.
   Parameters:
     STATE - OAuth state token string to validate
   Returns T if valid and not expired, NIL otherwise."
  (let ((created (gethash state *auth0-state-store*)))
    (and created (<= (- (get-universal-time) created) *auth0-state-ttl*))))

(defun pop-oauth-state (state)
  "Validate and consume an OAuth state token (single-use).
   Parameters:
     STATE - OAuth state token string to validate and remove
   Returns T if valid, NIL otherwise. Removes the state from store either way."
  (prog1 (valid-oauth-state-p state)
    (remhash state *auth0-state-store*)))

(defun http-post-form (url params)
  "Minimal urlencoded POST using dexador.
   Parameters:
     URL - Target URL string
     PARAMS - Alist of (key . value) pairs to URL-encode
   Returns dexador response"
  (let ((body (with-output-to-string (s)
                (loop for (key . value) in params
                      for first = t then nil
                      unless first do (write-string "&" s)
                      do (format s "~A=~A"
                                 (quri:url-encode key)
                                 (quri:url-encode value))))))
    (dexador:post url
                  :content body
                  :headers '(("Content-Type" . "application/x-www-form-urlencoded")))))

(defun exchange-code-for-tokens (code)
  "Exchange OAuth authorization code for access and ID tokens.
   Parameters:
     CODE - Authorization code received from Auth0 callback
   Returns alist with :id-token and :access-token keys.
   Signals specific errors: dexador:http-request-failed for network errors,
   error for JSON parsing failures."
  (ensure-auth0-config!)
  (handler-case
      (let* ((params `(("grant_type" . "authorization_code")
                       ("client_id" . ,*auth0-client-id*)
                       ("client_secret" . ,*auth0-client-secret*)
                       ("code" . ,code)
                       ("redirect_uri" . ,*auth0-callback-url*)))
             (resp (http-post-form (auth0-token-url) params)))
        (handler-case
            (parse-json resp)
          (error (e)
            (format t "[ERROR] Failed to parse token response: ~A~%" e)
            (error "OAuth provider returned malformed JSON response"))))
    (dexador:http-request-failed (e)
      (format t "[ERROR] Network error during token exchange: ~A~%" e)
      (error "Failed to connect to OAuth provider (network error)"))
    (error (e)
      (format t "[ERROR] Unexpected error during token exchange: ~A~%" e)
      (error "Token exchange failed: ~A" e))))

(defun handle-auth0-login (env)
  "Initiate Auth0 login by redirecting to /authorize with CSRF state.
   Parameters:
     ENV - Clack request environment
   Returns Clack response (status headers body).
   Rate limited to prevent abuse."
  ;; Check rate limit first
  (let ((client-ip (get-client-ip env)))
    (unless (check-rate-limit client-ip)
      (format t "[WARN] Rate limit exceeded for IP: ~A on /auth0/login~%" client-ip)
      (return-from handle-auth0-login (rate-limit-exceeded-response))))

  (ensure-auth0-config!)
  (let* ((params (parse-query-parameters env))
         (connection (cdr (assoc "connection" params :test #'string=)))
         (state (store-oauth-state (generate-oauth-state)))
         (url (auth0-authorize-url :state state :connection connection)))
    (list 302
          (append (cors-headers)
                  (list :location url))
          '(""))))

(defun decode-jwt-segments (jwt)
  "Split JWT token into header, payload, and signature segments.
   Parameters:
     JWT - JWT token string
   Returns list of (header payload signature) as base64url strings, or NIL if invalid format."
  (let* ((parts (cl-ppcre:split "\\." jwt)))
    (when (= (length parts) 3)
      (list (first parts) (second parts) (third parts)))))

(defun base64url-to-octets (s)
  "Decode base64url-encoded string to byte array.
   Parameters:
     S - Base64url-encoded string (URL-safe base64 without padding)
   Returns octet array (usb8 array).
   Handles padding and converts URL-safe characters to standard base64."
  (let* ((pad (mod (- 4 (mod (length s) 4)) 4))
         (padded (concatenate 'string s (make-string pad :initial-element #\=)))
         (std (substitute #\+ #\- (substitute #\/ #\_ padded))))
    (cl-base64:base64-string-to-usb8-array std)))

(defun parse-jwt-claims (id-token)
  "Extract and parse claims from JWT ID token payload.
   Parameters:
     ID-TOKEN - JWT token string
   Returns alist of claims, or NIL if invalid format.
   Does not validate signature - use decode-and-validate-jwt for full validation."
  (let ((parts (decode-jwt-segments id-token)))
    (when parts
      (let* ((payload (second parts))
             (octets (base64url-to-octets payload))
             (json-str (babel:octets-to-string octets :encoding :utf-8)))
        (parse-json json-str)))))

(defun get-jwks ()
  "Fetch JWKS from Auth0 with caching.
   Returns cached JWKS if still valid, otherwise fetches fresh copy.
   Falls back to cached version on network errors."
  (let ((now (get-universal-time)))
    (if (and *jwks-cache* (< (- now *jwks-cache-time*) *jwks-cache-ttl*))
        *jwks-cache*
        (handler-case
            (let* ((url (format nil "~A/.well-known/jwks.json" (auth0-base-url)))
                   (response (dexador:get url)))
              (handler-case
                  (let ((jwks (parse-json response)))
                    (setf *jwks-cache* jwks)
                    (setf *jwks-cache-time* now)
                    (format t "[INFO] JWKS fetched and cached~%")
                    jwks)
                (error (e)
                  (format t "[ERROR] Failed to parse JWKS JSON: ~A~%" e)
                  (if *jwks-cache*
                      (progn
                        (format t "[INFO] Using cached JWKS due to parse error~%")
                        *jwks-cache*)
                      (error "JWKS unavailable and no cached version")))))
          (dexador:http-request-failed (e)
            (format t "[WARN] Network error fetching JWKS: ~A~%" e)
            (if *jwks-cache*
                (progn
                  (format t "[INFO] Using cached JWKS due to network error~%")
                  *jwks-cache*)
                (error "JWKS unavailable: network error and no cached version")))
          (error (e)
            (format t "[WARN] Unexpected error fetching JWKS: ~A~%" e)
            (if *jwks-cache*
                *jwks-cache*
                (error "JWKS unavailable: ~A" e)))))))

(defun find-jwk-by-kid (jwks kid)
  "Find JWK by key ID from JWKS.
   Parameters:
     JWKS - JSON Web Key Set (alist with :keys)
     KID - Key ID string to find
   Returns JWK alist or NIL if not found."
  (let ((keys (cdr (assoc :keys jwks))))
    (when keys
      (find kid keys
            :key (lambda (key) (cdr (assoc :kid key)))
            :test #'string=))))

(defun jwk-to-rsa-public-key (jwk)
  "Convert JWK to RSA public key for signature verification.
   Parameters:
     JWK - JSON Web Key alist containing :n (modulus) and :e (exponent)
   Returns ironclad RSA public key object."
  (let* ((n-b64 (cdr (assoc :n jwk)))
         (e-b64 (cdr (assoc :e jwk)))
         (n-octets (base64url-to-octets n-b64))
         (e-octets (base64url-to-octets e-b64))
         ;; Convert octets to integers (big-endian)
         (n (ironclad:octets-to-integer n-octets))
         (e (ironclad:octets-to-integer e-octets)))
    (ironclad:make-public-key :rsa :n n :e e)))

(defun unix-time-to-universal-time (unix-timestamp)
  "Convert Unix timestamp (seconds since 1970-01-01) to Universal Time (seconds since 1900-01-01).
   Parameters:
     UNIX-TIMESTAMP - Integer seconds since Unix epoch (1970-01-01 00:00:00 UTC)
   Returns Universal Time integer."
  ;; Difference between Unix epoch (1970) and Universal Time epoch (1900) is 2208988800 seconds
  (+ unix-timestamp 2208988800))

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
    
    ;; Check expiration (convert Unix timestamp to Universal Time for comparison)
    (when (and exp (numberp exp))
      (let ((exp-universal (unix-time-to-universal-time exp))
            (now-universal (get-universal-time)))
        (format t "[DEBUG] Token exp (Unix): ~A, exp (Universal): ~A, now (Universal): ~A~%"
                exp exp-universal now-universal)
        (when (<= exp-universal now-universal)
          (error "Token expired"))))
    
    (format t "[INFO] JWT claims validated successfully~%")
    t))

(defun decode-and-validate-jwt (id-token)
  "Decode JWT and validate both signature and claims using RS256.
   Parameters:
     ID-TOKEN - Complete JWT token string
   Returns claims alist if valid, signals error otherwise.
   Performs full cryptographic signature verification using Auth0's JWKS and jose library."
  (let ((parts (decode-jwt-segments id-token)))
    (unless parts
      (error "Invalid JWT format"))

    ;; First, parse header to get kid and algorithm
    (let* ((header-b64 (first parts))
           (header-octets (base64url-to-octets header-b64))
           (header (parse-json (babel:octets-to-string header-octets :encoding :utf-8)))
           (kid (cdr (assoc :kid header)))
           (alg (cdr (assoc :alg header))))

      (format t "[DEBUG] JWT header kid: ~A, alg: ~A~%" kid alg)

      ;; Ensure RS256 algorithm
      (unless (string= alg "RS256")
        (error "Unsupported JWT algorithm: ~A (expected RS256)" alg))

      ;; Fetch JWKS and find matching key
      (let* ((jwks (get-jwks))
             (jwk (find-jwk-by-kid jwks kid)))

        (format t "[DEBUG] JWKS fetched, searching for kid: ~A~%" kid)

        (unless jwk
          (format t "[ERROR] Available kids in JWKS: ~A~%"
                  (mapcar (lambda (key) (cdr (assoc :kid key)))
                          (cdr (assoc :keys jwks))))
          (error "JWT key ID '~A' not found in JWKS" kid))

        (format t "[DEBUG] Found matching JWK for kid: ~A~%" kid)
        (format t "[DEBUG] JWK kty: ~A, use: ~A, alg: ~A~%"
                (cdr (assoc :kty jwk))
                (cdr (assoc :use jwk))
                (cdr (assoc :alg jwk)))

        ;; Convert JWK to RSA public key
        (let ((public-key (jwk-to-rsa-public-key jwk)))
          (format t "[DEBUG] Public key created: ~A~%" public-key)

          ;; Use jose library to decode and verify signature
          ;; jose:decode will raise an error if signature is invalid
          (handler-case
              (progn
                (format t "[DEBUG] Calling jose:decode with :rs256~%")
                (let ((claims (jose:decode :rs256 public-key id-token)))
                  (format t "[INFO] JWT signature verified successfully by jose library (kid: ~A)~%" kid)
                  (format t "[DEBUG] Claims type: ~A~%" (type-of claims))
                  (format t "[DEBUG] Claims value: ~A~%" claims)

                  ;; Convert claims to alist with keyword keys
                  (let ((claims-alist (cond
                                        ;; Hash table: convert to alist with keyword keys
                                        ((hash-table-p claims)
                                         (let ((alist '()))
                                           (maphash (lambda (k v)
                                                     (push (cons (intern (string-upcase (string k)) :keyword) v) alist))
                                                   claims)
                                           alist))
                                        ;; Already an alist: convert symbol keys to keyword keys
                                        ((listp claims)
                                         (mapcar (lambda (pair)
                                                  (cons (intern (string-upcase (string (car pair))) :keyword)
                                                        (cdr pair)))
                                                claims))
                                        ;; Unknown format
                                        (t claims))))
                    (format t "[DEBUG] Claims alist (after conversion): ~A~%" claims-alist)

                    ;; Validate claims (issuer, audience, expiration)
                    (validate-jwt-claims claims-alist)

                    claims-alist)))
            (error (e)
              (format t "[ERROR] JWT verification failed: ~A~%" e)
              (error "JWT signature verification failed: ~A" e))))))))

(defun handle-auth0-link (env)
  "Initiate Auth0 account linking for already logged-in user.
   Parameters:
     ENV - Clack request environment
   Returns Clack response (status headers body).
   Rate limited to prevent abuse."
  ;; Check rate limit first
  (let ((client-ip (get-client-ip env)))
    (unless (check-rate-limit client-ip)
      (format t "[WARN] Rate limit exceeded for IP: ~A on /auth0/link~%" client-ip)
      (return-from handle-auth0-link (rate-limit-exceeded-response))))

  (ensure-auth0-config!)

  ;; Require existing session
  (let ((session-id (get-cookie env *session-cookie-name*)))
    (unless session-id
      (return-from handle-auth0-link
        (clack-error-response "Must be logged in to link Auth0 account" :status 401)))

    (let ((session (validate-session session-id)))
      (unless session
        (return-from handle-auth0-link
          (clack-error-response "Invalid session" :status 401)))

      ;; Store user-id in state metadata for linking
      (let* ((user-id (cdr (assoc :user-id session)))
             (state (store-oauth-state-with-metadata
                     (generate-oauth-state)
                     `(:link-user-id ,user-id)))
             (url (auth0-authorize-url :state state)))
        (list 302
              (append (cors-headers)
                      (list :location url))
              '(""))))))

(defun store-oauth-state-with-metadata (state metadata)
  "Store OAuth state with additional metadata and creation timestamp.
   Cleanup is handled by the centralized cleanup thread."
  (setf (gethash state *auth0-state-store*)
        `(:created-at ,(get-universal-time) :metadata ,metadata))
  state)

(defun get-oauth-state-metadata (state)
  "Get metadata from OAuth state"
  (let ((state-data (gethash state *auth0-state-store*)))
    (when (and state-data (listp state-data))
      (getf state-data :metadata))))

;; Pure internal functions for Clack compatibility

(defun build-auth0-login-redirect (&optional connection)
  "Build Auth0 login redirect URL with CSRF state.
   Parameters:
     CONNECTION - Optional Auth0 connection name (e.g., 'google-oauth2')
   Returns alist with :redirect-url key."
  (ensure-auth0-config!)
  (let* ((state (store-oauth-state (generate-oauth-state)))
         (url (auth0-authorize-url :state state :connection connection)))
    `((:redirect-url . ,url))))

(defun handle-auth0-callback-internal (code state)
  "Process OAuth callback internally without Hunchentoot context.
   Parameters:
     CODE - Authorization code from Auth0
     STATE - CSRF state token to validate
   Returns alist with :session-id, :user-id, :username keys on success,
   or signals error on failure."
  ;; Get metadata before popping state
  (let ((metadata (get-oauth-state-metadata state)))
    (unless (and code state (pop-oauth-state state))
      (error "Invalid or expired state"))

    ;; Check if this is a linking flow
    (let ((link-user-id (and metadata (getf metadata :link-user-id))))
      (handler-case
          (let* ((token-resp (exchange-code-for-tokens code))
                 (id-token (cdr (assoc :id_token token-resp)))
                 (access-token (cdr (assoc :access_token token-resp))))
            (format t "[DEBUG] Token response keys: ~A~%" (mapcar #'car token-resp))
            (format t "[DEBUG] ID token present: ~A~%" (if id-token "YES" "NO"))
            (format t "[DEBUG] ID token length: ~A~%" (if id-token (length id-token) 0))
            (let* ((claims (and id-token (decode-and-validate-jwt id-token)))
                   (sub (and claims (cdr (assoc :sub claims))))
                   (email (and claims (cdr (assoc :email claims))))
                   (name (and claims (cdr (assoc :name claims))))
                   (picture (and claims (cdr (assoc :picture claims)))))
              (format t "[DEBUG] Claims present: ~A~%" (if claims "YES" "NO"))
              (format t "[DEBUG] Sub claim: ~A~%" sub)
              (unless sub
                (error "Missing subject in ID token"))

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

                ;; Get user info for response
                (let ((user (get-user-by-id user-id)))
                  ;; Create session
                  (let* ((session-id (generate-session-id))
                         (expires-at (local-time:format-timestring
                                     nil (local-time:timestamp+
                                          (local-time:now) *session-timeout* :sec))))
                    (create-session user-id session-id expires-at)

                    ;; Return session data
                    `((:session-id . ,session-id)
                      (:user-id . ,user-id)
                      (:username . ,(cdr (assoc :username user)))))))))
        (error (e)
          (format t "[ERROR] Callback processing failed: ~A~%" e)
          (error "Callback processing failed: ~A" e))))))

(defun handle-auth0-link-internal (session-id code state)
  "Link Auth0 account to existing user session.
   Parameters:
     SESSION-ID - Active session ID
     CODE - Authorization code from Auth0
     STATE - CSRF state token to validate
   Returns alist with :success t on success, or :success nil :error message on failure."
  (let ((session (validate-session session-id)))
    (unless session
      (return-from handle-auth0-link-internal
        `((:success . nil) (:error . "Invalid session"))))

    (let ((user-id (cdr (assoc :user-id session))))
      ;; Get metadata before popping state
      (let ((metadata (get-oauth-state-metadata state)))
        (unless (and code state (pop-oauth-state state))
          (return-from handle-auth0-link-internal
            `((:success . nil) (:error . "Invalid or expired state"))))

        (handler-case
            (let* ((token-resp (exchange-code-for-tokens code))
                   (id-token (cdr (assoc :id_token token-resp)))
                   (claims (and id-token (decode-and-validate-jwt id-token)))
                   (sub (and claims (cdr (assoc :sub claims))))
                   (email (and claims (cdr (assoc :email claims))))
                   (name (and claims (cdr (assoc :name claims))))
                   (picture (and claims (cdr (assoc :picture claims)))))
              (unless sub
                (return-from handle-auth0-link-internal
                  `((:success . nil) (:error . "Missing subject in ID token"))))

              ;; Ensure DB ready
              (ensure-auth0-user-columns)

              ;; Link account
              (link-auth0-to-existing-user user-id sub email name picture)

              `((:success . t) (:message . "Account linked successfully")))
          (error (e)
            (format t "[ERROR] Link processing failed: ~A~%" e)
            `((:success . nil) (:error . ,(format nil "~A" e)))))))))

(defun handle-auth0-callback (env)
  "Process OAuth callback, validate state, exchange code, create session.
   Parameters:
     ENV - Clack request environment
   Returns Clack response (status headers body).
   Rate limited to prevent abuse."
  ;; Check rate limit first
  (let ((client-ip (get-client-ip env)))
    (unless (check-rate-limit client-ip)
      (format t "[WARN] Rate limit exceeded for IP: ~A on /auth0/callback~%" client-ip)
      (return-from handle-auth0-callback (rate-limit-exceeded-response))))

  (let* ((params (parse-query-parameters env))
         (code (cdr (assoc "code" params :test #'string=)))
         (state (cdr (assoc "state" params :test #'string=)))
         (error-param (cdr (assoc "error" params :test #'string=))))
    (when error-param
      (return-from handle-auth0-callback (clack-error-response (format nil "OAuth error: ~A" error-param) :status 400)))

    ;; Get metadata before popping state
    (let ((metadata (get-oauth-state-metadata state)))
      (unless (and code state (pop-oauth-state state))
        (return-from handle-auth0-callback (clack-error-response "Invalid or expired state" :status 400)))

      ;; Check if this is a linking flow
      (let ((link-user-id (and metadata (getf metadata :link-user-id))))
        (handler-case
            (let* ((token-resp (exchange-code-for-tokens code))
                   (id-token (cdr (assoc :id_token token-resp)))
                   (access-token (cdr (assoc :access_token token-resp)))
                   ;; Validate JWT claims (signature validation is basic)
                   (claims (and id-token (decode-and-validate-jwt id-token)))
                   (sub (and claims (cdr (assoc :sub claims))))
                   (email (and claims (cdr (assoc :email claims))))
                   (name (and claims (cdr (assoc :name claims))))
                   (picture (and claims (cdr (assoc :picture claims)))))
              (unless sub
                (return-from handle-auth0-callback (clack-error-response "Missing subject in ID token" :status 400)))

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

                  ;; Build Set-Cookie header value
                  (let ((cookie-value (format nil "~A=~A; Path=/; HttpOnly~@[; Secure~]; Max-Age=~A"
                                              *session-cookie-name*
                                              session-id
                                              *use-secure-cookies*
                                              *session-timeout*)))
                    ;; Redirect to app on success with session cookie
                    (list 302
                          (append (cors-headers)
                                  (list :location "/"
                                        :set-cookie cookie-value))
                          '(""))))))
          (error (e)
            (clack-error-response (format nil "Callback processing failed: ~A" e) :status 500)))))))


