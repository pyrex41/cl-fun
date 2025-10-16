# PRD: Auth0 OAuth2 Authentication Integration

**Feature ID:** AUTH-004
**Priority:** P1 (High - Production Security)
**Version:** 1.0
**Last Updated:** October 2025
**Status:** Planning

---

## Executive Summary

Replace custom email/password authentication with Auth0 OAuth2/OIDC (OpenID Connect) flow for production-grade security, social login support, and enterprise features. This eliminates the need to handle passwords directly, reduces security surface area, and provides a foundation for SSO and multi-factor authentication.

### Key Benefits
- **Zero password handling**: Auth0 manages credentials, not our database
- **Social login**: Google, GitHub, Microsoft, etc. out of the box
- **Enterprise SSO**: SAML, Active Directory integration for corporate users
- **Multi-factor auth**: SMS, authenticator apps, WebAuthn
- **Security best practices**: Auth0 handles OWASP Top 10 auth vulnerabilities
- **Compliance**: SOC2, GDPR, HIPAA-ready authentication

### Migration Path
- **Phase 1**: Add Auth0 alongside existing custom auth (dual-mode)
- **Phase 2**: Migrate existing users to Auth0 (email verification flow)
- **Phase 3**: Deprecate custom auth, Auth0 only

---

## Problem Statement

### Current Custom Auth Limitations

**Security Concerns:**
```lisp
;; Current implementation - SHA-256 only (insufficient)
(defun hash-password (password)
  "Hash with SHA-256 - NOT SECURE for passwords!"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
    (ironclad:ascii-string-to-byte-array password))))
```

**Issues:**
1. **No salt**: Rainbow table attacks possible
2. **Fast hashing**: SHA-256 can be brute-forced (billions/sec on GPU)
3. **No key stretching**: Should use bcrypt/argon2 with work factor
4. **Password reset**: Not implemented (security hole)
5. **Email verification**: Not implemented (spam/abuse risk)
6. **Session fixation**: No CSRF protection
7. **No MFA**: Single factor authentication only

**Operational Burden:**
- Must handle PII (passwords) - compliance risk
- Must implement forgot password flow
- Must handle email verification
- Must monitor for credential stuffing attacks
- Must implement rate limiting per account
- Must handle password policy enforcement

### Auth0 Solution

**Security:**
- Industry-standard bcrypt password hashing (work factor 10+)
- Automatic rate limiting and bot detection
- Breach password detection (HaveIBeenPwned integration)
- CSRF and session fixation protection
- Secure cookie settings (HttpOnly, SameSite, Secure)

**Features:**
- Social login (Google, GitHub, Microsoft, Apple, etc.)
- MFA (SMS, TOTP, WebAuthn)
- Password reset with email verification
- Email verification on signup
- User profile management
- Enterprise SSO (SAML, AD)

---

## Technical Architecture

### OAuth2 Authorization Code Flow

```
┌──────────┐                                  ┌──────────┐
│  User    │                                  │  Auth0   │
│  Browser │                                  │  Server  │
└────┬─────┘                                  └────┬─────┘
     │                                             │
     │ 1. Click "Login with Auth0"                │
     ├────────────────────────────────────────────►
     │                                             │
     │ 2. Redirect to Auth0 login page            │
     │    (/authorize endpoint)                   │
     ◄────────────────────────────────────────────┤
     │                                             │
     │ 3. User enters credentials                 │
     ├────────────────────────────────────────────►
     │                                             │
     │ 4. Redirect back with auth code            │
     │    (to /auth/callback)                     │
     ◄────────────────────────────────────────────┤
     │                                             │
     ▼                                             │
┌──────────┐                                      │
│  Backend │                                      │
│  Server  │  5. Exchange code for tokens         │
└────┬─────┘     (POST /oauth/token)              │
     ├────────────────────────────────────────────►
     │                                             │
     │ 6. Return access_token + id_token          │
     ◄────────────────────────────────────────────┤
     │                                             │
     │ 7. Validate id_token (JWT signature)       │
     │                                             │
     │ 8. Extract user info from JWT claims       │
     │                                             │
     │ 9. Create local session                    │
     │                                             │
     │ 10. Redirect to app with session cookie    │
     ├────────────────────────────────────────────►
     │                                             │
```

### Components

#### 1. Auth0 Configuration (`src/auth0-config.lisp`)

```lisp
(defparameter *auth0-domain* (uiop:getenv "AUTH0_DOMAIN")
  "Auth0 tenant domain (e.g., 'collabcanvas.us.auth0.com')")

(defparameter *auth0-client-id* (uiop:getenv "AUTH0_CLIENT_ID")
  "OAuth2 client ID")

(defparameter *auth0-client-secret* (uiop:getenv "AUTH0_CLIENT_SECRET")
  "OAuth2 client secret")

(defparameter *auth0-callback-url* (uiop:getenv "AUTH0_CALLBACK_URL")
  "Callback URL (e.g., 'https://app.collabcanvas.com/auth/callback')")

(defparameter *auth0-audience* (uiop:getenv "AUTH0_AUDIENCE")
  "API identifier (optional, for API access tokens)")

(defun auth0-authorize-url (state)
  "Build Auth0 authorization URL"
  (format nil "https://~A/authorize?~A"
          *auth0-domain*
          (quri:url-encode-params
           `(("response_type" . "code")
             ("client_id" . ,*auth0-client-id*)
             ("redirect_uri" . ,*auth0-callback-url*)
             ("scope" . "openid profile email")
             ("state" . ,state)))))

(defun auth0-token-url ()
  "Auth0 token endpoint"
  (format nil "https://~A/oauth/token" *auth0-domain*))

(defun auth0-userinfo-url ()
  "Auth0 userinfo endpoint"
  (format nil "https://~A/userinfo" *auth0-domain*))

(defun auth0-logout-url (return-to)
  "Build Auth0 logout URL"
  (format nil "https://~A/v2/logout?~A"
          *auth0-domain*
          (quri:url-encode-params
           `(("client_id" . ,*auth0-client-id*)
             ("returnTo" . ,return-to)))))
```

#### 2. OAuth2 Flow Handlers (`src/auth0-oauth.lisp`)

**Step 1: Initiate Authorization**

```lisp
(defun handle-auth0-login (env)
  "Redirect user to Auth0 login page"
  (let* ((state (generate-state-token))
         (authorize-url (auth0-authorize-url state)))

    ;; Store state in session for CSRF protection
    (setf (gethash state *pending-oauth-states*) t)

    ;; Set expiry (5 minutes)
    (bt:make-thread
     (lambda ()
       (sleep 300)
       (remhash state *pending-oauth-states*)))

    ;; Redirect to Auth0
    (values 302
            `(:location ,authorize-url)
            nil)))
```

**Step 2: Handle Callback**

```lisp
(defun handle-auth0-callback (env)
  "Handle OAuth2 callback from Auth0"
  (let* ((params (quri:url-decode-params (getf env :query-string)))
         (code (cdr (assoc "code" params :test #'string=)))
         (state (cdr (assoc "state" params :test #'string=)))
         (error-param (cdr (assoc "error" params :test #'string=))))

    ;; Check for error
    (when error-param
      (return-from handle-auth0-callback
        (error-response (format nil "Auth0 error: ~A" error-param))))

    ;; Validate state (CSRF protection)
    (unless (gethash state *pending-oauth-states*)
      (return-from handle-auth0-callback
        (error-response "Invalid state parameter" :status 403)))

    ;; Exchange code for tokens
    (handler-case
        (let* ((tokens (exchange-code-for-tokens code))
               (id-token (getf tokens :id-token))
               (access-token (getf tokens :access-token))
               (user-info (decode-id-token id-token)))

          ;; Create or update user
          (let ((user (find-or-create-user-from-oauth user-info)))

            ;; Create session
            (let ((session-id (create-session-for-user (getf user :id))))

              ;; Set session cookie
              (set-session-cookie session-id)

              ;; Remove state from pending
              (remhash state *pending-oauth-states*)

              ;; Redirect to app
              (values 302
                      `(:location "/")
                      nil))))

      (error (e)
        (log-error "OAuth callback error: ~A" e)
        (error-response (format nil "Authentication failed: ~A" e))))))
```

**Step 3: Exchange Code for Tokens**

```lisp
(defun exchange-code-for-tokens (code)
  "Exchange authorization code for access and ID tokens"
  (let* ((url (auth0-token-url))
         (body (quri:url-encode-params
                `(("grant_type" . "authorization_code")
                  ("code" . ,code)
                  ("redirect_uri" . ,*auth0-callback-url*)
                  ("client_id" . ,*auth0-client-id*)
                  ("client_secret" . ,*auth0-client-secret*))))
         (headers '(("content-type" . "application/x-www-form-urlencoded"))))

    (multiple-value-bind (response status)
        (dex:post url
                  :headers headers
                  :content body
                  :timeout 10)

      (unless (= status 200)
        (error "Token exchange failed: ~A" response))

      (let ((data (parse-json response)))
        `(:access-token ,(getf data :access-token)
          :id-token ,(getf data :id-token)
          :expires-in ,(getf data :expires-in))))))
```

**Step 4: Decode and Validate JWT**

```lisp
(defun decode-id-token (id-token)
  "Decode and validate JWT ID token"
  ;; Split JWT into parts
  (let* ((parts (cl-ppcre:split "\\." id-token))
         (header-b64 (first parts))
         (payload-b64 (second parts))
         (signature-b64 (third parts)))

    ;; Decode header and payload (base64url)
    (let* ((header (parse-json (base64url-decode header-b64)))
           (payload (parse-json (base64url-decode payload-b64))))

      ;; Validate signature (using RS256 - fetch JWKS from Auth0)
      (unless (validate-jwt-signature id-token)
        (error "Invalid JWT signature"))

      ;; Validate claims
      (let ((iss (getf payload :iss))
            (aud (getf payload :aud))
            (exp (getf payload :exp)))

        ;; Check issuer
        (unless (string= iss (format nil "https://~A/" *auth0-domain*))
          (error "Invalid issuer"))

        ;; Check audience
        (unless (string= aud *auth0-client-id*)
          (error "Invalid audience"))

        ;; Check expiration
        (when (> (get-universal-time) exp)
          (error "Token expired"))

        ;; Return user claims
        `(:sub ,(getf payload :sub)
          :email ,(getf payload :email)
          :email-verified ,(getf payload :email-verified)
          :name ,(getf payload :name)
          :picture ,(getf payload :picture)
          :updated-at ,(getf payload :updated-at))))))

(defun validate-jwt-signature (jwt)
  "Validate JWT signature using Auth0's public key"
  ;; Fetch JWKS (JSON Web Key Set) from Auth0
  (let ((jwks (get-auth0-jwks)))
    ;; Extract key ID from JWT header
    (let* ((header-b64 (first (cl-ppcre:split "\\." jwt)))
           (header (parse-json (base64url-decode header-b64)))
           (kid (getf header :kid)))

      ;; Find matching key in JWKS
      (let ((key (find-jwks-key jwks kid)))
        (unless key
          (error "Key ID not found in JWKS"))

        ;; Verify signature using public key (RS256)
        (verify-rsa-signature jwt key)))))

(defparameter *jwks-cache* nil
  "Cached JWKS (refreshed every 24 hours)")

(defun get-auth0-jwks ()
  "Fetch JWKS from Auth0 (with caching)"
  (or *jwks-cache*
      (let ((url (format nil "https://~A/.well-known/jwks.json" *auth0-domain*)))
        (multiple-value-bind (response status)
            (dex:get url :timeout 10)
          (unless (= status 200)
            (error "Failed to fetch JWKS"))

          (let ((jwks (parse-json response)))
            (setf *jwks-cache* jwks)

            ;; Expire cache after 24 hours
            (bt:make-thread
             (lambda ()
               (sleep 86400)
               (setf *jwks-cache* nil)))

            jwks)))))
```

**Step 5: Find or Create User**

```lisp
(defun find-or-create-user-from-oauth (oauth-user-info)
  "Find existing user or create new one from OAuth data"
  (let ((auth0-sub (getf oauth-user-info :sub))
        (email (getf oauth-user-info :email))
        (name (getf oauth-user-info :name))
        (picture (getf oauth-user-info :picture))
        (email-verified (getf oauth-user-info :email-verified)))

    ;; Check if user exists by auth0_sub
    (let ((existing-user (get-user-by-auth0-sub auth0-sub)))
      (if existing-user
          ;; Update user info
          (progn
            (update-user-from-oauth existing-user oauth-user-info)
            existing-user)

          ;; Create new user
          (progn
            ;; Check if email already exists (legacy user)
            (let ((legacy-user (get-user-by-email email)))
              (if legacy-user
                  ;; Link Auth0 account to existing user
                  (progn
                    (link-auth0-to-user legacy-user auth0-sub)
                    legacy-user)

                  ;; Create brand new user
                  (create-user-from-oauth auth0-sub email name picture))))))))

(defun create-user-from-oauth (auth0-sub email name picture)
  "Create new user from OAuth data"
  (with-db-transaction ()
    (execute-db
     "INSERT INTO users (email, username, auth0_sub, display_name, avatar_url, created_at)
      VALUES (?, ?, ?, ?, ?, datetime('now'))"
     email
     (generate-username-from-email email)
     auth0-sub
     name
     picture)

    (let ((user-id (sqlite:last-insert-rowid *db*)))
      `((:id . ,user-id)
        (:email . ,email)
        (:username . ,(generate-username-from-email email))
        (:auth0-sub . ,auth0-sub)
        (:display-name . ,name)
        (:avatar-url . ,picture)))))
```

#### 3. Database Schema Changes

```sql
-- Add Auth0 fields to users table
ALTER TABLE users ADD COLUMN auth0_sub TEXT UNIQUE;
ALTER TABLE users ADD COLUMN display_name TEXT;
ALTER TABLE users ADD COLUMN avatar_url TEXT;
ALTER TABLE users ADD COLUMN email_verified BOOLEAN DEFAULT 0;
ALTER TABLE users ADD COLUMN last_login_at TEXT;

-- Make password_hash nullable (for OAuth-only users)
-- Cannot use ALTER COLUMN in SQLite, so need to recreate table:
CREATE TABLE users_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT UNIQUE NOT NULL,
    username TEXT UNIQUE NOT NULL,
    password_hash TEXT,  -- Nullable now
    auth0_sub TEXT UNIQUE,  -- Auth0 subject identifier
    display_name TEXT,
    avatar_url TEXT,
    email_verified BOOLEAN DEFAULT 0,
    last_login_at TEXT,
    created_at TEXT DEFAULT (datetime('now')),
    updated_at TEXT DEFAULT (datetime('now'))
);

-- Copy existing data
INSERT INTO users_new (id, email, username, password_hash, created_at, updated_at)
SELECT id, email, username, password_hash, created_at, updated_at
FROM users;

-- Drop old table and rename new one
DROP TABLE users;
ALTER TABLE users_new RENAME TO users;

-- Recreate indexes
CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);
CREATE INDEX IF NOT EXISTS idx_users_username ON users(username);
CREATE INDEX IF NOT EXISTS idx_users_auth0_sub ON users(auth0_sub);
```

---

## Frontend Integration

### Login Flow

```javascript
// frontend/src/auth.js

class AuthManager {
  constructor() {
    this.authMode = 'auth0'; // or 'legacy' during transition
  }

  // Auth0 login
  loginWithAuth0() {
    // Redirect to backend OAuth endpoint
    window.location.href = '/auth/auth0';
  }

  // Social login buttons
  loginWithGoogle() {
    // Auth0 handles social login
    window.location.href = '/auth/auth0?connection=google-oauth2';
  }

  loginWithGithub() {
    window.location.href = '/auth/auth0?connection=github';
  }

  // Legacy login (during transition)
  async loginWithPassword(email, password) {
    const response = await fetch('/api/login', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ email, password })
    });

    if (!response.ok) {
      throw new Error('Login failed');
    }

    const data = await response.json();
    return data;
  }

  // Logout (works for both Auth0 and legacy)
  logout() {
    if (this.authMode === 'auth0') {
      // Auth0 logout (clears Auth0 session + redirects)
      window.location.href = '/auth/auth0/logout';
    } else {
      // Legacy logout
      fetch('/api/logout', { method: 'POST' })
        .then(() => {
          window.location.href = '/login';
        });
    }
  }

  // Check auth status
  async checkAuth() {
    const response = await fetch('/api/session');
    if (response.ok) {
      const data = await response.json();
      return data;
    }
    return null;
  }
}
```

### UI Updates

```html
<!-- Login page with Auth0 -->
<div class="login-container">
  <h2>Sign in to CollabCanvas</h2>

  <!-- Auth0 universal login -->
  <button onclick="auth.loginWithAuth0()" class="btn-primary">
    Continue with Auth0
  </button>

  <!-- Social login shortcuts -->
  <div class="social-login">
    <button onclick="auth.loginWithGoogle()" class="btn-social">
      <img src="/icons/google.svg" alt="Google">
      Continue with Google
    </button>

    <button onclick="auth.loginWithGithub()" class="btn-social">
      <img src="/icons/github.svg" alt="GitHub">
      Continue with GitHub
    </button>
  </div>

  <!-- Legacy login (during transition) -->
  <div class="legacy-login">
    <a href="/login/legacy">Sign in with email/password</a>
  </div>
</div>
```

---

## Migration Strategy

### Phase 1: Dual Mode (Weeks 1-2)

**Goal:** Add Auth0 alongside existing auth, no user disruption

1. Add Auth0 configuration
2. Implement OAuth2 flow handlers
3. Update database schema (add auth0_sub column)
4. Add "Login with Auth0" button
5. Test with new accounts

**User Experience:**
- Existing users: Continue using email/password
- New users: Encouraged to use Auth0
- No forced migration

### Phase 2: User Migration (Weeks 3-4)

**Goal:** Encourage existing users to link Auth0 accounts

1. Show banner: "Upgrade to secure Auth0 login"
2. One-click linking: User logs in with password, then clicks "Link Auth0"
3. Backend: Updates `auth0_sub` for existing user
4. Email campaign: "We've upgraded our security"

**Migration Flow:**
```lisp
(defun handle-link-auth0-account (env)
  "Link Auth0 account to existing user (must be logged in)"
  ;; Validate user is logged in with legacy auth
  (let ((session (require-auth)))
    (unless session
      (return-from handle-link-auth0-account
        (error-response "Must be logged in" :status 401)))

    ;; Start OAuth flow with special state
    (let ((state (generate-state-token `(:link-user-id ,(getf session :user-id)))))
      (redirect-to (auth0-authorize-url state)))))
```

### Phase 3: Deprecation (Weeks 5-6)

**Goal:** Remove legacy auth code

1. Disable password login for new users
2. Show warnings for users without Auth0
3. After 90% migration: Force remaining users to link
4. Remove password_hash handling code
5. Remove legacy endpoints

---

## Testing Strategy

### Unit Tests

```lisp
(deftest test-auth0-authorize-url
  (let ((url (auth0-authorize-url "test-state")))
    (ok (search "response_type=code" url))
    (ok (search "state=test-state" url))
    (ok (search *auth0-client-id* url))))

(deftest test-jwt-validation
  ;; Mock valid JWT
  (with-mocked-jwt-validation
    (let ((user-info (decode-id-token *test-jwt*)))
      (ok (getf user-info :sub))
      (ok (getf user-info :email)))))

(deftest test-find-or-create-user
  (let ((oauth-info '(:sub "auth0|123456"
                      :email "test@example.com"
                      :name "Test User")))
    ;; First call creates user
    (let ((user1 (find-or-create-user-from-oauth oauth-info)))
      (ok (getf user1 :id))

      ;; Second call returns same user
      (let ((user2 (find-or-create-user-from-oauth oauth-info)))
        (ok (= (getf user1 :id) (getf user2 :id)))))))
```

### Integration Tests

**Test: Full OAuth Flow**
```bash
# 1. Start server
./start-server.sh

# 2. Initiate OAuth
curl -i http://localhost:8080/auth/auth0
# → Should return 302 redirect to Auth0

# 3. Mock Auth0 callback (in test environment)
curl -i "http://localhost:8080/auth/callback?code=TEST_CODE&state=TEST_STATE"
# → Should return 302 redirect to / with session cookie

# 4. Verify session
curl -b session.cookie http://localhost:8080/api/session
# → Should return user info
```

**Test: Social Login**
1. Click "Login with Google" in browser
2. Complete Google OAuth
3. Verify redirected back to app
4. Check user created in database
5. Verify session works

### Manual Testing Checklist

- [ ] New user signup via Auth0
- [ ] Login with Google
- [ ] Login with GitHub
- [ ] Existing user links Auth0 account
- [ ] Logout clears Auth0 session
- [ ] Session expires correctly
- [ ] CSRF protection works (invalid state rejected)
- [ ] JWT signature validation works
- [ ] Expired token rejected

---

## Security Considerations

### CSRF Protection

**State Parameter:**
- Generate cryptographically random state token
- Store in memory with 5-minute expiry
- Validate on callback
- Prevents CSRF attacks on OAuth flow

```lisp
(defun generate-state-token (&optional metadata)
  "Generate secure state token"
  (let ((random-bytes (ironclad:make-random-salt 32)))
    (let ((state (ironclad:byte-array-to-hex-string random-bytes)))
      ;; Store with metadata
      (setf (gethash state *pending-oauth-states*)
            `(:created-at ,(get-universal-time)
              :metadata ,metadata))
      state)))
```

### Token Storage

**Access Tokens:** Never store in localStorage (XSS risk)
**ID Tokens:** Validate immediately, extract claims, discard
**Refresh Tokens:** Not needed (session-based approach)

**Session Cookies:**
```lisp
(defun set-session-cookie (session-id)
  "Set secure session cookie"
  (hunchentoot:set-cookie "session_id"
                         :value session-id
                         :path "/"
                         :http-only t       ; Not accessible to JavaScript
                         :secure t          ; HTTPS only
                         :same-site :strict ; CSRF protection
                         :max-age 86400))   ; 24 hours
```

### Rate Limiting

**OAuth endpoints:**
- Max 10 login attempts per IP per hour
- Max 5 account links per user per day
- Exponential backoff on failures

---

## Monitoring & Alerting

### Key Metrics

```lisp
(defun get-auth-metrics ()
  "Get authentication metrics"
  `(:total-users ,(count-total-users)
    :auth0-users ,(count-auth0-users)
    :legacy-users ,(count-legacy-users)
    :logins-today ,(count-logins-today)
    :failed-logins-today ,(count-failed-logins-today)
    :oauth-errors-today ,(count-oauth-errors-today)))
```

### Alerts

- ⚠️ OAuth error rate >5% (Auth0 API issues)
- 🔴 Failed login rate >20% (possible attack)
- ⚠️ JWT validation failures (possible token tampering)
- 🔴 Auth0 API timeout (service down)

---

## Costs

### Auth0 Pricing

**Free Tier:**
- 7,500 active users/month
- Social login included
- Email/password authentication
- **Cost: $0**

**Professional Tier (if needed):**
- Unlimited active users
- $240/month base + $35 per 1,000 external active users
- **Estimated: $240-500/month for 10K users**

**Trade-offs:**
- Free tier sufficient for MVP and early growth
- Upgrade when >7,500 monthly active users
- Cost replaces security engineering time ($10K+/year)

---

## Success Criteria

### Must Have (MVP)
- ✅ New users can sign up via Auth0
- ✅ Social login (Google, GitHub) works
- ✅ JWT validation correctly implemented
- ✅ Existing users can link Auth0 accounts
- ✅ Sessions work identically to legacy auth

### Nice to Have (Post-MVP)
- MFA support (authenticator apps)
- Passwordless login (magic links)
- Enterprise SSO (SAML)
- Custom branding (Auth0 login page)
- Analytics dashboard (login sources, conversion)

---

## Rollback Plan

If Auth0 integration has critical issues:

1. **Immediate:** Set feature flag `*auth0-enabled*` to `nil`
2. **Database:** No rollback needed (auth0_sub column nullable)
3. **Users:** Legacy auth still works
4. **Monitoring:** Alert if Auth0 usage drops to 0

---

**Document Version:** 1.0
**Last Reviewed:** October 2025
**Owner:** Backend/Security Team
