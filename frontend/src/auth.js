// auth.js - Authentication UI and logic for CollabCanvas

export class AuthManager {
    constructor() {
        this.modal = document.getElementById('auth-modal')
        this.loginForm = document.getElementById('login-form')
        this.registerForm = document.getElementById('register-form')
        this.auth0Domain = import.meta.env.VITE_AUTH0_DOMAIN
        this.auth0ClientId = import.meta.env.VITE_AUTH0_CLIENT_ID
        this.setupEventListeners()
        this.checkAuthCallback()
    }

    setupEventListeners() {
        // Tab switching
        document.querySelectorAll('.tab-btn').forEach(btn => {
            btn.addEventListener('click', () => {
                const tab = btn.dataset.tab
                this.switchTab(tab)
            })
        })

        // Form submissions
        this.loginForm.addEventListener('submit', async (e) => {
            e.preventDefault()
            await this.handleLogin()
        })

        this.registerForm.addEventListener('submit', async (e) => {
            e.preventDefault()
            await this.handleRegister()
        })

        // OAuth buttons
        const auth0LoginBtn = document.getElementById('auth0-login-btn')
        if (auth0LoginBtn) {
            auth0LoginBtn.addEventListener('click', () => this.loginWithAuth0())
        }

        const googleLoginBtn = document.getElementById('google-login-btn')
        if (googleLoginBtn) {
            googleLoginBtn.addEventListener('click', () => this.loginWithGoogle())
        }

        const githubLoginBtn = document.getElementById('github-login-btn')
        if (githubLoginBtn) {
            githubLoginBtn.addEventListener('click', () => this.loginWithGithub())
        }
    }

    switchTab(tab) {
        // Update tab buttons
        document.querySelectorAll('.tab-btn').forEach(btn => {
            btn.classList.toggle('active', btn.dataset.tab === tab)
        })

        // Update forms
        document.querySelectorAll('.auth-form').forEach(form => {
            form.classList.toggle('active', form.id === `${tab}-form`)
        })

        // Clear errors
        document.querySelectorAll('.error-message').forEach(err => {
            err.textContent = ''
        })
    }

    showModal() {
        return new Promise((resolve) => {
            this.modal.classList.remove('hidden')
            this.resolveAuth = resolve
        })
    }

    hideModal() {
        this.modal.classList.add('hidden')
    }

    showError(message, formId = 'login') {
        const errorEl = document.getElementById(`${formId}-error`)
        if (errorEl) {
            errorEl.textContent = message
            errorEl.style.display = 'block'
        }
    }

    async loginWithAuth0() {
        try {
            // Check backend health before redirecting
            const response = await fetch('/auth0/login', { method: 'HEAD' })
            if (!response.ok && response.status !== 405) {
                // 405 Method Not Allowed is expected since we're doing HEAD
                throw new Error('Authentication service unavailable')
            }
            // Redirect to backend OAuth initiation
            window.location.href = '/auth0/login'
        } catch (error) {
            console.error('Auth0 login error:', error)
            this.showError('Could not connect to authentication service. Please try again.')
        }
    }

    async loginWithGoogle() {
        try {
            // Check backend health before redirecting
            const response = await fetch('/auth0/login?connection=google-oauth2', { method: 'HEAD' })
            if (!response.ok && response.status !== 405) {
                throw new Error('Authentication service unavailable')
            }
            // Redirect to backend OAuth with Google connection
            window.location.href = '/auth0/login?connection=google-oauth2'
        } catch (error) {
            console.error('Google login error:', error)
            this.showError('Could not connect to Google authentication. Please try again.')
        }
    }

    async loginWithGithub() {
        try {
            // Check backend health before redirecting
            const response = await fetch('/auth0/login?connection=github', { method: 'HEAD' })
            if (!response.ok && response.status !== 405) {
                throw new Error('Authentication service unavailable')
            }
            // Redirect to backend OAuth with GitHub connection
            window.location.href = '/auth0/login?connection=github'
        } catch (error) {
            console.error('GitHub login error:', error)
            this.showError('Could not connect to GitHub authentication. Please try again.')
        }
    }

    checkAuthCallback() {
        // Check if returning from Auth0 callback
        const url = new URL(window.location)
        const code = url.searchParams.get('code')
        const error = url.searchParams.get('error')
        const errorDescription = url.searchParams.get('error_description')

        if (error) {
            // OAuth error returned from Auth0
            console.error('OAuth error:', error, errorDescription)

            // Show user-friendly error message
            let message = 'Authentication failed. '
            if (error === 'access_denied') {
                message += 'Access was denied. Please try again.'
            } else if (errorDescription) {
                message += errorDescription
            } else {
                message += 'Please try again or contact support.'
            }

            this.showError(message)

            // Clean URL
            window.history.replaceState({}, document.title, window.location.pathname)
        } else if (code) {
            // Callback will be handled by backend redirect to /auth0/callback
            // which will set session cookie and redirect back
            console.log('Auth0 callback detected, processing...')
        }
    }

    async handleLogin() {
        const email = document.getElementById('login-email').value
        const password = document.getElementById('login-password').value
        const errorEl = document.getElementById('login-error')

        try {
            const response = await fetch('/api/login', {
                method: 'POST',
                credentials: 'include',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({ email, password })
            })

            const data = await response.json()

            if (data.success && data.data) {
                this.hideModal()
                this.resolveAuth({
                    sessionId: data.data['session-id'],
                    userId: data.data['user-id'],
                    username: data.data.username
                })
            } else {
                errorEl.textContent = data.error || 'Login failed'
            }
        } catch (error) {
            console.error('Login error:', error)
            errorEl.textContent = 'Connection error. Please try again.'
        }
    }

    async handleRegister() {
        const username = document.getElementById('register-username').value
        const email = document.getElementById('register-email').value
        const password = document.getElementById('register-password').value
        const errorEl = document.getElementById('register-error')

        try {
            const response = await fetch('/api/register', {
                method: 'POST',
                credentials: 'include',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({ username, email, password })
            })

            const data = await response.json()

            if (data.success) {
                // Auto-login after registration
                await this.loginAfterRegister(email, password)
            } else {
                errorEl.textContent = data.error || 'Registration failed'
            }
        } catch (error) {
            console.error('Registration error:', error)
            errorEl.textContent = 'Connection error. Please try again.'
        }
    }

    async loginAfterRegister(email, password) {
        // Switch to login tab
        this.switchTab('login')

        // Fill in credentials
        document.getElementById('login-email').value = email
        document.getElementById('login-password').value = password

        // Auto-submit
        await this.handleLogin()
    }
}