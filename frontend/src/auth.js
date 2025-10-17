// auth.js - Auth0-only authentication for CollabCanvas

export class AuthManager {
    constructor() {
        this.modal = document.getElementById('auth-modal')
        this.auth0Domain = import.meta.env.VITE_AUTH0_DOMAIN
        this.auth0ClientId = import.meta.env.VITE_AUTH0_CLIENT_ID
        this.setupEventListeners()
        this.checkAuthCallback()
    }

    setupEventListeners() {
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

    showModal() {
        console.log('[AUTH] showModal() called')
        console.log('[AUTH] Modal element:', this.modal)
        console.log('[AUTH] Modal classes before:', this.modal?.classList.value)
        return new Promise((resolve) => {
            this.modal.classList.remove('hidden')
            console.log('[AUTH] Modal classes after remove hidden:', this.modal.classList.value)
            this.resolveAuth = resolve
        })
    }

    hideModal() {
        console.log('[AUTH] hideModal() called')
        console.log('[AUTH] Modal element:', this.modal)
        console.log('[AUTH] Modal classes before:', this.modal?.classList.value)
        this.modal.classList.add('hidden')
        console.log('[AUTH] Modal classes after add hidden:', this.modal.classList.value)
    }

    showError(message) {
        const errorEl = document.getElementById('auth-error')
        if (errorEl) {
            errorEl.textContent = message
            errorEl.style.display = 'block'
        }
    }

    async loginWithAuth0() {
        try {
            // Redirect to backend OAuth initiation
            window.location.href = '/auth0/login'
        } catch (error) {
            console.error('Auth0 login error:', error)
            this.showError('Could not connect to authentication service. Please try again.')
        }
    }

    async loginWithGoogle() {
        try {
            // Redirect to backend OAuth with Google connection
            window.location.href = '/auth0/login?connection=google-oauth2'
        } catch (error) {
            console.error('Google login error:', error)
            this.showError('Could not connect to Google authentication. Please try again.')
        }
    }

    async loginWithGithub() {
        try {
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
}
