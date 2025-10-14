// auth.js - Authentication UI and logic for CollabCanvas

export class AuthManager {
    constructor() {
        this.modal = document.getElementById('auth-modal')
        this.loginForm = document.getElementById('login-form')
        this.registerForm = document.getElementById('register-form')
        this.setupEventListeners()
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