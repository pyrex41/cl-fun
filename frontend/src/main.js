// main.js - Application entry point for CollabCanvas

import './styles.css'
import * as PIXI from 'pixi.js'
import { CanvasManager } from './canvas.js'
import { WebSocketClient } from './websocket.js'
import { AuthManager } from './auth.js'

class CollabCanvas {
    constructor() {
        this.canvasManager = null
        this.wsClient = null
        this.authManager = null
        this.sessionId = null
        this.userId = null
        this.username = null
        this.canvasId = this.getCanvasId()
        this.activeUsers = [] // Track active users
    }

    getCanvasId() {
        // Get canvas ID from URL or use default shared canvas
        const params = new URLSearchParams(window.location.search)
        let canvasId = params.get('canvas')

        if (!canvasId) {
            // Use a fixed default canvas so all users join the same one
            canvasId = 'default-canvas'
            // Update URL without reload
            const newUrl = new URL(window.location)
            newUrl.searchParams.set('canvas', canvasId)
            window.history.replaceState({}, '', newUrl)
        }

        return canvasId
    }

    async init() {
        console.log('Initializing CollabCanvas...')

        // Update canvas ID in status bar
        document.getElementById('canvas-id').textContent = this.canvasId

        // Initialize authentication
        this.authManager = new AuthManager()

        // Check for existing session
        this.sessionId = localStorage.getItem('sessionId')

        if (this.sessionId) {
            // Validate session with backend
            const isValid = await this.validateSession()

            if (!isValid) {
                this.sessionId = null
                localStorage.removeItem('sessionId')
            }
        }

        if (!this.sessionId) {
            // Hide loading screen and show auth modal
            this.hideLoadingScreen()
            const authData = await this.authManager.showModal()
            this.sessionId = authData.sessionId
            this.userId = authData.userId
            this.username = authData.username
            localStorage.setItem('sessionId', this.sessionId)
        }

        // Hide loading screen (session is valid)
        this.hideLoadingScreen()

        // Initialize canvas (async in v8)
        await this.initCanvas()

        // Initialize WebSocket connection
        this.initWebSocket()

        // Setup UI event handlers
        this.setupUIHandlers()

        console.log('CollabCanvas initialized successfully')
    }

    hideLoadingScreen() {
        const loadingScreen = document.getElementById('loading-screen')
        if (loadingScreen) {
            loadingScreen.classList.add('hidden')
        }
    }

    async validateSession() {
        console.log('[FRONTEND] Validating session...')
        console.log('[FRONTEND] sessionId from localStorage:', this.sessionId)

        try {
            const response = await fetch('/api/session', {
                credentials: 'include',
                headers: {
                    'X-Session-ID': this.sessionId
                }
            })

            console.log('[FRONTEND] Response status:', response.status)
            console.log('[FRONTEND] Response ok:', response.ok)

            if (response.ok) {
                const data = await response.json()
                console.log('[FRONTEND] Response data:', data)

                if (data.success && data.data && data.data.valid) {
                    this.userId = data.data['user-id']
                    this.username = data.data.username
                    console.log('[FRONTEND] Session restored:', this.username)
                    return true
                } else {
                    console.log('[FRONTEND] Session validation failed - data.success:', data.success, 'data.data:', data.data, 'data.data.valid:', data.data?.valid)
                }
            } else {
                const errorData = await response.json()
                console.log('[FRONTEND] Error response:', errorData)
            }
        } catch (error) {
            console.error('[FRONTEND] Session validation exception:', error)
        }

        return false
    }

    async initCanvas() {
        const container = document.getElementById('canvas-container')

        // Get current theme for canvas background
        const currentTheme = document.documentElement.getAttribute('data-theme') || 'dark'
        const backgroundColor = currentTheme === 'dark' ? 0x16161f : 0xffffff

        // Create PixiJS application with v8 async initialization
        const app = new PIXI.Application()
        await app.init({
            width: window.innerWidth,
            height: window.innerHeight,
            backgroundColor: backgroundColor,
            resizeTo: window,
            antialias: true,
            autoDensity: true,
            resolution: window.devicePixelRatio || 1
        })

        container.appendChild(app.canvas)
        
        // Listen for theme changes and update canvas background
        const themeToggle = document.getElementById('theme-toggle')
        if (themeToggle) {
            themeToggle.addEventListener('click', () => {
                setTimeout(() => {
                    const newTheme = document.documentElement.getAttribute('data-theme')
                    const newBgColor = newTheme === 'dark' ? 0x16161f : 0xffffff
                    app.renderer.background.color = newBgColor
                }, 50)
            })
        }

        this.canvasManager = new CanvasManager(app)

        // Make performance stats available globally for console access
        window.getPerformanceStats = () => {
            return this.canvasManager.getPerformanceStats();
        };

        // Make latency stats available globally
        window.getLatencyStats = () => {
            if (!this.wsClient) return null;
            return this.wsClient.getLatencyStats();
        };

        window.logLatencyStats = () => {
            if (!this.wsClient) {
                console.warn('WebSocket client not initialized');
                return;
            }
            this.wsClient.logLatencyStats();
        };

        window.getLatencyStatsByType = (messageType) => {
            if (!this.wsClient) return null;
            return this.wsClient.getLatencyStatsByType(messageType);
        };

        // Set up canvas callbacks for WebSocket synchronization
        this.canvasManager.onCursorMoved = (x, y) => {
            if (this.wsClient && this.wsClient.isConnected) {
                this.wsClient.sendCursorUpdate(x, y)
            }
        }

        this.canvasManager.onObjectCreated = (object) => {
            if (this.wsClient && this.wsClient.isConnected) {
                this.wsClient.sendObjectCreate(object)
            }
        }

        this.canvasManager.onObjectUpdated = (objectId, updates) => {
            if (this.wsClient && this.wsClient.isConnected) {
                this.wsClient.sendObjectUpdate(objectId, updates)
            }
        }

        this.canvasManager.onObjectDeleted = (objectId) => {
            if (this.wsClient && this.wsClient.isConnected) {
                this.wsClient.sendObjectDelete(objectId)
            }
        }

        this.canvasManager.onObjectsDeleted = (objectIds) => {
            if (this.wsClient && this.wsClient.isConnected) {
                this.wsClient.sendObjectsDelete(objectIds)
            }
        }

        // Update status bar callbacks
        this.canvasManager.onToolChange = (tool) => {
            document.getElementById('current-tool').textContent =
                tool.charAt(0).toUpperCase() + tool.slice(1)
        }

        this.canvasManager.onMouseMove = (x, y) => {
            document.getElementById('mouse-position').textContent = `${Math.round(x)}, ${Math.round(y)}`
        }

        this.canvasManager.onZoomChange = (zoom) => {
            document.getElementById('zoom-level').textContent = `${Math.round(zoom * 100)}%`
        }

        this.canvasManager.onObjectCountChange = (count) => {
            document.getElementById('object-count').textContent = count
        }

        // Object selection callback for physics properties UI
        this.canvasManager.onSelectionChange = (selectedObjects) => {
            this.updatePhysicsPropertiesUI(selectedObjects)
        }

        // Start periodic memory cleanup (every 60 seconds)
        // This removes orphaned selection indicators and inactive cursors
        this.canvasManager.startPeriodicCleanup(60000)
        console.log('Started periodic memory cleanup (60s interval)')
    }

    initWebSocket() {
        // Use wss:// for HTTPS pages, ws:// for HTTP
        const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:'
        // window.location.host includes port if non-standard (e.g., localhost:8080)
        const wsUrl = `${wsProtocol}//${window.location.host}/ws/${this.canvasId}`

        this.wsClient = new WebSocketClient(wsUrl, this.sessionId, this.canvasId)

        // Set up WebSocket callbacks
        this.wsClient.onAuthSuccess = (data) => {
            console.error('=== WebSocket authenticated ===')
            console.error('Auth data received:', data)

            // Backend sends 'canvas-state' (kebab-case), not 'canvasState'
            const canvasState = data['canvas-state'] || data.canvasState
            console.error('canvasState exists?', !!canvasState)
            console.error('canvasState type:', typeof canvasState)
            console.error('canvasState length:', canvasState ? canvasState.length : 0)

            // Load initial canvas state
            if (canvasState) {
                console.error('=== CALLING loadState ===')
                this.canvasManager.loadState(canvasState)
                console.error('=== loadState RETURNED ===')
            } else {
                console.error('=== NO CANVAS STATE IN AUTH RESPONSE ===')
            }
        }

        this.wsClient.onUserConnected = (data) => {
            // Add new user to active users list
            this.activeUsers.push({
                'user-id': data.userId || data['user-id'],
                username: data.username,
                color: data.color
            })
            this.updatePresenceList(this.activeUsers)
            this.showNotification(`${data.username} joined`, 'info')
        }

        this.wsClient.onUserDisconnected = (data) => {
            // Remove user from active users list
            const userId = data.userId || data['user-id']
            this.activeUsers = this.activeUsers.filter(u =>
                (u['user-id'] || u.userId) !== userId
            )
            this.updatePresenceList(this.activeUsers)
            this.canvasManager.removeRemoteCursor(userId)
            this.showNotification(`${data.username} left`, 'info')
        }

        this.wsClient.onPresenceUpdate = (users) => {
            // Replace entire active users list with server's authoritative list
            this.activeUsers = users
            this.updatePresenceList(this.activeUsers)
        }

        this.wsClient.onCursorUpdate = (data) => {
            this.canvasManager.updateRemoteCursor(
                data.userId,
                data.username,
                data.x,
                data.y,
                data.color
            )
        }

        this.wsClient.onObjectCreated = (data) => {
            this.canvasManager.createRemoteObject(data.object)
        }

        this.wsClient.onObjectUpdated = (data) => {
            // Backend sends 'object-id' (kebab-case)
            const objectId = data['object-id'] || data.objectId
            this.canvasManager.updateRemoteObject(objectId, data.delta)
        }

        this.wsClient.onObjectDeleted = (data) => {
            // Remove user from active users list
            const userId = data.userId || data['user-id']
            this.activeUsers = this.activeUsers.filter(u =>
                (u['user-id'] || u.userId) !== userId
            )
            this.updatePresenceList(this.activeUsers)
            this.canvasManager.removeRemoteCursor(userId)
            this.showNotification(`${data.username} left`, 'info')
        }

        this.wsClient.onObjectsDeleted = (data) => {
            const objectIds = data['object-ids'] || data.objectIds || []
            console.log('Received bulk delete for objects:', objectIds)

            // Handle remote bulk deletion
            if (objectIds.length > 0) {
                objectIds.forEach(objectId => {
                    this.canvasManager.deleteObject(objectId)
                })
                console.log(`Processed remote bulk deletion of ${objectIds.length} objects`)
            }
        }

        // Physics snapshot handler (Subtask 5.2: Handle snapshot reception)
        this.wsClient.onPhysicsSnapshot = (snapshot) => {
            this.canvasManager.handlePhysicsSnapshot(snapshot)
        }

        // Physics state change handler (play/pause/reset notifications)
        this.wsClient.onPhysicsStateChange = (data) => {
            const action = data.action || data.state
            console.log(`Physics state changed: ${action}`)

            if (action === 'play' || action === 'playing') {
                this.canvasManager.enablePhysics()
                this.showNotification('Physics simulation started', 'info')
            } else if (action === 'pause' || action === 'paused') {
                this.canvasManager.disablePhysics()
                this.showNotification('Physics simulation paused', 'info')
            } else if (action === 'reset') {
                this.canvasManager.disablePhysics()
                this.canvasManager.physicsEngine.reset()
                this.showNotification('Physics simulation reset', 'info')
            } else if (action === 'set-gravity') {
                const gravity = data.value || data.gravity
                this.canvasManager.setGravity(gravity)
                this.showNotification(`Gravity set to ${gravity}`, 'info')
            }
        }

        this.wsClient.onError = (error) => {
            console.error('WebSocket error:', error)
            this.showNotification('Connection error', 'error')
        }

        this.wsClient.onReconnecting = () => {
            this.showNotification('Reconnecting...', 'warning')
        }

        this.wsClient.onReconnected = () => {
            this.showNotification('Reconnected', 'success')
        }

        // Connect to WebSocket
        this.wsClient.connect()
    }

    setupUIHandlers() {
        // Tool buttons
        document.querySelectorAll('.tool-btn').forEach(btn => {
            btn.addEventListener('click', () => {
                const tool = btn.dataset.tool
                this.canvasManager.setTool(tool)

                // Update active button
                document.querySelectorAll('.tool-btn').forEach(b =>
                    b.classList.remove('active'))
                btn.classList.add('active')
            })
        })

        // Color picker
        const colorPicker = document.getElementById('color-picker')
        colorPicker.addEventListener('change', (e) => {
            this.canvasManager.setColor(e.target.value)
        })

        // Physics Controls
        this.setupPhysicsControls()

        // Physics Properties UI
        this.setupPhysicsPropertiesUI()

        // Keyboard shortcuts
        document.addEventListener('keydown', (e) => {
            // Prevent shortcuts when typing in inputs
            if (e.target.tagName === 'INPUT') return

            switch(e.key.toLowerCase()) {
                case 'v':
                    this.selectTool('select')
                    break
                case 'r':
                    this.selectTool('rectangle')
                    break
                case 'c':
                    this.selectTool('circle')
                    break
                case 'p':
                    if (e.ctrlKey && e.shiftKey) {
                        e.preventDefault()
                        this.runPerformanceTest()
                    }
                    break
                case 'delete':
                case 'backspace':
                    if (!e.target.isContentEditable) {
                        e.preventDefault()
                        this.canvasManager.deleteSelected()
                    }
                    break
                case 'z':
                    if (e.ctrlKey || e.metaKey) {
                        e.preventDefault()
                        if (e.shiftKey) {
                            this.canvasManager.redo()
                        } else {
                            this.canvasManager.undo()
                        }
                    }
                    break
            }
        })

        // Logout button (if added to UI)
        const logoutBtn = document.getElementById('logout-btn')
        if (logoutBtn) {
            logoutBtn.addEventListener('click', async () => {
                await this.logout()
            })
        }
    }

    selectTool(tool) {
        this.canvasManager.setTool(tool)

        // Update UI
        document.querySelectorAll('.tool-btn').forEach(btn => {
            if (btn.dataset.tool === tool) {
                btn.classList.add('active')
            } else {
                btn.classList.remove('active')
            }
        })
    }

    setupPhysicsControls() {
        // Play button
        const playBtn = document.getElementById('play-btn')
        if (playBtn) {
            playBtn.addEventListener('click', () => {
                if (this.wsClient && this.wsClient.isConnected) {
                    this.wsClient.sendPhysicsControl('play')
                    console.log('Sent physics control: play')
                }
            })
        }

        // Pause button
        const pauseBtn = document.getElementById('pause-btn')
        if (pauseBtn) {
            pauseBtn.addEventListener('click', () => {
                if (this.wsClient && this.wsClient.isConnected) {
                    this.wsClient.sendPhysicsControl('pause')
                    console.log('Sent physics control: pause')
                }
            })
        }

        // Reset button
        const resetBtn = document.getElementById('reset-btn')
        if (resetBtn) {
            resetBtn.addEventListener('click', () => {
                if (this.wsClient && this.wsClient.isConnected) {
                    this.wsClient.sendPhysicsControl('reset')
                    console.log('Sent physics control: reset')
                }
            })
        }

        // Gravity slider
        const gravitySlider = document.getElementById('gravity-slider')
        const gravityValue = document.getElementById('gravity-value')
        if (gravitySlider && gravityValue) {
            gravitySlider.addEventListener('input', (e) => {
                const value = parseFloat(e.target.value)
                gravityValue.textContent = value.toFixed(1)
            })

            gravitySlider.addEventListener('change', (e) => {
                const value = parseFloat(e.target.value)
                if (this.wsClient && this.wsClient.isConnected) {
                    this.wsClient.sendPhysicsControl('set-gravity', value)
                    console.log('Sent physics control: set-gravity', value)
                }
            })
        }

        // Boundary selector
        const boundarySelector = document.getElementById('boundary-selector')
        if (boundarySelector) {
            boundarySelector.addEventListener('change', (e) => {
                const value = e.target.value
                if (this.wsClient && this.wsClient.isConnected) {
                    this.wsClient.sendPhysicsControl('set-boundary', value)
                    console.log('Sent physics control: set-boundary', value)
                }
            })
        }
    }

    updatePresenceList(users = []) {
        const container = document.getElementById('users-container')
        container.innerHTML = ''

        users.forEach(user => {
            const userItem = document.createElement('div')
            userItem.className = 'user-item'
            // Handle both kebab-case and camelCase keys
            const username = user.username
            const color = user.color
            userItem.innerHTML = `
                <span class="user-indicator" style="background-color: ${color}"></span>
                <span>${username}</span>
            `
            container.appendChild(userItem)
        })
    }

    setupPhysicsPropertiesUI() {
        // isDynamic checkbox
        const isDynamicCheckbox = document.getElementById('prop-isDynamic')
        if (isDynamicCheckbox) {
            isDynamicCheckbox.addEventListener('change', (e) => {
                this.updateSelectedObjectPhysicsProperty('isDynamic', e.target.checked)
            })
        }

        // Friction slider
        const frictionSlider = document.getElementById('prop-friction')
        const frictionValue = document.getElementById('prop-friction-value')
        if (frictionSlider && frictionValue) {
            frictionSlider.addEventListener('input', (e) => {
                frictionValue.textContent = parseFloat(e.target.value).toFixed(2)
            })
            frictionSlider.addEventListener('change', (e) => {
                const value = parseFloat(e.target.value)
                this.updateSelectedObjectPhysicsProperty('friction', value)
            })
        }

        // Restitution slider
        const restitutionSlider = document.getElementById('prop-restitution')
        const restitutionValue = document.getElementById('prop-restitution-value')
        if (restitutionSlider && restitutionValue) {
            restitutionSlider.addEventListener('input', (e) => {
                restitutionValue.textContent = parseFloat(e.target.value).toFixed(2)
            })
            restitutionSlider.addEventListener('change', (e) => {
                const value = parseFloat(e.target.value)
                this.updateSelectedObjectPhysicsProperty('restitution', value)
            })
        }
    }

    updatePhysicsPropertiesUI(selectedObjects) {
        const propertiesSection = document.getElementById('properties-section')
        const isDynamicCheckbox = document.getElementById('prop-isDynamic')
        const isDynamicHint = document.getElementById('prop-isDynamic-hint')
        const frictionSlider = document.getElementById('prop-friction')
        const frictionValue = document.getElementById('prop-friction-value')
        const restitutionSlider = document.getElementById('prop-restitution')
        const restitutionValue = document.getElementById('prop-restitution-value')

        // Show/hide properties panel based on selection
        if (selectedObjects.size === 0) {
            propertiesSection.style.display = 'none'
            return
        }

        // Show properties panel
        propertiesSection.style.display = 'block'

        // Get first selected object (for simplicity, only show properties for single selection)
        const firstObjectId = Array.from(selectedObjects)[0]
        const object = this.canvasManager.getObjectById(firstObjectId)

        if (!object) {
            propertiesSection.style.display = 'none'
            return
        }

        // Multi-selection: show but disable controls
        if (selectedObjects.size > 1) {
            isDynamicCheckbox.disabled = true
            frictionSlider.disabled = true
            restitutionSlider.disabled = true
            isDynamicHint.textContent = 'Select a single object to edit properties'
            return
        }

        // Single selection: enable controls and populate values
        frictionSlider.disabled = false
        restitutionSlider.disabled = false

        // Check if object is a rectangle (rectangles can't be dynamic per Task 3)
        const isRectangle = object.type === 'rectangle'

        if (isRectangle) {
            isDynamicCheckbox.disabled = true
            isDynamicCheckbox.checked = false
            isDynamicHint.textContent = 'Rectangles are always static (boundaries)'
        } else {
            isDynamicCheckbox.disabled = false
            isDynamicCheckbox.checked = object.isDynamic || false
            isDynamicHint.textContent = ''
        }

        // Update slider values
        const friction = object.friction !== undefined ? object.friction : 0.02
        const restitution = object.restitution !== undefined ? object.restitution : 0.7

        frictionSlider.value = friction
        frictionValue.textContent = friction.toFixed(2)

        restitutionSlider.value = restitution
        restitutionValue.textContent = restitution.toFixed(2)
    }

    updateSelectedObjectPhysicsProperty(propertyName, value) {
        // Only update if single object is selected
        if (this.canvasManager.selectedObjects.size !== 1) {
            console.warn('Cannot update physics properties for multiple objects')
            return
        }

        const objectId = Array.from(this.canvasManager.selectedObjects)[0]
        const object = this.canvasManager.getObjectById(objectId)

        if (!object) {
            console.warn('Selected object not found:', objectId)
            return
        }

        // Prevent setting isDynamic=true for rectangles
        if (propertyName === 'isDynamic' && value === true && object.type === 'rectangle') {
            console.warn('Rectangles cannot be dynamic (they are static boundaries)')
            return
        }

        // Update object locally
        object[propertyName] = value
        console.log(`Updated object ${objectId} ${propertyName} to ${value}`)

        // Send update via WebSocket
        if (this.wsClient && this.wsClient.isConnected) {
            const updates = {}
            updates[propertyName] = value
            this.wsClient.sendObjectUpdate(objectId, updates)
            console.log('Sent physics property update via WebSocket:', updates)
        }
    }

    showNotification(message, type = 'info') {
        // Simple notification system (can be enhanced with a library)
        console.log(`[${type.toUpperCase()}] ${message}`)

        // TODO: Implement visual notifications
    }

    async logout() {
        try {
            await fetch('/api/logout', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                    'Authorization': this.sessionId
                }
            })
        } catch (error) {
            console.error('Logout error:', error)
        }

        // Clean up
        localStorage.removeItem('sessionId')

        // Stop periodic cleanup timer
        if (this.canvasManager) {
            this.canvasManager.stopPeriodicCleanup()
        }

        // Disconnect WebSocket (also cleans up cursor throttle)
        if (this.wsClient) {
            this.wsClient.disconnect()
        }

        // Reload page to show login
        window.location.reload()
    }

    async runPerformanceTest() {
        console.log('Performance testing is not available in production build');
        console.log('Use development build (npm run dev) for performance testing');
    }
}

// Initialize application when DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => {
        const app = new CollabCanvas()
        app.init()
        window.collabCanvas = app // For debugging
    })
} else {
    const app = new CollabCanvas()
    app.init()
    window.collabCanvas = app // For debugging
}