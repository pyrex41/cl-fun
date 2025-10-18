// main.js - Application entry point for CollabCanvas
// MODIFIED FOR PHYSICS DEBUGGING - Simplified version with auth bypass

import './styles.css'
import * as PIXI from 'pixi.js'
import { SimplePhysicsCanvas } from './simple-physics-canvas.js' // Using simplified canvas
import { PhysicsUI } from './physics-ui.js'
import { WebSocketClient } from './websocket.js'
import { AuthManager } from './auth.js'

class CollabCanvas {
    constructor() {
        this.canvasManager = null
        this.wsClient = null
        this.authManager = null
        this.physicsUI = null
        this.sessionId = null
        this.userId = null
        this.username = null
        this.canvasId = this.getCanvasId()
        this.activeUsers = [] // Track active users
    }

    getCookie(name) {
        const value = `; ${document.cookie}`;
        const parts = value.split(`; ${name}=`);
        if (parts.length === 2) {
            return parts.pop().split(';').shift();
        }
        return null;
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
        console.log('[INIT] ==================== PHYSICS DEBUG MODE ====================')
        console.log('[INIT] Authentication BYPASSED for debugging')
        console.log('[INIT] Using simplified physics canvas')

        // Update canvas ID in status bar
        document.getElementById('canvas-id').textContent = this.canvasId

        // ========== BYPASS AUTHENTICATION ==========
        // Use hardcoded test credentials
        this.sessionId = 'test-session-' + Date.now()
        this.userId = 'test-user-' + Math.random().toString(36).substring(7)
        this.username = 'TestUser-' + this.userId.slice(-4)
        
        console.log('[INIT] Test credentials:', {
            sessionId: this.sessionId,
            userId: this.userId,
            username: this.username,
            canvasId: this.canvasId
        })

        // Hide loading screen immediately
        this.hideLoadingScreen()
        console.log('[INIT] Loading screen hidden')

        // Initialize simplified canvas
        console.log('[INIT] Initializing simple physics canvas...')
        await this.initSimpleCanvas()

        // Initialize WebSocket connection
        console.log('[INIT] Initializing WebSocket...')
        this.initWebSocket()

        // Setup minimal UI handlers (skip most tool handlers)
        console.log('[INIT] Setting up UI handlers...')
        this.setupUIHandlers()

        console.log('[INIT] ==================== INITIALIZATION COMPLETE ====================')
        console.log('[INIT] You can now:')
        console.log('[INIT]   1. Click "Spawn Ball" button to spawn physics balls')
        console.log('[INIT]   2. Use middle mouse button to pan')
        console.log('[INIT]   3. Use mouse wheel to zoom')
        console.log('[INIT]   4. Open console and use: window.debugPhysics')
    }

    hideLoadingScreen() {
        const loadingScreen = document.getElementById('loading-screen')
        if (loadingScreen) {
            loadingScreen.classList.add('hidden')
        }
    }

    async validateSession() {
        console.log('[VALIDATE] Validating session...')
        console.log('[VALIDATE] sessionId from localStorage:', this.sessionId || 'NULL')

        try {
            const headers = { }
            if (this.sessionId) {
                headers['X-Session-ID'] = this.sessionId
            }

            const response = await fetch('/api/session', {
                credentials: 'include',
                headers: headers
            })

            console.log('[VALIDATE] Response status:', response.status)
            console.log('[VALIDATE] Response ok:', response.ok)

            if (response.ok) {
                const data = await response.json()
                console.log('[VALIDATE] Response data:', data)

                // Backend returns uppercase keys (SUCCESS, DATA, VALID)
                const responseData = data.DATA || data.data
                const success = data.SUCCESS !== undefined ? data.SUCCESS : data.success

                if (success && responseData && (responseData.VALID || responseData.valid)) {
                    // Extract session info from backend
                    this.userId = responseData['USER-ID'] || responseData['user-id']
                    this.username = responseData.USERNAME || responseData.username
                    // Extract session ID from response (backend should provide this)
                    const sessionId = responseData['SESSION-ID'] || responseData['session-id']
                    if (sessionId) {
                        this.sessionId = sessionId
                        console.log('[VALIDATE] Session ID from backend:', this.sessionId)
                    }
                    // Extract preferred color and save it
                    const preferredColor = responseData['PREFERRED-COLOR'] || responseData['preferred-color']
                    if (preferredColor) {
                        this.preferredColor = preferredColor
                        console.log('[VALIDATE] Preferred color from backend:', this.preferredColor)
                    }
                    console.log('[VALIDATE] Session restored:', this.username)
                    return true
                } else {
                    console.log('[VALIDATE] Session validation failed - success:', success, 'responseData:', responseData, 'responseData.valid:', responseData?.valid, 'responseData.VALID:', responseData?.VALID)
                }
            } else {
                const errorData = await response.json()
                console.log('[VALIDATE] Error response:', errorData)
            }
        } catch (error) {
            console.error('[VALIDATE] Session validation exception:', error)
        }

        return false
    }

    async initSimpleCanvas() {
        console.log('[InitCanvas] Creating simple physics canvas...')
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
        console.log('[InitCanvas] PixiJS app created, canvas size:', app.screen.width, 'x', app.screen.height)

        // Create simplified physics canvas
        this.canvasManager = new SimplePhysicsCanvas(app)
        console.log('[InitCanvas] SimplePhysicsCanvas created')

        // Set up ONLY physics callback
        this.canvasManager.onPhysicsDelta = (data) => {
            console.log('[Canvas] onPhysicsDelta callback triggered, entities:', data.entities?.length || 0)
        }

        // Setup global debug commands
        console.log('[InitCanvas] Setting up global debug commands...')
        window.debugPhysics = {
            // List all balls
            listBalls: () => {
                console.log('=== Active Balls ===')
                console.log(`Total: ${this.canvasManager.balls.size}`)
                this.canvasManager.balls.forEach((ball, id) => {
                    console.log(`${id}:`, {
                        current: [ball.currentX.toFixed(1), ball.currentY.toFixed(1)],
                        server: [ball.serverX.toFixed(1), ball.serverY.toFixed(1)],
                        radius: ball.radius
                    })
                })
                return this.canvasManager.balls
            },
            
            // Get canvas manager
            canvas: () => this.canvasManager,
            
            // Clear all balls
            clearAll: () => {
                console.log('[Debug] Clearing all balls...')
                this.canvasManager.clearAllBalls()
            },
            
            // Spawn test ball at center
            spawnTest: () => {
                console.log('[Debug] Spawning test ball at canvas center')
                const centerX = this.canvasManager.app.screen.width / 2
                const centerY = this.canvasManager.app.screen.height / 2
                this.canvasManager.createBall('test-ball-' + Date.now(), centerX, centerY, 30, 0xFF0000)
            },
            
            // Get WebSocket client
            ws: () => this.wsClient,
            
            // Get viewport info
            viewport: () => ({
                x: this.canvasManager.viewportX,
                y: this.canvasManager.viewportY,
                zoom: this.canvasManager.zoom,
                childrenCount: this.canvasManager.viewport.children.length
            })
        }

        console.log('[InitCanvas] ✓ Simple canvas initialized')
        console.log('[InitCanvas] Debug commands available at: window.debugPhysics')
    }

    initWebSocket() {
        console.log('[WebSocket] Initializing WebSocket connection...')
        
        // Use wss:// for HTTPS pages, ws:// for HTTP
        const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:'
        const wsUrl = `${wsProtocol}//${window.location.host}/ws/${this.canvasId}`
        
        console.log('[WebSocket] URL:', wsUrl)
        console.log('[WebSocket] Session ID:', this.sessionId)
        console.log('[WebSocket] Canvas ID:', this.canvasId)

        this.wsClient = new WebSocketClient(wsUrl, this.sessionId, this.canvasId)

        // ========== SIMPLIFIED CALLBACKS - PHYSICS ONLY ==========
        
        this.wsClient.onAuthSuccess = (data) => {
            console.log('[WebSocket] ✓ Authentication successful')
            console.log('[WebSocket] Auth data:', data)
            // Skip loading canvas state - we only care about physics
        }

        this.wsClient.onError = (error) => {
            console.error('[WebSocket] ✗ Error:', error)
        }

        this.wsClient.onReconnecting = () => {
            console.warn('[WebSocket] ⟳ Reconnecting...')
        }

        this.wsClient.onReconnected = () => {
            console.log('[WebSocket] ✓ Reconnected')
        }

        // ========== PHYSICS MESSAGE HANDLER ==========
        this.wsClient.onPhysicsDelta = (data) => {
            console.log('[WebSocket] ← physics-delta received, entities:', data?.entities?.length || 0)
            
            // Log first message in detail
            if (!this._firstPhysicsDeltaLogged) {
                console.log('[WebSocket] First physics-delta message (full):', JSON.stringify(data, null, 2))
                this._firstPhysicsDeltaLogged = true
            }
            
            if (this.canvasManager && this.canvasManager.handlePhysicsDelta) {
                this.canvasManager.handlePhysicsDelta(data)
            } else {
                console.error('[WebSocket] ✗ canvasManager.handlePhysicsDelta not available')
            }
        }

        // Connect to WebSocket
        console.log('[WebSocket] Connecting...')
        this.wsClient.connect()

        // Initialize Physics UI controls
        console.log('[WebSocket] Initializing Physics UI...')
        this.physicsUI = new PhysicsUI(this.wsClient, this.canvasManager)
        console.log('[WebSocket] ✓ Physics UI initialized')
    }

    setupUIHandlers() {
        console.log('[UI] Setting up UI handlers (simplified for physics debug)...')
        
        // Most UI handlers are disabled in debug mode
        // Physics UI (spawn button, gravity slider) is handled by PhysicsUI class
        
        // Keyboard shortcuts - minimal set
        document.addEventListener('keydown', (e) => {
            // Prevent shortcuts when typing in inputs
            if (e.target.tagName === 'INPUT') return

            switch(e.key.toLowerCase()) {
                case 'p':
                    if (e.ctrlKey && e.shiftKey) {
                        e.preventDefault()
                        console.log('[Keyboard] Performance test not available in debug mode')
                    }
                    break
            }
        })

        console.log('[UI] ✓ UI handlers ready (minimal set)')
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