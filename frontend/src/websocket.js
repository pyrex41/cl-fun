// websocket.js - WebSocket client for real-time synchronization

export class WebSocketClient {
    constructor(url, sessionId, canvasId) {
        this.url = url
        this.sessionId = sessionId
        this.canvasId = canvasId
        this.ws = null
        this.isConnected = false
        this.reconnectAttempts = 0
        this.maxReconnectAttempts = 5
        this.reconnectDelay = 1000

        // Improved cursor throttling
        this.lastCursorSend = 0
        this.cursorSendInterval = 16 // ~60 FPS
        this.pendingCursor = null
        this.cursorAnimationFrame = null

        // Callbacks
        this.onAuthSuccess = () => {}
        this.onAuthFailed = () => {}
        this.onUserConnected = () => {}
        this.onUserDisconnected = () => {}
        this.onPresenceUpdate = () => {}
        this.onCursorUpdate = () => {}
        this.onObjectCreated = () => {}
        this.onObjectUpdated = () => {}
        this.onObjectDeleted = () => {}
        this.onError = () => {}
        this.onReconnecting = () => {}
        this.onReconnected = () => {}
    }

    connect() {
        console.log(`Connecting to WebSocket: ${this.url}`)

        try {
            this.ws = new WebSocket(this.url)
            this.setupEventHandlers()
        } catch (error) {
            console.error('WebSocket connection error:', error)
            this.onError(error)
            this.scheduleReconnect()
        }
    }

    setupEventHandlers() {
        this.ws.onopen = () => {
            console.log('WebSocket connected')
            this.isConnected = true
            this.reconnectAttempts = 0

            // Send authentication message
            this.send({
                type: 'auth',
                sessionId: this.sessionId,
                canvasId: this.canvasId
            })

            if (this.reconnectAttempts > 0) {
                this.onReconnected()
            }
        }

        this.ws.onmessage = (event) => {
            try {
                const data = JSON.parse(event.data)
                this.handleMessage(data)
            } catch (error) {
                console.error('Error parsing WebSocket message:', error)
                this.onError(error)
            }
        }

        this.ws.onclose = (event) => {
            console.log('WebSocket disconnected:', event.code, event.reason)
            this.isConnected = false

            if (!event.wasClean) {
                this.scheduleReconnect()
            }
        }

        this.ws.onerror = (error) => {
            console.error('WebSocket error:', error)
            this.onError(error)
        }
    }

    handleMessage(data) {
        switch (data.type) {
            case 'auth-success':
                this.onAuthSuccess(data)
                break

            case 'auth-failed':
                this.onAuthFailed(data)
                break

            case 'user-connected':
                this.onUserConnected(data)
                break

            case 'user-disconnected':
                this.onUserDisconnected(data)
                break

            case 'presence':
                this.onPresenceUpdate(data.users)
                break

            case 'cursor':
                this.onCursorUpdate(data)
                break

            case 'object-create':
                this.onObjectCreated(data)
                break

            case 'object-update':
                this.onObjectUpdated(data)
                break

            case 'object-delete':
                this.onObjectDeleted(data)
                break

            case 'error':
                console.error('Server error:', data.message)
                this.onError(new Error(data.message))
                break

            default:
                console.warn('Unknown message type:', data.type)
        }
    }

    send(data) {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify(data))
        } else {
            console.warn('WebSocket not connected, message not sent:', data)
        }
    }

    sendCursorUpdate(x, y) {
        // Store pending cursor position
        this.pendingCursor = { x, y }

        // If we haven't scheduled a send, schedule one
        if (!this.cursorAnimationFrame) {
            this.cursorAnimationFrame = requestAnimationFrame(() => {
                this.processCursorUpdate()
            })
        }
    }

    processCursorUpdate() {
        const now = performance.now()

        // Check if enough time has passed since last send
        if (now - this.lastCursorSend >= this.cursorSendInterval && this.pendingCursor) {
            this.send({
                type: 'cursor',
                x: this.pendingCursor.x,
                y: this.pendingCursor.y
            })
            this.lastCursorSend = now
            this.pendingCursor = null
            this.cursorAnimationFrame = null
        } else if (this.pendingCursor) {
            // Schedule another check
            this.cursorAnimationFrame = requestAnimationFrame(() => {
                this.processCursorUpdate()
            })
        } else {
            this.cursorAnimationFrame = null
        }
    }

    sendObjectCreate(object) {
        this.send({
            type: 'object-create',
            object: object
        })
    }

    sendObjectUpdate(objectId, updates) {
        this.send({
            type: 'object-update',
            'object-id': objectId,
            updates: updates
        })
    }

    sendObjectDelete(objectId) {
        this.send({
            type: 'object-delete',
            'object-id': objectId
        })
    }

    scheduleReconnect() {
        if (this.reconnectAttempts >= this.maxReconnectAttempts) {
            console.error('Max reconnection attempts reached')
            this.onError(new Error('Unable to reconnect to server'))
            return
        }

        this.reconnectAttempts++
        const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1)

        console.log(`Attempting to reconnect in ${delay}ms (attempt ${this.reconnectAttempts}/${this.maxReconnectAttempts})`)
        this.onReconnecting()

        setTimeout(() => {
            this.connect()
        }, delay)
    }

    disconnect() {
        if (this.ws) {
            this.ws.close(1000, 'User disconnect')
            this.ws = null
            this.isConnected = false
        }
    }
}