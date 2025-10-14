// websocket.js - WebSocket client for real-time synchronization

export class LatencyMonitor {
    constructor(options = {}) {
        this.maxHistorySize = options.maxHistorySize || 1000
        this.warningThreshold = options.warningThreshold || 100 // 100ms
        this.pendingMessages = new Map() // messageId -> { sentTime, type }
        this.latencyHistory = [] // Array of latency measurements
        this.stats = {
            totalMessages: 0,
            averageLatency: 0,
            minLatency: Infinity,
            maxLatency: 0,
            warningCount: 0
        }
    }

    startTracking(messageId, messageType) {
        this.pendingMessages.set(messageId, {
            sentTime: performance.now(),
            type: messageType
        })
    }

    endTracking(messageId) {
        const pending = this.pendingMessages.get(messageId)
        if (!pending) {
            return null // Message not found or already tracked
        }

        const latency = performance.now() - pending.sentTime
        this.pendingMessages.delete(messageId)

        // Record latency
        this.recordLatency(latency, pending.type)

        // Warn if high latency
        if (latency > this.warningThreshold) {
            console.warn(`⚠️ High latency detected: ${Math.round(latency)}ms for ${pending.type} message`)
            this.stats.warningCount++
        }

        return latency
    }

    recordLatency(latency, messageType) {
        // Add to history
        this.latencyHistory.push({
            latency,
            type: messageType,
            timestamp: Date.now()
        })

        // Trim history if too large
        if (this.latencyHistory.length > this.maxHistorySize) {
            this.latencyHistory.shift()
        }

        // Update stats
        this.stats.totalMessages++
        this.stats.minLatency = Math.min(this.stats.minLatency, latency)
        this.stats.maxLatency = Math.max(this.stats.maxLatency, latency)

        // Recalculate average
        const sum = this.latencyHistory.reduce((acc, item) => acc + item.latency, 0)
        this.stats.averageLatency = sum / this.latencyHistory.length
    }

    getPercentile(percentile) {
        if (this.latencyHistory.length === 0) {
            return 0
        }

        // Sort latencies
        const sorted = this.latencyHistory
            .map(item => item.latency)
            .sort((a, b) => a - b)

        // Calculate percentile index
        const index = Math.ceil((percentile / 100) * sorted.length) - 1
        return sorted[Math.max(0, index)]
    }

    getStats() {
        return {
            totalMessages: this.stats.totalMessages,
            averageLatency: Math.round(this.stats.averageLatency * 100) / 100,
            minLatency: this.stats.minLatency === Infinity ? 0 : Math.round(this.stats.minLatency * 100) / 100,
            maxLatency: Math.round(this.stats.maxLatency * 100) / 100,
            p50: Math.round(this.getPercentile(50) * 100) / 100,
            p95: Math.round(this.getPercentile(95) * 100) / 100,
            p99: Math.round(this.getPercentile(99) * 100) / 100,
            warningCount: this.stats.warningCount,
            historySize: this.latencyHistory.length,
            pendingMessages: this.pendingMessages.size
        }
    }

    getStatsByType(messageType) {
        const filtered = this.latencyHistory.filter(item => item.type === messageType)
        if (filtered.length === 0) {
            return null
        }

        const latencies = filtered.map(item => item.latency).sort((a, b) => a - b)
        const sum = latencies.reduce((acc, val) => acc + val, 0)

        const getPercentile = (p) => {
            const index = Math.ceil((p / 100) * latencies.length) - 1
            return latencies[Math.max(0, index)]
        }

        return {
            messageType,
            count: filtered.length,
            averageLatency: Math.round((sum / filtered.length) * 100) / 100,
            minLatency: Math.round(latencies[0] * 100) / 100,
            maxLatency: Math.round(latencies[latencies.length - 1] * 100) / 100,
            p50: Math.round(getPercentile(50) * 100) / 100,
            p95: Math.round(getPercentile(95) * 100) / 100,
            p99: Math.round(getPercentile(99) * 100) / 100
        }
    }

    logStats() {
        const stats = this.getStats()
        console.log('=== Latency Statistics ===')
        console.log(`Total messages: ${stats.totalMessages}`)
        console.log(`Average latency: ${stats.averageLatency}ms`)
        console.log(`Min latency: ${stats.minLatency}ms`)
        console.log(`Max latency: ${stats.maxLatency}ms`)
        console.log(`P50 (median): ${stats.p50}ms`)
        console.log(`P95: ${stats.p95}ms`)
        console.log(`P99: ${stats.p99}ms`)
        console.log(`High latency warnings: ${stats.warningCount}`)
        console.log(`History size: ${stats.historySize}`)
        console.log(`Pending messages: ${stats.pendingMessages}`)
    }

    reset() {
        this.pendingMessages.clear()
        this.latencyHistory = []
        this.stats = {
            totalMessages: 0,
            averageLatency: 0,
            minLatency: Infinity,
            maxLatency: 0,
            warningCount: 0
        }
    }
}

export class CursorThrottle {
    constructor(sendCallback, intervalMs = 50) { // 20/sec = 50ms
        this.sendCallback = sendCallback
        this.intervalMs = intervalMs
        this.pendingCursor = null
        this.intervalId = null
        this.start()
    }

    start() {
        this.intervalId = setInterval(() => {
            if (this.pendingCursor) {
                this.sendCallback(this.pendingCursor.x, this.pendingCursor.y)
                this.pendingCursor = null
            }
        }, this.intervalMs)
    }

    update(x, y) {
        this.pendingCursor = { x, y } // Always update to latest
    }

    stop() {
        if (this.intervalId) {
            clearInterval(this.intervalId)
            this.intervalId = null
        }
    }
}

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

        // Message ID counter for latency tracking
        this.messageIdCounter = 0

        // Latency monitoring
        this.latencyMonitor = new LatencyMonitor({
            maxHistorySize: 1000,
            warningThreshold: 100 // 100ms
        })

        // Bandwidth tracking for delta compression testing
        this.bandwidthStats = {
            totalBytesReceived: 0,
            objectUpdateMessages: 0,
            startTime: Date.now()
        }

        // Cursor throttling using CursorThrottle class
        this.cursorThrottle = new CursorThrottle((x, y) => {
            this.send({ type: 'cursor', x, y })
        })

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
        this.onObjectsDeleted = () => {}
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
        // Track latency if message has an ID (response to our request)
        if (data.messageId) {
            const latency = this.latencyMonitor.endTracking(data.messageId)
            if (latency !== null && latency < 100) {
                // Only log sub-100ms latencies at debug level
                console.debug(`Message ${data.type} latency: ${Math.round(latency)}ms`)
            }
        }

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

            case 'cursor-batch':
                // Handle batched cursor updates
                if (data.cursors && Array.isArray(data.cursors)) {
                    data.cursors.forEach(cursor => {
                        this.onCursorUpdate(cursor)
                    })
                }
                break

            case 'object-create':
                this.onObjectCreated(data)
                break

            case 'object-update':
                // Track bandwidth for delta compression testing
                const messageSize = JSON.stringify(data).length
                this.bandwidthStats.totalBytesReceived += messageSize
                this.bandwidthStats.objectUpdateMessages++
                console.log(`Object update received: ${messageSize} bytes (total: ${this.bandwidthStats.totalBytesReceived} bytes, ${this.bandwidthStats.objectUpdateMessages} messages)`)
                this.onObjectUpdated(data)
                break

            case 'object-delete':
                this.onObjectDeleted(data)
                break

            case 'objects-delete':
                this.onObjectsDeleted(data)
                break

            case 'error':
                console.error('Server error:', data.message)
                this.onError(new Error(data.message))
                break

            default:
                console.warn('Unknown message type:', data.type)
        }
    }

    send(data, trackLatency = false) {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            // Add message ID for latency tracking if requested
            if (trackLatency && data.type !== 'cursor') {
                data.messageId = ++this.messageIdCounter
                this.latencyMonitor.startTracking(data.messageId, data.type)
            }

            this.ws.send(JSON.stringify(data))
        } else {
            console.warn('WebSocket not connected, message not sent:', data)
        }
    }

    sendCursorUpdate(x, y) {
        this.cursorThrottle.update(x, y)
    }

    sendObjectCreate(object) {
        this.send({
            type: 'object-create',
            object: object
        }, true) // Enable latency tracking
    }

    sendObjectUpdate(objectId, updates) {
        this.send({
            type: 'object-update',
            'object-id': objectId,
            updates: updates
        }, true) // Enable latency tracking
    }

    sendObjectDelete(objectId) {
        this.send({
            type: 'object-delete',
            'object-id': objectId
        }, true) // Enable latency tracking
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

    getBandwidthStats() {
        const elapsedMinutes = (Date.now() - this.bandwidthStats.startTime) / (1000 * 60)
        return {
            totalBytes: this.bandwidthStats.totalBytesReceived,
            messageCount: this.bandwidthStats.objectUpdateMessages,
            averageBytesPerMessage: this.bandwidthStats.objectUpdateMessages > 0
                ? Math.round(this.bandwidthStats.totalBytesReceived / this.bandwidthStats.objectUpdateMessages)
                : 0,
            bytesPerMinute: elapsedMinutes > 0
                ? Math.round(this.bandwidthStats.totalBytesReceived / elapsedMinutes)
                : 0,
            elapsedMinutes: Math.round(elapsedMinutes * 10) / 10
        }
    }

    getLatencyStats() {
        return this.latencyMonitor.getStats()
    }

    getLatencyStatsByType(messageType) {
        return this.latencyMonitor.getStatsByType(messageType)
    }

    logLatencyStats() {
        this.latencyMonitor.logStats()
    }

    logBandwidthStats() {
        const stats = this.getBandwidthStats()
        console.log('Bandwidth Stats (Delta Compression):', {
            'Total bytes received': `${stats.totalBytes} bytes`,
            'Object update messages': stats.messageCount,
            'Avg bytes per message': `${stats.averageBytesPerMessage} bytes`,
            'Bytes per minute': `${stats.bytesPerMinute} bytes/min`,
            'Elapsed time': `${stats.elapsedMinutes} minutes`
        })
    }

    disconnect() {
        // Log final bandwidth stats before disconnecting
        if (this.bandwidthStats.objectUpdateMessages > 0) {
            console.log('=== Final Bandwidth Stats (Delta Compression) ===')
            this.logBandwidthStats()
        }

        // Stop cursor throttle timer
        if (this.cursorThrottle) {
            this.cursorThrottle.stop()
            console.log('Stopped cursor throttle timer')
        }

        // Close WebSocket connection
        if (this.ws) {
            this.ws.close(1000, 'User disconnect')
            this.ws = null
            this.isConnected = false
        }

        console.log('=== Disconnect cleanup complete ===')
    }

    sendObjectsDelete(objectIds) {
        this.send({
            type: 'objects-delete',
            'object-ids': objectIds
        })
    }

    sendObjectsDelete(objectIds) {
        this.send({
            type: 'objects-delete',
            'object-ids': objectIds
        })
    }
}