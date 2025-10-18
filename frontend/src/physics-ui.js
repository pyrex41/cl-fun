// physics-ui.js - UI Controls for Physics Interaction

/**
 * PhysicsUI - Manages UI controls for physics simulation interaction
 *
 * Features:
 * - Spawn ball button with click-to-spawn mode
 * - Fan toggle buttons for controlling force fields
 * - Gravity adjustment slider (-20 to +20 m/s²)
 * - Physics stats overlay (FPS, object count, latency)
 */
export class PhysicsUI {
    constructor(websocketClient, physicsRenderer) {
        this.ws = websocketClient;
        this.renderer = physicsRenderer;

        // State
        this.spawnMode = false;
        this.magnetCreateMode = null; // null, 'north', or 'south'
        this.emitterCreateMode = false; // true when in emitter creation mode
        this.emitterSettings = {
            rate: 1.0,
            direction: 0, // radians
            velocity: 200
        };
        this.fans = new Map(); // fanId -> {enabled: boolean, name: string}
        this.currentGravity = 9.8;
        this.stats = {
            fps: 0,
            objectCount: 0,
            activeCount: 0,
            sleepingCount: 0,
            ghostCount: 0,
            latency: 0
        };

        // Stats update interval
        this.statsUpdateInterval = null;

        this.initializeUI();
    }

    initializeUI() {
        this.createControlPanel();
        this.setupEventListeners();
        this.startStatsUpdate();
    }

    createControlPanel() {
        // Create main control panel
        const panel = document.createElement('div');
        panel.id = 'physics-controls';
        panel.innerHTML = `
            <div class="physics-panel-header">
                <h3 class="physics-panel-title">Physics Controls</h3>
                <button id="physics-toggle-panel" class="panel-collapse-btn" title="Collapse">−</button>
            </div>

            <div class="physics-panel-content">
                <!-- Spawn Ball Section -->
                <div class="physics-section">
                    <h4 class="physics-section-title">Spawn Ball</h4>
                    <button id="spawn-ball-btn" class="physics-btn physics-btn-primary">
                        <span class="btn-icon">⚽</span>
                        <span class="btn-text">Click to Spawn</span>
                    </button>
                    <div class="physics-hint">Click on canvas to spawn a ball</div>
                </div>

                <!-- Fans Section -->
                <div class="physics-section">
                    <h4 class="physics-section-title">Force Fields</h4>
                    <div id="fans-list" class="fans-list">
                        <div class="physics-hint">No fans available</div>
                    </div>
                </div>

                <!-- Magnets Section -->
                <div class="physics-section">
                    <h4 class="physics-section-title">Magnets</h4>
                    <div class="magnet-controls">
                        <button id="create-north-magnet-btn" class="physics-btn">
                            <span class="btn-icon" style="color: #e74c3c;">🧲</span>
                            <span class="btn-text">Create North Pole</span>
                        </button>
                        <button id="create-south-magnet-btn" class="physics-btn">
                            <span class="btn-icon" style="color: #3498db;">🧲</span>
                            <span class="btn-text">Create South Pole</span>
                        </button>
                        <div class="physics-hint">Click canvas to place magnet</div>
                    </div>
                </div>

                <!-- Emitters Section (Post-MVP) -->
                <div class="physics-section">
                    <h4 class="physics-section-title">Emitters</h4>
                    <div class="emitter-controls">
                        <button id="create-emitter-btn" class="physics-btn">
                            <span class="btn-icon">💨</span>
                            <span class="btn-text">Create Emitter</span>
                        </button>
                        <div class="emitter-settings" id="emitter-settings" style="display: none;">
                            <label class="control-label">
                                <span>Rate:</span>
                                <span id="emitter-rate-value" class="control-value">1.0 balls/s</span>
                            </label>
                            <input type="range" id="emitter-rate-slider" class="emitter-slider"
                                   min="0.1" max="10" step="0.1" value="1.0">

                            <label class="control-label" style="margin-top: 8px;">
                                <span>Direction:</span>
                                <span id="emitter-direction-value" class="control-value">0°</span>
                            </label>
                            <input type="range" id="emitter-direction-slider" class="emitter-slider"
                                   min="0" max="360" step="15" value="0">

                            <label class="control-label" style="margin-top: 8px;">
                                <span>Velocity:</span>
                                <span id="emitter-velocity-value" class="control-value">200 px/s</span>
                            </label>
                            <input type="range" id="emitter-velocity-slider" class="emitter-slider"
                                   min="50" max="500" step="10" value="200">
                        </div>
                        <div class="physics-hint">Click canvas to place emitter</div>
                    </div>
                </div>

                <!-- Gravity Section -->
                <div class="physics-section">
                    <h4 class="physics-section-title">Gravity</h4>
                    <div class="gravity-control">
                        <label for="gravity-slider" class="control-label">
                            <span>Gravity:</span>
                            <span id="gravity-value" class="control-value">9.8 m/s²</span>
                        </label>
                        <input
                            type="range"
                            id="gravity-slider"
                            class="gravity-slider"
                            min="-20"
                            max="20"
                            step="0.1"
                            value="9.8"
                        >
                        <div class="slider-labels">
                            <span>-20</span>
                            <span>0</span>
                            <span>+20</span>
                        </div>
                    </div>
                </div>

                <!-- Stats Section -->
                <div class="physics-section">
                    <h4 class="physics-section-title">Statistics</h4>
                    <div class="stats-grid">
                        <div class="stat-item">
                            <span class="stat-label">FPS</span>
                            <span id="stat-fps" class="stat-value">60</span>
                        </div>
                        <div class="stat-item">
                            <span class="stat-label">Objects</span>
                            <span id="stat-objects" class="stat-value">0</span>
                        </div>
                        <div class="stat-item">
                            <span class="stat-label">Active</span>
                            <span id="stat-active" class="stat-value">0</span>
                        </div>
                        <div class="stat-item">
                            <span class="stat-label">Sleeping</span>
                            <span id="stat-sleeping" class="stat-value">0</span>
                        </div>
                        <div class="stat-item">
                            <span class="stat-label">Ghosts</span>
                            <span id="stat-ghosts" class="stat-value">0</span>
                        </div>
                        <div class="stat-item">
                            <span class="stat-label">Latency</span>
                            <span id="stat-latency" class="stat-value">0 ms</span>
                        </div>
                    </div>
                </div>
            </div>
        `;

        document.body.appendChild(panel);
        this.applyStyles();
    }

    applyStyles() {
        const style = document.createElement('style');
        style.textContent = `
            #physics-controls {
                position: fixed;
                bottom: 20px;
                right: 20px;
                width: 320px;
                background: var(--bg-glass);
                backdrop-filter: blur(20px) saturate(180%);
                -webkit-backdrop-filter: blur(20px) saturate(180%);
                border: 1px solid var(--border-primary);
                border-radius: 16px;
                box-shadow: var(--shadow-lg);
                z-index: 200;
                transition: all 0.3s ease;
            }

            #physics-controls.collapsed .physics-panel-content {
                display: none;
            }

            .physics-panel-header {
                display: flex;
                justify-content: space-between;
                align-items: center;
                padding: 16px 20px;
                border-bottom: 1px solid var(--border-primary);
            }

            .physics-panel-title {
                font-size: 14px;
                font-weight: 700;
                color: var(--text-primary);
                margin: 0;
                text-transform: uppercase;
                letter-spacing: 1px;
            }

            .panel-collapse-btn {
                width: 28px;
                height: 28px;
                background: rgba(255, 255, 255, 0.03);
                border: 1px solid var(--border-secondary);
                border-radius: 6px;
                color: var(--text-secondary);
                cursor: pointer;
                font-size: 18px;
                line-height: 1;
                transition: all 0.2s ease;
            }

            .panel-collapse-btn:hover {
                background: rgba(255, 255, 255, 0.08);
                border-color: var(--accent-primary);
                color: var(--text-primary);
            }

            .physics-panel-content {
                padding: 16px;
                display: flex;
                flex-direction: column;
                gap: 20px;
                max-height: calc(100vh - 200px);
                overflow-y: auto;
            }

            .physics-section {
                display: flex;
                flex-direction: column;
                gap: 12px;
            }

            .physics-section-title {
                font-size: 11px;
                font-weight: 600;
                color: var(--text-tertiary);
                text-transform: uppercase;
                letter-spacing: 1.2px;
                margin: 0;
            }

            .physics-btn {
                padding: 12px 16px;
                border: 1px solid var(--border-secondary);
                border-radius: 10px;
                background: rgba(255, 255, 255, 0.03);
                color: var(--text-primary);
                font-size: 14px;
                font-weight: 600;
                cursor: pointer;
                transition: all 0.2s ease;
                display: flex;
                align-items: center;
                justify-content: center;
                gap: 8px;
            }

            .physics-btn:hover {
                background: rgba(255, 255, 255, 0.08);
                border-color: var(--accent-primary);
                transform: translateY(-1px);
            }

            .physics-btn-primary {
                background: linear-gradient(135deg, var(--accent-primary) 0%, var(--accent-secondary) 100%);
                border-color: var(--accent-primary);
                color: #ffffff;
                box-shadow: 0 4px 16px rgba(139, 92, 246, 0.3);
            }

            .physics-btn-primary:hover {
                transform: translateY(-2px);
                box-shadow: 0 6px 24px rgba(139, 92, 246, 0.4);
            }

            .physics-btn-primary.active {
                background: linear-gradient(135deg, #10b981 0%, #059669 100%);
                border-color: #10b981;
                box-shadow: 0 4px 16px rgba(16, 185, 129, 0.3);
            }

            .btn-icon {
                font-size: 18px;
            }

            .physics-hint {
                font-size: 12px;
                color: var(--text-tertiary);
                font-style: italic;
            }

            .fans-list {
                display: flex;
                flex-direction: column;
                gap: 8px;
            }

            .fan-item {
                display: flex;
                align-items: center;
                justify-content: space-between;
                padding: 10px 12px;
                background: rgba(255, 255, 255, 0.03);
                border: 1px solid var(--border-secondary);
                border-radius: 8px;
                transition: all 0.2s ease;
            }

            .fan-item:hover {
                background: rgba(255, 255, 255, 0.06);
                border-color: var(--accent-primary);
            }

            .fan-info {
                display: flex;
                align-items: center;
                gap: 10px;
            }

            .fan-icon {
                font-size: 20px;
            }

            .fan-name {
                font-size: 13px;
                font-weight: 600;
                color: var(--text-primary);
            }

            .fan-toggle {
                width: 48px;
                height: 26px;
                background: rgba(255, 255, 255, 0.1);
                border: 1px solid var(--border-secondary);
                border-radius: 13px;
                cursor: pointer;
                position: relative;
                transition: all 0.2s ease;
            }

            .fan-toggle::after {
                content: '';
                position: absolute;
                top: 2px;
                left: 2px;
                width: 20px;
                height: 20px;
                background: var(--text-secondary);
                border-radius: 50%;
                transition: all 0.2s ease;
            }

            .fan-toggle.active {
                background: var(--accent-primary);
                border-color: var(--accent-primary);
            }

            .fan-toggle.active::after {
                left: 24px;
                background: #ffffff;
            }

            .gravity-control {
                display: flex;
                flex-direction: column;
                gap: 8px;
            }

            .control-label {
                display: flex;
                justify-content: space-between;
                align-items: center;
                font-size: 13px;
                color: var(--text-secondary);
                font-weight: 600;
            }

            .control-value {
                color: var(--accent-primary);
                font-weight: 700;
            }

            .gravity-slider {
                width: 100%;
                height: 6px;
                background: rgba(255, 255, 255, 0.1);
                border-radius: 3px;
                outline: none;
                -webkit-appearance: none;
            }

            .gravity-slider::-webkit-slider-thumb {
                -webkit-appearance: none;
                width: 18px;
                height: 18px;
                background: var(--accent-primary);
                border-radius: 50%;
                cursor: pointer;
                box-shadow: 0 2px 8px rgba(139, 92, 246, 0.4);
                transition: all 0.2s ease;
            }

            .gravity-slider::-webkit-slider-thumb:hover {
                transform: scale(1.2);
                box-shadow: 0 4px 16px rgba(139, 92, 246, 0.6);
            }

            .gravity-slider::-moz-range-thumb {
                width: 18px;
                height: 18px;
                background: var(--accent-primary);
                border-radius: 50%;
                cursor: pointer;
                border: none;
                box-shadow: 0 2px 8px rgba(139, 92, 246, 0.4);
            }

            .slider-labels {
                display: flex;
                justify-content: space-between;
                font-size: 11px;
                color: var(--text-tertiary);
                margin-top: -4px;
            }

            .stats-grid {
                display: grid;
                grid-template-columns: repeat(2, 1fr);
                gap: 10px;
            }

            .stat-item {
                display: flex;
                flex-direction: column;
                gap: 4px;
                padding: 10px 12px;
                background: rgba(255, 255, 255, 0.03);
                border: 1px solid var(--border-secondary);
                border-radius: 8px;
                transition: all 0.2s ease;
            }

            .stat-item:hover {
                background: rgba(255, 255, 255, 0.06);
                border-color: var(--accent-primary);
            }

            .stat-label {
                font-size: 11px;
                color: var(--text-tertiary);
                font-weight: 500;
            }

            .stat-value {
                font-size: 16px;
                color: var(--text-primary);
                font-weight: 700;
            }

            .emitter-controls {
                display: flex;
                flex-direction: column;
                gap: 12px;
            }

            .emitter-settings {
                padding: 12px;
                background: rgba(255, 255, 255, 0.03);
                border: 1px solid var(--border-secondary);
                border-radius: 8px;
                display: flex;
                flex-direction: column;
                gap: 8px;
            }

            .emitter-slider {
                width: 100%;
                height: 6px;
                background: rgba(255, 255, 255, 0.1);
                border-radius: 3px;
                outline: none;
                -webkit-appearance: none;
            }

            .emitter-slider::-webkit-slider-thumb {
                -webkit-appearance: none;
                width: 18px;
                height: 18px;
                background: var(--accent-primary);
                border-radius: 50%;
                cursor: pointer;
                box-shadow: 0 2px 8px rgba(139, 92, 246, 0.4);
                transition: all 0.2s ease;
            }

            .emitter-slider::-webkit-slider-thumb:hover {
                transform: scale(1.2);
                box-shadow: 0 4px 16px rgba(139, 92, 246, 0.6);
            }

            .emitter-slider::-moz-range-thumb {
                width: 18px;
                height: 18px;
                background: var(--accent-primary);
                border-radius: 50%;
                cursor: pointer;
                border: none;
                box-shadow: 0 2px 8px rgba(139, 92, 246, 0.4);
            }
        `;
        document.head.appendChild(style);
    }

    setupEventListeners() {
        // Spawn ball button
        const spawnBtn = document.getElementById('spawn-ball-btn');
        spawnBtn.addEventListener('click', () => this.toggleSpawnMode());

        // Magnet creation buttons
        const northMagnetBtn = document.getElementById('create-north-magnet-btn');
        const southMagnetBtn = document.getElementById('create-south-magnet-btn');
        northMagnetBtn.addEventListener('click', () => this.toggleMagnetCreateMode('north'));
        southMagnetBtn.addEventListener('click', () => this.toggleMagnetCreateMode('south'));

        // Emitter creation button
        const emitterBtn = document.getElementById('create-emitter-btn');
        emitterBtn.addEventListener('click', () => this.toggleEmitterCreateMode());

        // Emitter sliders
        const rateSlider = document.getElementById('emitter-rate-slider');
        const rateValue = document.getElementById('emitter-rate-value');
        rateSlider.addEventListener('input', (e) => {
            const value = parseFloat(e.target.value);
            rateValue.textContent = `${value.toFixed(1)} balls/s`;
            this.emitterSettings.rate = value;
        });

        const directionSlider = document.getElementById('emitter-direction-slider');
        const directionValue = document.getElementById('emitter-direction-value');
        directionSlider.addEventListener('input', (e) => {
            const value = parseFloat(e.target.value);
            directionValue.textContent = `${value.toFixed(0)}°`;
            this.emitterSettings.direction = (value * Math.PI) / 180; // Convert to radians
        });

        const velocitySlider = document.getElementById('emitter-velocity-slider');
        const velocityValue = document.getElementById('emitter-velocity-value');
        velocitySlider.addEventListener('input', (e) => {
            const value = parseFloat(e.target.value);
            velocityValue.textContent = `${value.toFixed(0)} px/s`;
            this.emitterSettings.velocity = value;
        });

        // Gravity slider
        const gravitySlider = document.getElementById('gravity-slider');
        const gravityValue = document.getElementById('gravity-value');

        gravitySlider.addEventListener('input', (e) => {
            const value = parseFloat(e.target.value);
            gravityValue.textContent = `${value.toFixed(1)} m/s²`;
            this.currentGravity = value;
        });

        gravitySlider.addEventListener('change', (e) => {
            const value = parseFloat(e.target.value);
            this.sendGravityUpdate(value);
        });

        // Panel collapse toggle
        const toggleBtn = document.getElementById('physics-toggle-panel');
        const panel = document.getElementById('physics-controls');

        toggleBtn.addEventListener('click', () => {
            panel.classList.toggle('collapsed');
            toggleBtn.textContent = panel.classList.contains('collapsed') ? '+' : '−';
        });

        // Canvas click for spawn mode
        const canvasContainer = document.getElementById('canvas-container');
        if (canvasContainer) {
            canvasContainer.addEventListener('click', (e) => this.handleCanvasClick(e));
        }
    }

    toggleSpawnMode() {
        this.spawnMode = !this.spawnMode;
        const btn = document.getElementById('spawn-ball-btn');
        const btnText = btn.querySelector('.btn-text');

        if (this.spawnMode) {
            btn.classList.add('active');
            btnText.textContent = 'Spawn Mode ON';
            document.getElementById('canvas-container').style.cursor = 'crosshair';
        } else {
            btn.classList.remove('active');
            btnText.textContent = 'Click to Spawn';
            document.getElementById('canvas-container').style.cursor = 'default';
        }
    }

    toggleMagnetCreateMode(polarity) {
        // Disable spawn mode if active
        if (this.spawnMode) {
            this.toggleSpawnMode();
        }

        // Disable emitter create mode if active
        if (this.emitterCreateMode) {
            this.toggleEmitterCreateMode();
        }

        // Toggle magnet create mode
        if (this.magnetCreateMode === polarity) {
            this.magnetCreateMode = null;
            document.getElementById('canvas-container').style.cursor = 'default';
            // Reset button states
            document.getElementById('create-north-magnet-btn').classList.remove('active');
            document.getElementById('create-south-magnet-btn').classList.remove('active');
        } else {
            this.magnetCreateMode = polarity;
            document.getElementById('canvas-container').style.cursor = 'crosshair';
            // Update button states
            document.getElementById('create-north-magnet-btn').classList.toggle('active', polarity === 'north');
            document.getElementById('create-south-magnet-btn').classList.toggle('active', polarity === 'south');
        }
    }

    toggleEmitterCreateMode() {
        // Disable other modes
        if (this.spawnMode) {
            this.toggleSpawnMode();
        }
        if (this.magnetCreateMode) {
            this.toggleMagnetCreateMode(this.magnetCreateMode);
        }

        // Toggle emitter create mode
        this.emitterCreateMode = !this.emitterCreateMode;
        const btn = document.getElementById('create-emitter-btn');
        const btnText = btn.querySelector('.btn-text');
        const settings = document.getElementById('emitter-settings');

        if (this.emitterCreateMode) {
            btn.classList.add('active');
            btnText.textContent = 'Emitter Mode ON';
            settings.style.display = 'flex';
            document.getElementById('canvas-container').style.cursor = 'crosshair';
        } else {
            btn.classList.remove('active');
            btnText.textContent = 'Create Emitter';
            settings.style.display = 'none';
            document.getElementById('canvas-container').style.cursor = 'default';
        }
    }

    handleCanvasClick(event) {
        // Get click position relative to canvas
        const rect = event.target.getBoundingClientRect();
        const x = event.clientX - rect.left;
        const y = event.clientY - rect.top;

        // Handle spawn mode
        if (this.spawnMode) {
            // Generate random velocity for spawned ball
            const vx = (Math.random() - 0.5) * 200; // -100 to +100 px/s
            const vy = (Math.random() - 0.5) * 200;
            this.spawnBall(x, y, vx, vy);
            this.showSpawnFeedback(x, y);
            return;
        }

        // Handle magnet create mode
        if (this.magnetCreateMode) {
            const polarity = this.magnetCreateMode === 'north' ? 1.0 : -1.0;
            this.createMagnet(x, y, polarity);
            this.showMagnetFeedback(x, y, polarity);
            // Auto-disable mode after creation
            this.toggleMagnetCreateMode(this.magnetCreateMode);
            return;
        }

        // Handle emitter create mode
        if (this.emitterCreateMode) {
            this.createEmitter(x, y);
            this.showEmitterFeedback(x, y);
            return;
        }
    }

    spawnBall(x, y, vx = 0, vy = 0) {
        const ghostId = `ghost-${Date.now()}-${Math.random()}`;

        // Create ghost locally (via renderer)
        if (this.renderer && this.renderer.predictor) {
            this.renderer.predictor.spawnGhost(ghostId, x, y, vx, vy);
        }

        // Send to server
        this.ws.send({
            type: 'physics-spawn-ball',
            x: x,
            y: y,
            vx: vx,
            vy: vy,
            ghostId: ghostId
        }, true);

        console.log(`Spawned ball at (${x.toFixed(0)}, ${y.toFixed(0)}) with velocity (${vx.toFixed(0)}, ${vy.toFixed(0)})`);
    }

    showSpawnFeedback(x, y) {
        const feedback = document.createElement('div');
        feedback.className = 'spawn-feedback';
        feedback.style.cssText = `
            position: absolute;
            left: ${x}px;
            top: ${y}px;
            width: 20px;
            height: 20px;
            border: 2px solid var(--accent-primary);
            border-radius: 50%;
            pointer-events: none;
            animation: spawn-pulse 0.5s ease-out;
        `;

        document.getElementById('canvas-container').appendChild(feedback);

        setTimeout(() => feedback.remove(), 500);

        // Add animation if not exists
        if (!document.getElementById('spawn-animation-style')) {
            const style = document.createElement('style');
            style.id = 'spawn-animation-style';
            style.textContent = `
                @keyframes spawn-pulse {
                    0% {
                        transform: scale(0.5);
                        opacity: 1;
                    }
                    100% {
                        transform: scale(2);
                        opacity: 0;
                    }
                }
            `;
            document.head.appendChild(style);
        }
    }

    createMagnet(x, y, polarity, strength = 200.0, radius = 150.0) {
        // Send to server
        this.ws.send({
            type: 'physics-create-magnet',
            x: x,
            y: y,
            polarity: polarity,
            strength: strength,
            radius: radius
        }, true);

        console.log(`Creating magnet at (${x.toFixed(0)}, ${y.toFixed(0)}) polarity ${polarity > 0 ? 'North' : 'South'}`);
    }

    showMagnetFeedback(x, y, polarity) {
        const feedback = document.createElement('div');
        feedback.className = 'magnet-feedback';
        const color = polarity > 0 ? '#e74c3c' : '#3498db';
        feedback.style.cssText = `
            position: absolute;
            left: ${x}px;
            top: ${y}px;
            width: 40px;
            height: 40px;
            border: 3px solid ${color};
            border-radius: 8px;
            pointer-events: none;
            animation: magnet-pulse 0.6s ease-out;
        `;

        document.getElementById('canvas-container').appendChild(feedback);

        setTimeout(() => feedback.remove(), 600);

        // Add animation if not exists
        if (!document.getElementById('magnet-animation-style')) {
            const style = document.createElement('style');
            style.id = 'magnet-animation-style';
            style.textContent = `
                @keyframes magnet-pulse {
                    0% {
                        transform: scale(0.5) rotate(0deg);
                        opacity: 1;
                    }
                    100% {
                        transform: scale(2) rotate(180deg);
                        opacity: 0;
                    }
                }
            `;
            document.head.appendChild(style);
        }
    }

    sendGravityUpdate(gravity) {
        this.ws.send({
            type: 'physics-adjust-gravity',
            gravityY: gravity  // Backend expects gravityY field
        }, true);

        console.log(`Updated gravity to ${gravity.toFixed(1)} m/s²`);
    }

    // Handle incoming gravity changes from other clients
    handleGravityChanged(data) {
        const newGravity = data.gravityY || data.gravity_y || 9.8;
        this.currentGravity = newGravity;

        // Update slider UI without triggering another update
        const gravitySlider = document.getElementById('gravity-slider');
        const gravityValue = document.getElementById('gravity-value');

        if (gravitySlider && gravityValue) {
            gravitySlider.value = newGravity;
            gravityValue.textContent = `${newGravity.toFixed(1)} m/s²`;
        }

        console.log(`Gravity updated by ${data.username || 'another user'} to ${newGravity.toFixed(1)} m/s²`);
    }

    createEmitter(x, y) {
        const { rate, direction, velocity } = this.emitterSettings;

        // Send to server
        this.ws.send({
            type: 'physics-create-emitter',
            x: x,
            y: y,
            rate: rate,
            direction: direction,
            initialVelocity: velocity,
            ballRadius: 10.0,
            ballMass: 1.0
        }, true);

        console.log(`Creating emitter at (${x.toFixed(0)}, ${y.toFixed(0)}) rate=${rate}/s dir=${(direction * 180 / Math.PI).toFixed(0)}° vel=${velocity}`);
    }

    showEmitterFeedback(x, y) {
        const feedback = document.createElement('div');
        feedback.className = 'emitter-feedback';
        const directionDeg = (this.emitterSettings.direction * 180 / Math.PI).toFixed(0);

        feedback.style.cssText = `
            position: absolute;
            left: ${x}px;
            top: ${y}px;
            width: 50px;
            height: 50px;
            border: 3px solid #f39c12;
            border-radius: 4px;
            pointer-events: none;
            animation: emitter-pulse 0.6s ease-out;
            transform: rotate(${directionDeg}deg);
        `;

        document.getElementById('canvas-container').appendChild(feedback);

        setTimeout(() => feedback.remove(), 600);

        // Add animation if not exists
        if (!document.getElementById('emitter-animation-style')) {
            const style = document.createElement('style');
            style.id = 'emitter-animation-style';
            style.textContent = `
                @keyframes emitter-pulse {
                    0% {
                        transform: scale(0.5);
                        opacity: 1;
                    }
                    100% {
                        transform: scale(2);
                        opacity: 0;
                    }
                }
            `;
            document.head.appendChild(style);
        }
    }

    // Fan management
    addFan(fanId, fanName, enabled = false) {
        this.fans.set(fanId, { enabled, name: fanName });
        this.updateFansList();
    }

    removeFan(fanId) {
        this.fans.delete(fanId);
        this.updateFansList();
    }

    updateFansList() {
        const fansList = document.getElementById('fans-list');

        if (this.fans.size === 0) {
            fansList.innerHTML = '<div class="physics-hint">No fans available</div>';
            return;
        }

        fansList.innerHTML = '';

        for (const [fanId, fan] of this.fans.entries()) {
            const fanItem = document.createElement('div');
            fanItem.className = 'fan-item';
            fanItem.innerHTML = `
                <div class="fan-info">
                    <span class="fan-icon">💨</span>
                    <span class="fan-name">${fan.name}</span>
                </div>
                <div class="fan-toggle ${fan.enabled ? 'active' : ''}" data-fan-id="${fanId}"></div>
            `;

            const toggle = fanItem.querySelector('.fan-toggle');
            toggle.addEventListener('click', () => this.toggleFan(fanId));

            fansList.appendChild(fanItem);
        }
    }

    toggleFan(fanId) {
        const fan = this.fans.get(fanId);
        if (!fan) return;

        fan.enabled = !fan.enabled;

        // Update UI
        this.updateFansList();

        // Send to server
        this.ws.send({
            type: 'physics-toggle-fan',
            fanId: fanId,
            enabled: fan.enabled
        }, true);

        console.log(`Fan ${fanId} ${fan.enabled ? 'enabled' : 'disabled'}`);
    }

    // Stats update
    startStatsUpdate() {
        this.statsUpdateInterval = setInterval(() => {
            this.updateStats();
        }, 100); // Update stats 10 times per second
    }

    updateStats() {
        // Get stats from renderer
        if (this.renderer) {
            this.stats.objectCount = this.renderer.serverBalls ? this.renderer.serverBalls.size : 0;
            this.stats.ghostCount = this.renderer.predictor ? this.renderer.predictor.ghosts.size : 0;
        }

        // Get latency from websocket
        if (this.ws && this.ws.latencyMonitor) {
            const latencyStats = this.ws.getLatencyStats();
            this.stats.latency = latencyStats.averageLatency || 0;
        }

        // Update FPS (use performance.now() for frame timing)
        this.stats.fps = this.calculateFPS();

        // Update DOM
        document.getElementById('stat-fps').textContent = this.stats.fps.toFixed(0);
        document.getElementById('stat-objects').textContent = this.stats.objectCount;
        document.getElementById('stat-active').textContent = this.stats.activeCount;
        document.getElementById('stat-sleeping').textContent = this.stats.sleepingCount;
        document.getElementById('stat-ghosts').textContent = this.stats.ghostCount;
        document.getElementById('stat-latency').textContent = `${this.stats.latency.toFixed(0)} ms`;
    }

    calculateFPS() {
        // Simple FPS calculation based on requestAnimationFrame timing
        if (!this._lastFrameTime) {
            this._lastFrameTime = performance.now();
            this._frameCount = 0;
            this._fps = 60;
            return this._fps;
        }

        const now = performance.now();
        const delta = now - this._lastFrameTime;

        if (delta >= 1000) { // Update every second
            this._fps = (this._frameCount / delta) * 1000;
            this._frameCount = 0;
            this._lastFrameTime = now;
        }

        this._frameCount++;
        return this._fps;
    }

    // Update stats from server messages
    updateServerStats(stats) {
        if (stats.activeCount !== undefined) {
            this.stats.activeCount = stats.activeCount;
        }
        if (stats.sleepingCount !== undefined) {
            this.stats.sleepingCount = stats.sleepingCount;
        }
        if (stats.totalCount !== undefined) {
            this.stats.objectCount = stats.totalCount;
        }
    }

    // Cleanup
    destroy() {
        if (this.statsUpdateInterval) {
            clearInterval(this.statsUpdateInterval);
        }

        const panel = document.getElementById('physics-controls');
        if (panel) {
            panel.remove();
        }
    }
}
