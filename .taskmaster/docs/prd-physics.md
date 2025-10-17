Of course. Here is a detailed Product Requirements Document (PRD) for the "Living Canvas" feature, tailored to your existing CollabCanvas codebase.

***

# PRD: The Living Canvas - Dynamic Physics & Generative Art

**Author:** Gemini
**Status:** Scoped
**Date:** October 17, 2025
**Related Task Master PRD:** `.taskmaster/docs/prd-living-canvas.md`

## 1. Introduction & Vision

### 1.1. Problem

The current CollabCanvas is a powerful tool for static collaborative design, similar to a simplified Figma. However, it lacks the ability to create dynamic, interactive, and evolving creations. All objects are passive and only change through direct user manipulation, limiting its use for creative coding, simulations, and generative art.

### 1.2. Vision

Transform CollabCanvas from a static design tool into a **"Living Canvas"**: a real-time, multiplayer environment for creative and physics-based experimentation. Instead of just co-designing layouts, users will co-create dynamic systems, interactive art, and simple simulations. This evolution will differentiate CollabCanvas, turning it into a collaborative platform for creative coding and dynamic brainstorming.

### 1.3. Goals

*   **Enable New Forms of Creativity:** Allow users to create generative art and simple physics simulations collaboratively.
*   **Demonstrate Technical Excellence:** Implement a robust, high-performance, and complex state synchronization model that goes beyond simple object property updates.
*   **Enhance AI Capabilities:** Expand the AI agent's role from a design assistant to a system conductor, capable of manipulating physical laws and behaviors on the canvas.
*   **Maintain Performance:** Ensure the introduction of complex dynamics does not degrade the core 60 FPS user experience for at least 5 simultaneous users and 200+ dynamic objects.

### 1.4. Non-Goals

*   This is not a full-fledged game engine. We will not be implementing advanced rendering, complex collision detection (e.g., concave polygons), or a scripting engine.
*   Physics will be illustrative, not scientifically accurate.
*   We will not initially support saving and replaying simulation "recordings," only the initial state of the objects.

## 2. Core Features & Requirements

### Epic 1: The Physics Engine

#### FR-1.1: Server-Side Simulation Core

The backend must implement a deterministic, fixed-timestep 2D physics engine.

*   **Engine Type:** A simple **Verlet integration** engine is recommended. It's stable, computationally efficient, and well-suited for the desired creative effects.
*   **Deterministic:** The simulation must produce the exact same results given the same initial state and inputs, regardless of where it is run (server or client).
*   **Fixed Timestep:** The engine will advance the simulation in fixed intervals (e.g., 50 times per second / every 20ms) independent of the server's tick rate or network latency. This ensures consistency.
*   **Implementation:** A new `backend/src/physics.lisp` file should contain the core simulation logic.

#### FR-1.2: Simulation Controls & Boundaries

The canvas needs global simulation controls and physical boundaries.

*   **UI Controls:** Add `Play`, `Pause`, and `Reset Simulation` buttons to the UI (e.g., in the right-hand sidebar).
*   **Canvas Boundaries:** The visible canvas area will act as a container. A "wall" property will be added, allowing users to choose between:
    *   **Contain:** Objects bounce off the edges.
    *   **Wrap-around:** Objects exiting one side reappear on the opposite side.
*   **Global Gravity:** A global setting for vertical gravity (e.g., from 0 to 10) should be available.

### Epic 2: Dynamic Object Properties

Existing shapes (Rectangles, Circles) must be extensible with physical properties.

#### FR-2.1: New Object Properties

Add the following properties to the object data model:
*   `isDynamic` (boolean): If `true`, the object is affected by the physics engine. If `false`, it's a static object that dynamic objects can collide with.
*   `mass` (number, default: 1): Influences how it reacts to forces.
*   `friction` (number, 0-1, default: 0.1): Air and ground friction.
*   `restitution` (number, 0-1, default: 0.8): "Bounciness." A value of 1 is perfectly elastic.

#### FR-2.2: User Interface for Properties

*   The right-hand sidebar, when an object is selected, must display a new "Physics" section.
*   This section will contain controls (checkbox, sliders) to edit the `isDynamic`, `mass`, `friction`, and `restitution` properties.
*   Changes to these properties must be broadcast in real-time.

### Epic 3: New "Influencer" Objects

Introduce new tool types that influence the simulation.

#### FR-3.1: Gravity Source

*   **Tool:** Add a new "Gravity" tool to the left-hand toolbar.
*   **Functionality:** Users can click on the canvas to place a Gravity Source.
*   **Properties:**
    *   `strength` (number, can be negative for repulsion).
    *   `radius` (number): The area of effect.
*   **Behavior:** Gravity Sources will exert a radial force on all `isDynamic` objects within their radius.

#### FR-3.2: Particle Emitter

*   **Tool:** Add a new "Emitter" tool to the toolbar.
*   **Functionality:** Users place an emitter that continuously generates small, simple `isDynamic` objects (particles).
*   **Properties:**
    *   `rate` (particles per second).
    *   `lifespan` (ms).
    *   `initialVelocity` (vector).
    *   `particleSize` (number).

### Epic 4: High-Performance State Synchronization

To handle the interdependent state of a physics simulation, the WebSocket communication protocol must be upgraded.

#### FR-4.1: State Snapshot Broadcasting

*   Instead of sending individual object updates (`object-update`), the server's physics loop will broadcast a complete snapshot of all `isDynamic` objects.
*   **Frequency:** The snapshot will be sent at a fixed rate (e.g., 20 times per second).
*   **Payload:** The snapshot will contain an array of objects, each with at least its `id`, `x`, `y`, `rotation`.

#### FR-4.2: (Optional Stretch Goal) Binary Protocol

*   To minimize bandwidth, investigate and potentially implement a binary serialization format for the state snapshot, such as **MessagePack**, instead of JSON. This will significantly reduce the payload size. The WebSocket protocol will need to be updated to handle binary frames.

#### FR-4.3: Client-Side Prediction & Reconciliation

*   **Prediction:** The frontend will run the *exact same* deterministic physics simulation code as the server. This allows the client to predict the state of dynamic objects between receiving server snapshots, resulting in a perfectly smooth 60 FPS experience.
*   **Reconciliation:** When a state snapshot arrives from the server, the client will correct the positions of its locally simulated objects. A gentle interpolation (smoothing) should be applied to this correction to avoid visual jitter.

### Epic 5: AI Agent Integration

The AI agent must be taught to understand and control the Living Canvas.

#### FR-5.1: New AI Tools (Function-Calling Schema)

Define new tools for the AI agent:
*   `setObjectPhysics(objectIds: string[], properties: { mass?: number, friction?: number, restitution?: number, isDynamic?: boolean })`
*   `setGlobalPhysics(properties: { gravity?: number, boundaryRule?: 'contain' | 'wrap' })`
*   `createGravitySource(x: number, y: number, strength: number, radius: number)`
*   `createEmitter(x: number, y: number, rate: number, lifespan: number, initialVelocity: {x: number, y: number})`

#### FR-5.2: Example AI Commands

The AI agent should be able to process commands like:
*   "Make all the red circles bouncy and heavy."
*   "Turn on gravity."
*   "Add a gravity source in the center that repels things."
*   "Create a fountain of particles at the bottom of the screen."
*   "Turn everything into a dynamic object."
*   "Reset the simulation."

## 3. Technical Implementation Plan

This feature requires significant changes to both the backend and frontend.

### 3.1. Backend (`.lisp` files)

*   **`backend/src/physics.lisp` (New File):**
    *   Implement the Verlet integration engine.
    *   Define functions for `applyGravity`, `applyConstraints`, `updatePositions`, `handleCollisions`.

*   **`backend/src/websocket-adapter.lisp`:**
    *   Create and manage a central simulation loop (`(bt:make-thread ...)`).
    *   The loop should run at a fixed tick rate (e.g., 50Hz).
    *   Inside the loop: call the physics engine to step the simulation.
    *   After a certain number of steps, create a state snapshot of all dynamic objects.
    *   Broadcast this snapshot to all clients in the room.
    *   The existing `handle-object-update` will now primarily be used for static property changes (e.g., color, physics properties) and initial placement, not for continuous position updates of dynamic objects.

*   **`backend/src/canvas-state.lisp`:**
    *   The object data model must be updated to include the new physics properties (`isDynamic`, `mass`, etc.).

*   **`backend/src/ai-agent.lisp`:**
    *   The `*component-tools*` list needs to be updated with the new function-calling schemas defined in **FR-5.1**.
    *   The logic in `execute-ai-command` will need to dispatch these new tool calls, which will then modify the simulation state.

### 3.2. Frontend (`.js` files)

*   **`frontend/src/physics.js` (New File):**
    *   This file will contain a JavaScript port of the *exact same* deterministic physics engine from the backend. This is crucial for client-side prediction.

*   **`frontend/src/canvas.js` (CanvasManager):**
    *   In the main animation loop (`app.ticker.add(...)`), call the local physics engine to step the simulation for all `isDynamic` objects. This provides the smooth 60 FPS prediction.
    *   Object creation methods (`createRectangle`, etc.) must be updated to handle new physics properties.
    *   New methods to create `GravitySource` and `ParticleEmitter` visualizations.

*   **`frontend/src/websocket.js` (WebSocketClient):**
    *   The `handleMessage` function needs a new case for `state-snapshot` messages.
    *   When a snapshot is received, it must not apply the new positions directly. Instead, it should pass them to the `CanvasManager` for reconciliation.
    *   **Reconciliation Logic (in `CanvasManager`):** For each object in the snapshot, compare the server position with the locally predicted position. If they differ, gently interpolate the local object's position towards the server's authoritative position over a few frames to prevent jarring snaps.

*   **`frontend/src/main.js` & `index.html`:**
    *   Add new UI elements for simulation controls (Play/Pause/Reset).
    *   Add the new "Gravity" and "Emitter" tools to the toolbar.
    *   Add the "Physics" property section to the right-hand sidebar.

## 4. Success Metrics

*   **Performance:** The application maintains a minimum of 55 FPS during simulation with 5 concurrent users and at least 200 dynamic objects on screen.
*   **Synchronization:** Visual divergence between two clients viewing the same simulation is imperceptible (<10ms lag). The simulation remains perfectly in sync.
*   **Functionality:** All features described in this PRD are implemented and functional. Users can collaboratively create and interact with physics-based scenes.
*   **AI Agent:** The AI can successfully execute at least 80% of the example commands listed in **FR-5.2**.
