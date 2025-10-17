# Living Canvas Physics - Mermaid Diagrams

This directory contains mermaid diagrams documenting the physics feature architecture.

## Diagrams

1. **physics_system_architecture.mermaid** - Overall system architecture showing frontend/backend interaction
2. **physics_simulation_loop.mermaid** - Detailed 50Hz physics loop with 20Hz broadcasts
3. **physics_object_model.mermaid** - Class diagram of object data structures
4. **physics_message_protocol.mermaid** - WebSocket message flow for physics operations
5. **physics_data_flow.mermaid** - Complete data flow from user action to rendering
6. **physics_collision_detection.mermaid** - Collision detection algorithms (circle-circle, circle-rectangle)
7. **physics_emitter_system.mermaid** - Particle emitter lifecycle
8. **physics_frontend_rendering.mermaid** - Client-side rendering strategy with interpolation
9. **physics_state_machine.mermaid** - Physics simulation state transitions
10. **physics_performance_optimization.mermaid** - Performance optimization strategies

## Viewing Diagrams

### In GitHub
These diagrams will render automatically in GitHub markdown viewers.

### Locally with VSCode
Install the "Markdown Preview Mermaid Support" extension.

### Online
Use https://mermaid.live/ to paste and view diagrams.

## Key Architectural Decisions

### Server-Authoritative Physics
- **50Hz fixed timestep** on backend ensures deterministic simulation
- **20Hz broadcast rate** balances smoothness and bandwidth
- Server resolves all collisions and physics interactions

### Client-Side Rendering with Prediction
- **60 FPS rendering** using RequestAnimationFrame
- **Client-side prediction** using identical Verlet physics engine
- **Reconciliation** with 20Hz server snapshots
- **Gentle interpolation** to correct prediction errors smoothly

### Object Model
- **Circles**: Dynamic by default (isDynamic=true), mass ∝ radius² (π*r²)
- **Rectangles**: Always static (isDynamic=false), act as walls/platforms
- **Emitters**: Spawn particles at configurable rates with lifecycle management

### Collision System
- **Verlet integration** for stable position-based physics
- **3 iterations** per timestep for collision resolution
- **Elastic collisions** with configurable restitution (bounciness)

### Performance Strategy
- **Spatial partitioning** for 100+ objects
- **Particle cleanup** to cap at 300-500 max objects
- **Auto-despawn** particles after lifespan expires
- **Remove oldest** particles when limit reached
- **Viewport culling** (already implemented in canvas.js)
