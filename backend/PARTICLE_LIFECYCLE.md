# Particle Lifecycle Management System

## Overview

The particle lifecycle management system provides automatic cleanup and limiting of particles in the physics engine. This ensures optimal performance when using particle emitters by:

1. **Auto-despawning particles** after their lifespan expires
2. **Capping total particles** at a configurable maximum (default: 500)
3. **Removing oldest particles** when the limit is reached
4. **Tracking particle age** for management and statistics

## Architecture

### Data Structure Extensions

The `physics-object` struct has been extended with three new fields:

```lisp
(defstruct physics-object
  ;; ... existing fields ...
  particle-p      ; T if this is a particle (for lifecycle management)
  birth-time      ; Universal time when particle was created
  lifespan)       ; Lifespan in seconds (NIL = infinite)
```

### Configuration Parameters

```lisp
*max-particles*              ; Maximum particles allowed (default: 500)
*default-particle-lifespan*  ; Default lifespan in seconds (default: 5.0)
```

## API Reference

### Creating Particles

#### `create-particle (id x y radius &key color lifespan initial-vx initial-vy)`
Creates a particle with automatic lifecycle management.

**Parameters:**
- `id` - Unique identifier string
- `x`, `y` - Initial position
- `radius` - Particle size
- `color` - Color hex string (default: "#ffaa00")
- `lifespan` - Lifespan in seconds (default: 5.0)
- `initial-vx`, `initial-vy` - Initial velocity (default: 0.0)

**Example:**
```lisp
(create-particle "particle-001" 100.0 200.0 5.0
                 :color "#ff6600"
                 :lifespan 3.0
                 :initial-vx 50.0
                 :initial-vy -30.0)
```

### Lifecycle Management Functions

#### `cleanup-expired-particles ()`
Removes all particles that have exceeded their lifespan.

**Returns:** Number of particles removed

**Auto-called:** Every physics step

**Example:**
```lisp
(cleanup-expired-particles)  ; => 5 (removed 5 expired particles)
```

#### `enforce-particle-limit ()`
Enforces the maximum particle limit by removing oldest particles.

**Returns:** Number of particles removed

**Auto-called:** Every physics step

**Logs warning when limit exceeded:**
```
[Physics] WARNING: Particle limit reached (520/500). Removing 20 oldest particles
```

#### `count-particles ()`
Returns the current number of particles in the simulation.

**Returns:** Integer count

**Example:**
```lisp
(count-particles)  ; => 237
```

### Particle Query Functions

#### `get-particle-age (particle)`
Calculates the age of a particle in seconds.

**Parameters:**
- `particle` - A physics-object

**Returns:** Age in seconds (integer)

**Example:**
```lisp
(let ((p (get-physics-object "particle-001")))
  (get-particle-age p))  ; => 3 (3 seconds old)
```

#### `is-particle-expired (particle)`
Checks if a particle has exceeded its lifespan.

**Parameters:**
- `particle` - A physics-object

**Returns:** `T` if expired, `NIL` otherwise

**Example:**
```lisp
(let ((p (get-physics-object "particle-001")))
  (is-particle-expired p))  ; => T
```

#### `get-particle-statistics ()`
Returns comprehensive statistics about particles.

**Returns:** Property list with:
- `:total-objects` - Total physics objects
- `:total-particles` - Total particles
- `:regular-objects` - Non-particle objects
- `:oldest-particle-age` - Age of oldest particle
- `:particle-capacity` - Maximum allowed particles
- `:particle-usage-percent` - Percentage of capacity used

**Example:**
```lisp
(get-particle-statistics)
; => (:TOTAL-OBJECTS 156
;     :TOTAL-PARTICLES 150
;     :REGULAR-OBJECTS 6
;     :OLDEST-PARTICLE-AGE 4
;     :PARTICLE-CAPACITY 500
;     :PARTICLE-USAGE-PERCENT 30.0)
```

### Configuration Functions

#### `set-max-particles (max)`
Sets the maximum number of particles allowed.

**Parameters:**
- `max` - Integer maximum

**Example:**
```lisp
(set-max-particles 1000)  ; Allow up to 1000 particles
```

#### `get-max-particles ()`
Returns the current maximum particle limit.

**Returns:** Integer

**Example:**
```lisp
(get-max-particles)  ; => 500
```

## Integration with Physics Loop

The particle lifecycle management is automatically integrated into `physics-step()`:

```lisp
(defun physics-step ()
  "Execute one physics simulation step"
  (unless *physics-paused*
    (let ((dt *physics-timestep*))
      ;; 1. Apply forces
      ;; 2. Integrate positions
      ;; 3. Apply constraints
      ;; 4. Resolve collisions

      ;; 5. Particle lifecycle management
      (cleanup-expired-particles)
      (enforce-particle-limit))))
```

This ensures:
- Expired particles are removed every frame
- Particle limits are enforced every frame
- No manual cleanup required

## Usage Examples

### Basic Particle Creation

```lisp
;; Create a short-lived particle
(create-particle "spark-01" 300.0 200.0 3.0
                 :lifespan 1.0
                 :color "#ffff00")

;; Create a particle with initial velocity
(create-particle "debris-01" 500.0 400.0 8.0
                 :lifespan 2.5
                 :initial-vx 100.0
                 :initial-vy -80.0
                 :color "#cc6600")
```

### Particle Emitter Pattern

```lisp
(defun emit-particles (emitter-x emitter-y rate)
  "Emit particles from a position at given rate"
  (dotimes (i rate)
    (let* ((angle (* 2 pi (random 1.0)))
           (speed (+ 50.0 (random 100.0)))
           (vx (* speed (cos angle)))
           (vy (* speed (sin angle))))
      (create-particle
       (format nil "particle-~D" (get-universal-time))
       emitter-x emitter-y
       (+ 3.0 (random 5.0))
       :lifespan (+ 2.0 (random 3.0))
       :initial-vx vx
       :initial-vy vy
       :color (nth (random 3) '("#ff6600" "#ffaa00" "#ffdd00"))))))
```

### Monitoring Particle Usage

```lisp
(defun log-particle-stats ()
  "Log particle statistics to console"
  (let ((stats (get-particle-statistics)))
    (format t "Particles: ~D/~D (~,1F% capacity)~%"
            (getf stats :total-particles)
            (getf stats :particle-capacity)
            (getf stats :particle-usage-percent))))
```

### Performance Tuning

```lisp
;; For high-performance scenarios, reduce max particles
(set-max-particles 300)

;; For visual richness, increase max particles
(set-max-particles 1000)

;; Get current particle count to monitor performance
(when (> (count-particles) 800)
  (format t "WARNING: High particle count may impact performance~%"))
```

## Performance Considerations

### Memory Management
- Particles are automatically cleaned up when expired
- Old particles are removed when limit is exceeded
- Hash table cleanup is efficient (O(n) where n = particles to remove)

### CPU Usage
- Particle age checking: O(n) per frame
- Expired particle cleanup: O(n) per frame
- Limit enforcement: O(n log n) per frame (sorting oldest particles)

### Recommended Limits
- **Low-end devices:** 300 particles max
- **Medium devices:** 500 particles max (default)
- **High-end devices:** 1000+ particles max

### Optimization Tips
1. Use shorter lifespans for better turnover
2. Reduce emission rate if hitting particle limits
3. Monitor with `get-particle-statistics()` during development
4. Adjust `*max-particles*` based on target platform

## Backwards Compatibility

### Regular Objects
Regular physics objects (created with `create-physics-circle` or `create-physics-rectangle`) are **not affected** by particle lifecycle management:

- `particle-p` is set to `NIL`
- `birth-time` and `lifespan` are `NIL`
- Will never be cleaned up automatically
- Will never count towards particle limit

### Migration
Existing code continues to work without changes. The new particle system is opt-in via `create-particle()`.

## Testing

Run the test suite to verify particle lifecycle functionality:

```bash
cd backend
sbcl --load test-particle-lifecycle.lisp --quit
```

Expected output:
```
=== Testing Particle Lifecycle Management ===

Test 1: Creating particles...
  Created 3 particles and 1 regular objects

Test 2: Counting particles...
  Total particles: 3
  Max particles: 500

Test 3: Testing particle age tracking...
  Particle p1 age: 0 seconds
  Particle p1 expired? NIL

Test 4: Simulating particle expiration...
  Updated p1 birth-time to 3 seconds ago
  Particle p1 age: 3 seconds
  Particle p1 expired? T

Test 5: Cleaning up expired particles...
  Removed 1 expired particle(s)
  Remaining particles: 2

Test 6: Testing particle limit enforcement...
  Set max particles to 5
  Created 10 additional particles
  Total before limit enforcement: 12
  Removed 7 oldest particle(s) to enforce limit
  Total after limit enforcement: 5

Test 7: Particle statistics...
  Total objects: 6
  Total particles: 5
  Regular objects: 1
  Oldest particle age: 0 seconds
  Particle capacity: 5
  Particle usage: 100.0%

=== All Tests Completed ===
```

## Future Enhancements

Potential improvements for the particle lifecycle system:

1. **Particle pools** - Reuse particle objects instead of creating/destroying
2. **Spatial culling** - Remove off-screen particles
3. **Performance-based throttling** - Auto-adjust limits based on frame rate
4. **Particle priorities** - Keep important particles, remove unimportant ones
5. **Age-based fading** - Gradual alpha reduction as particles age
6. **Particle groups** - Group management for particle systems

## Summary

The particle lifecycle management system provides:
- ✅ Automatic cleanup of expired particles
- ✅ Configurable particle limits
- ✅ Oldest-first removal strategy
- ✅ Comprehensive statistics and monitoring
- ✅ Zero-overhead for regular physics objects
- ✅ Integrated into physics loop
- ✅ Production-ready and tested

Use `create-particle()` for auto-managed particles and `create-physics-circle()` for permanent objects.
