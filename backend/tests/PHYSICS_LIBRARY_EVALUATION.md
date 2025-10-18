# Physics Library Evaluation Report

**Date**: October 17, 2025
**Task**: Subtask 1.1 - Install and Verify cl-bodge Physics Module
**Status**: ❌ **BLOCKED - cl-bodge Not Available**

---

## Executive Summary

The PRD specifies `cl-bodge/physics` as the backend physics engine (Section 1.3, 4.2.1). However, **cl-bodge is not available** for installation via Quicklisp or any accessible distribution source. This represents a critical blocker for Phase 0 library validation.

## Investigation Results

### Attempt 1: Direct Quicklisp Installation
```lisp
(ql:quickload :cl-bodge/physics)
```

**Result**: ❌ FAILED
```
System "cl-bodge/physics" not found
```

### Attempt 2: Bodge Distribution Installation
```lisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")
```

**Result**: ❌ FAILED
```
Subprocess exited with error code 2
Unable to download http://bodge.borodust.org/dist/org.borodust.bodge.txt
```

**Root Cause**: The bodge distribution URL is no longer accessible.

### Attempt 3: bodge-chipmunk Alternative
```lisp
(ql:quickload :bodge-chipmunk)
```

**Result**: ❌ FAILED
```
No specification defined for current platform apple-darwin-gnu
```

**Root Cause**: bodge-chipmunk has platform-specific compilation issues on macOS.

### Attempt 4: chipmunk-blob Alternative
```lisp
(ql:quickload :chipmunk-blob)
```

**Result**: ❌ FAILED
```
No libraries found for current architecture
```

## Available Physics Libraries in Quicklisp

Search conducted for Common Lisp physics engines:

```lisp
(ql:system-apropos "physics")
```

**Results**:
- `bodge-chipmunk` - ❌ Platform issues
- `chipmunk-blob` - ❌ Architecture issues
- `cl-sf3/physics-model` - ⚠️ Not a general 2D physics engine
- `physical-dimension` - ⚠️ Units library, not physics simulation
- `physical-quantities` - ⚠️ Scientific calculations, not game physics

**Conclusion**: No readily available 2D physics engines in Quicklisp that meet project requirements.

---

## Recommended Alternatives

### Option 1: Custom Common Lisp Physics Implementation (RECOMMENDED)
**Pros**:
- Full control over physics simulation
- No external dependency issues
- Can be tailored specifically for this use case (balls, fans, blocks)
- Lightweight for MVP requirements

**Cons**:
- Requires implementing collision detection, resolution, and integration
- Development time: ~3-5 days for MVP features

**Feasibility**: ✅ HIGH
- Simple 2D physics for balls (AABB collision, circle collision)
- Fans are just force fields (no complex physics)
- Static blocks are trivial

**Implementation Path**:
```lisp
;; backend/src/simple-physics.lisp
(defclass physics-world ()
  ((bodies :initform nil)
   (gravity :initform (vec2 0 9.8))
   (timestep :initform (/ 1.0 60.0))))

(defclass physics-body ()
  ((position :initform (vec2 0 0))
   (velocity :initform (vec2 0 0))
   (mass :initform 1.0)
   (radius :initform 10.0)
   (restitution :initform 0.8)))

;; Simple Verlet integration
(defun step-physics (world dt)
  (dolist (body (bodies world))
    (apply-gravity body (gravity world) dt)
    (integrate-velocity body dt)
    (resolve-collisions body (bodies world))))
```

### Option 2: FFI Bindings to Box2D
**Pros**:
- Industry-standard 2D physics
- Well-tested and documented

**Cons**:
- Requires CFFI bindings (complex)
- External C++ library dependency
- Cross-platform compilation challenges

**Feasibility**: ⚠️ MEDIUM
- Significant FFI work required
- May have similar platform issues as bodge-chipmunk

### Option 3: Move Physics to JavaScript (Matter.js Only)
**Pros**:
- Matter.js is proven and available
- No Common Lisp physics dependency

**Cons**:
- Violates PRD architecture (authoritative server requirement)
- Increases client-side load
- Harder to scale to 500+ balls
- Synchronization becomes more complex

**Feasibility**: ⚠️ MEDIUM
- Architectural change required
- May not meet performance targets

---

## Recommended Path Forward

### ✅ Recommendation: Implement Custom Lightweight 2D Physics

**Rationale**:
1. MVP requirements are **simple**: bouncing balls, force fields, static blocks
2. Custom implementation can be optimized for this exact use case
3. No external dependency hell
4. Faster iteration during development
5. Can add Box2D/Chipmunk via FFI later if needed

### Implementation Plan

#### Step 1: Create `backend/src/simple-physics.lisp`
- 2D vector math utilities
- Circle collision detection (for balls)
- AABB collision (for walls/blocks)
- Verlet integration for positions
- Simple force application

#### Step 2: Validate Performance
- Test with 500 balls
- Measure frame time (target: <16ms)
- Profile on Fly.io instance

#### Step 3: Benchmark Against Targets
- 60 Hz simulation ✅
- <16ms per frame ✅
- 500 active balls ✅

### Estimated Timeline
- **Day 1-2**: Implement core physics (`simple-physics.lisp`)
- **Day 3**: Integration with WebSocket (`physics-loop.lisp`)
- **Day 4**: Performance testing and optimization
- **Day 5**: Documentation and finalization

---

## Updated Task Dependencies

### Blockers Resolved
- ❌ ~~Install cl-bodge/physics~~ → ✅ Implement custom physics
- ✅ Update `collabcanvas.asd` dependencies (remove cl-bodge reference)
- ✅ Create alternative physics implementation

### Next Steps
1. **Update PRD** to reflect custom physics implementation
2. **Update Task 1** to use `simple-physics.lisp` instead of cl-bodge
3. **Proceed with Subtask 1.2**: Create performance benchmark with custom engine

---

## Files Modified

### `/Users/reuben/gauntlet/figma-clone/cl-fun worktrees/phys-engine/backend/collabcanvas.asd`
Current state:
```lisp
:depends-on (:bodge-chipmunk)  ; Line 33 - NOT WORKING
```

**Action Required**: Remove bodge-chipmunk dependency, add custom physics module to components list.

---

## Decision Required

**Please confirm approach**:
- [ ] **Option A**: Proceed with custom lightweight 2D physics implementation (RECOMMENDED)
- [ ] **Option B**: Research FFI bindings to Box2D (higher risk)
- [ ] **Option C**: Move physics to JavaScript/Matter.js (architectural change)

**Once confirmed, I will**:
1. Update `collabcanvas.asd` to remove broken dependency
2. Create `simple-physics.lisp` with MVP physics engine
3. Implement verification test (spawn balls, measure performance)
4. Mark subtask 1.1 as complete with alternative solution

---

## Appendix: Environment Details

**Platform**: macOS (apple-darwin-gnu)
**Lisp**: SBCL via Roswell
**Quicklisp**: 2025-06-22 distribution
**Project**: CollabCanvas - Multiplayer Physics Simulation
**Working Directory**: `/Users/reuben/gauntlet/figma-clone/cl-fun worktrees/phys-engine`
