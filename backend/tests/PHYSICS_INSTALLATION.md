# Physics Engine Installation Guide
## Task 1.1 - cl-bodge Physics Module Installation

### Date: October 17, 2025
### Status: Investigation Complete - Alternative Approach Required

---

## Investigation Summary

### Original Plan: cl-bodge/physics
The PRD specified using `cl-bodge/physics` as the backend physics engine, installed via:
```lisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")
(ql:quickload :cl-bodge/physics)
```

### Issues Discovered

1. **Bodge Distribution Unavailable**:
   - URL `http://bodge.borodust.org/dist/org.borodust.bodge.txt` returns errors
   - Cannot install cl-bodge distribution via Quicklisp
   - Error: `SUBPROCESS-ERROR` when attempting download

2. **bodge-chipmunk Compilation Errors**:
   - Available in Quicklisp as `bodge-chipmunk`
   - Compilation fails during CLAW binding generation
   - Error: `ASDF/LISP-ACTION:COMPILE-OP` failure

3. **Available Alternatives**:
   - `bodge-chipmunk` - Chipmunk2D bindings (compilation issues)
   - `bodge-ode` - ODE physics bindings (not tested)
   - `chipmunk-blob` - Alternative Chipmunk bindings (not tested)
   - `cl-sf3/physics-model` - Simple physics model (not suitable for 2D game physics)

---

## Recommended Next Steps

### Option 1: Fix bodge-chipmunk (Preferred if possible)
Investigation needed to resolve compilation errors. Potential causes:
- CFFI/CLAW version compatibility
- Missing system dependencies (Chipmunk2D library)
- Platform-specific build issues (macOS ARM64)

**Action Items**:
1. Check system Chipmunk2D installation: `brew install chipmunk`
2. Verify CFFI/CLAW versions
3. Test on x86_64 Linux (Fly.io deployment target)
4. Contact bodge maintainer if needed

### Option 2: Alternative Physics Engine
Switch to a more actively maintained Common Lisp physics library:

**Candidates**:
1. **chipmunk-blob**: Alternative Chipmunk2D bindings
   - May have better macOS ARM64 support
   - Requires testing

2. **Write Custom Box2D Bindings**:
   - Box2D is industry-standard (used in Angry Birds, etc.)
   - CFFI bindings could be written
   - High effort, but full control

3. **Use Chipmunk2D C Library Directly**:
   - Write minimal CFFI bindings ourselves
   - Only implement features we need (balls, rectangles, forces)
   - Lower complexity than full library

### Option 3: Server-Side Matter.js (Node.js)
**Radical Alternative**: Run Matter.js on server via Node.js sidecar
- CollabCanvas backend calls Node.js physics service via HTTP/IPC
- Lisp handles WebSocket routing, Node handles physics simulation
- Tradeoff: Additional process, language boundary overhead
- Benefit: Well-tested library, easier development

---

## Current State

### Files Modified
1. **backend/collabcanvas.asd**:
   - Added `:bodge-chipmunk` dependency (commented with caveat)
   - Noted cl-bodge/physics unavailability

2. **backend/tests/cl-bodge-verification.lisp**:
   - Created verification test suite
   - Tests package loading, basic API exploration
   - Currently FAILS due to library unavailability

3. **backend/tests/PHYSICS_INSTALLATION.md** (this file):
   - Documentation of investigation
   - Alternative approaches

### Installation Commands Tested

```bash
# Attempted: Install bodge distribution
ros -e '(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt" :prompt nil)'
# Result: FAILED - URL unreachable

# Attempted: Load cl-bodge/physics directly
ros -e '(ql:quickload :cl-bodge/physics)'
# Result: FAILED - System not found

# Attempted: Load bodge-chipmunk
ros -e '(ql:quickload :bodge-chipmunk)'
# Result: FAILED - Compilation error in CLAW bindings
```

---

## Decision Required

**This task (1.1) is BLOCKED pending decision**:

1. Should we investigate fixing bodge-chipmunk compilation?
2. Should we switch to alternative physics library?
3. Should we revise architecture to use Node.js/Matter.js on server?
4. Should we contact bodge maintainer for distribution access?

**Recommendation**:
- Test on Linux x86_64 (Fly.io environment) before abandoning bodge-chipmunk
- macOS ARM64 compilation issues may not affect production deployment
- Worst case: Use Node.js Matter.js sidecar (proven, fast to implement)

---

## Testing Plan (Once Library Available)

When physics library is working, run:
```bash
cd backend
ros --load tests/cl-bodge-verification.lisp \
    --eval '(cl-bodge-verification:run-all-tests)' \
    --quit
```

Expected output:
```
╔════════════════════════════════════════════════════╗
║  CL-BODGE/PHYSICS VERIFICATION TEST SUITE        ║
║  Subtask 1.1 - Library Installation Validation  ║
╚════════════════════════════════════════════════════╝

=== Test 1: Loading cl-bodge/physics ===
✓ Successfully loaded cl-bodge/physics

=== Test 2: Creating Physics Universe ===
✓ Found cl-bodge.physics package
✓ Basic physics package verification complete

=== Test 3: Verifying Package Exports ===
✓ cl-bodge.physics package exists

╔════════════════════════════════════════════════════╗
║  TEST RESULTS                                    ║
╚════════════════════════════════════════════════════╝

Tests passed: 3/3

✓ ALL TESTS PASSED - cl-bodge/physics is properly installed
```

---

## References

- **PRD**: `.taskmaster/docs/prd-phys-engine.md` (Section 4.2.1)
- **Bodge GitHub**: https://github.com/borodust/bodge
- **Chipmunk2D**: https://chipmunk-physics.net/
- **Matter.js**: https://brm.io/matter-js/ (frontend + potential server option)

---

**Status**: Task 1.1 marked IN-PROGRESS, awaiting architecture decision
**Next Action**: Consult with team on physics engine strategy
