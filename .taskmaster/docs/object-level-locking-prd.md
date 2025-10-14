# Object-Level Locking PRD

## Overview

The current collaborative canvas application uses pessimistic locking at the canvas level, which prevents multiple users from editing simultaneously even when working on different objects. This creates a poor user experience where users must wait for others to finish editing before they can make changes, significantly limiting collaboration efficiency.

## Problem Statement

The current collaborative canvas application uses pessimistic locking at the canvas level, which prevents multiple users from editing simultaneously even when working on different objects. This creates a poor user experience where users must wait for others to finish editing before they can make changes, significantly limiting collaboration efficiency.

## Current State

- Canvas-level pessimistic locking blocks entire canvas during any edit
- Users experience blocking when trying to edit simultaneously
- No conflict resolution mechanism – last writer wins silently
- Single-user editing workflow despite collaborative intent

## Proposed Solution

Implement object-level optimistic locking to allow concurrent editing of different canvas objects while preventing conflicts on the same object through version-based conflict detection and resolution.

## Goals & Objectives

### Primary Goals

1. **Enable concurrent editing** – Multiple users can edit different objects simultaneously
2. **Prevent silent data loss** – Detect and handle conflicts when same object is edited
3. **Maintain data integrity** – Ensure consistent canvas state across all clients
4. **Improve user experience** – Reduce blocking and waiting in collaborative sessions

### Success Criteria

- 80% reduction in edit blocking scenarios
- Zero silent data overwrites
- <5% performance degradation vs current system
- Clear conflict resolution UX for users

## Requirements

### Functional Requirements

#### Core Features

1. **Object Version Tracking**
   - Each canvas object gets a version number
   - Version increments on every successful update
   - Version stored in database and cached in memory

2. **Conflict Detection**
   - Check object version before applying updates
   - Detect concurrent modifications to same object
   - Prevent updates with stale version numbers

3. **Conflict Resolution UI**
   - Visual indicator when conflict detected
   - Show conflicting changes side-by-side
   - Allow user to choose resolution strategy:
     - Accept their changes (overwrite)
     - Accept other's changes (discard theirs)
     - Manual merge (for compatible changes)

4. **Real-time Synchronization**
   - Immediate updates for non-conflicting changes
   - Conflict notifications pushed to affected clients
   - Automatic refresh of resolved conflicts

#### User Workflows

1. **Normal Editing** – User edits object, changes apply immediately if no conflict
2. **Conflict Detection** – User attempts edit, conflict detected, resolution UI appears
3. **Conflict Resolution** – User chooses resolution, changes applied or merged
4. **Concurrent Editing** – Multiple users edit different objects simultaneously without issues

### Non-Functional Requirements

#### Performance

- Conflict check overhead <10ms per update
- Memory usage increase <20% for version tracking
- Database query performance maintained
- Real-time update latency <100ms

#### Scalability

- Support 100+ concurrent users per canvas
- Handle 1000+ objects per canvas
- Maintain performance under high conflict rates

#### Reliability

- No data loss during conflicts
- Consistent state across all clients
- Graceful degradation during network issues
- Comprehensive error handling

## Technical Design

### Database Schema Changes

Add version tracking to canvas objects:

```sql
-- Add version column to canvas_objects
ALTER TABLE canvas_objects ADD COLUMN version INTEGER DEFAULT 1;
ALTER TABLE canvas_objects ADD COLUMN last_modified_by INTEGER REFERENCES users(id);
ALTER TABLE canvas_objects ADD COLUMN last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP;

-- Conflict log table
CREATE TABLE object_conflicts (
  id SERIAL PRIMARY KEY,
  canvas_id TEXT NOT NULL,
  object_id TEXT NOT NULL,
  user_id INTEGER REFERENCES users(id),
  client_version INTEGER NOT NULL,
  server_version INTEGER NOT NULL,
  attempted_changes JSON,
  conflict_type TEXT, -- 'version_mismatch', 'concurrent_edit'
  resolved_at TIMESTAMP,
  resolution_type TEXT -- 'overwrite', 'discard', 'merge'
);
```

### Backend Changes

1. **Object Version Management**
   - Extend canvas-state.lisp with version tracking
   - Update websocket.lisp conflict detection logic
   - Modify database.lisp save operations

2. **Conflict Detection Engine**
   - Version comparison before updates
   - Conflict logging and notification
   - Resolution state management

3. **WebSocket Protocol Updates**
   - Add version fields to update messages
   - New conflict notification message types
   - Resolution confirmation messages

### Frontend Changes

1. **Conflict Detection UI**
   - Visual indicators on conflicting objects
   - Modal or sidebar for conflict resolution
   - Side-by-side diff view

2. **Locking Visualization**
   - Show which objects are being edited by others
   - Display user avatars/names on locked objects
   - Real-time lock status updates

3. **Resolution Workflows**
   - Simple "accept/reject" buttons
   - Preview before applying resolution
   - Undo capability after resolution

### WebSocket Message Protocol

```javascript
// Client → Server: Update with version
{
  type: 'object-update',
  objectId: 'obj-123',
  version: 5,
  updates: {
    x: 100,
    y: 200,
    width: 50
  }
}

// Server → Client: Update successful
{
  type: 'object-update-success',
  objectId: 'obj-123',
  version: 6
}

// Server → Client: Conflict detected
{
  type: 'object-conflict',
  objectId: 'obj-123',
  clientVersion: 5,
  serverVersion: 6,
  serverState: {...},
  attemptedChanges: {...},
  conflictingUser: 'user@example.com'
}

// Client → Server: Conflict resolution
{
  type: 'resolve-conflict',
  objectId: 'obj-123',
  resolution: 'overwrite', // or 'discard', 'merge'
  finalState: {...}
}
```

## Implementation Plan

### Phase 1: Backend Foundation (Priority: High)
- Add version tracking to database schema
- Implement version checking in canvas-state.lisp
- Update websocket message handlers for version fields
- Add conflict logging

### Phase 2: Conflict Detection (Priority: High)
- Implement version comparison logic
- Create conflict detection engine
- Add conflict notification system
- Test with concurrent edits

### Phase 3: Frontend UI (Priority: High)
- Design conflict resolution modal
- Implement visual indicators
- Add resolution action handlers
- Update WebSocket client for new messages

### Phase 4: Advanced Features (Priority: Medium)
- Object locking visualization (show who's editing)
- Automatic conflict resolution for non-overlapping changes
- Conflict history and analytics
- Performance optimization

### Phase 5: Testing & Refinement (Priority: High)
- Load testing with multiple concurrent users
- Edge case testing (network issues, race conditions)
- User acceptance testing
- Performance profiling and optimization

## Testing Strategy

### Unit Tests
- Version increment logic
- Conflict detection algorithm
- Resolution state transitions

### Integration Tests
- WebSocket message flow
- Database transaction handling
- Multi-client synchronization

### End-to-End Tests
- Concurrent editing scenarios
- Conflict resolution workflows
- Network failure recovery
- High-load performance

## Success Metrics

- **Edit blocking reduction**: Target 80% fewer blocked edits
- **Data integrity**: Zero silent overwrites
- **Performance**: <5% latency increase
- **User satisfaction**: Measured through feedback surveys
- **Conflict rate**: Monitor and optimize for <10% conflict rate
- **Resolution time**: Average <5 seconds per conflict

## Risks & Mitigations

### Technical Risks
- **Race conditions**: Use database transactions and optimistic locking
- **Performance degradation**: Implement efficient caching and indexing
- **Complex conflicts**: Provide clear UI and escape hatches

### User Experience Risks
- **Confusion**: Provide clear conflict explanations and help text
- **Disruption**: Make resolution process quick and intuitive
- **Learning curve**: Add tooltips and onboarding guide

## Future Enhancements

- AI-powered automatic conflict resolution
- Branching and merging for complex scenarios
- Time-travel debugging for conflicts
- Analytics dashboard for collaboration patterns
