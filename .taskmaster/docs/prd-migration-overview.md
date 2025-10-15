# CollabCanvas Backend Migration: Architecture Overview

**Document Type:** Master Migration Plan
**Version:** 1.0
**Last Updated:** October 2025
**Status:** Planning

---

## Executive Summary

This document provides a high-level overview of the four major backend architectural improvements for CollabCanvas. These migrations transform the system from a proof-of-concept to a production-ready, scalable, and feature-rich collaborative design platform.

### The Four Pillars

```
┌─────────────────────────────────────────────────────────────────┐
│                  CollabCanvas Backend Evolution                  │
├──────────────────┬──────────────────┬──────────────────┬────────┤
│   Foundation     │   Scalability    │  Differentiator  │Security│
│                  │                  │                  │        │
│  Clack/Woo       │  SQLite Message  │  AI Component    │ Auth0  │
│  Async Server    │  Queue           │  Generation      │ OAuth2 │
│                  │                  │                  │        │
│  10x Capacity    │  Multi-Instance  │  10x Faster      │  Zero  │
│  Event-Driven    │  Distributed     │  Prototyping     │Password│
│                  │                  │                  │Handling│
└──────────────────┴──────────────────┴──────────────────┴────────┘
```

---

## Component Breakdown

### 1. Clack/Woo Async Architecture (Foundation)

**File:** [`prd-clack-woo-migration.md`](./prd-clack-woo-migration.md)

**Why This First:**
This is the foundational change that enables everything else. Without async I/O, the system cannot scale to thousands of concurrent WebSocket connections.

**Impact:**
- **10x connection capacity**: 100 → 1,000+ concurrent WebSocket connections
- **Reduced latency**: Event-driven architecture eliminates thread context switching
- **Memory efficiency**: <500KB per connection vs. 1-2MB with threads
- **Modern architecture**: Aligns with Node.js, Go, and Rust patterns

**Dependencies:**
- Required before: SQLite Message Queue (both use async I/O paradigm)
- Compatible with: All other features

**Timeline:** Weeks 1-2 (12-16 hours of work)

---

### 2. SQLite Message Queue (Scalability)

**File:** [`prd-sqlite-message-queue.md`](./prd-sqlite-message-queue.md)

**Why After Clack/Woo:**
The async architecture makes polling efficient. With threads, polling would waste CPU on thread wakeups.

**Impact:**
- **Multi-instance deployment**: Run 2-10 server processes behind Nginx
- **Message persistence**: Messages survive server restarts
- **No external dependencies**: Redis/RabbitMQ not needed
- **Horizontal scaling**: Foundation for cloud deployment

**Dependencies:**
- Requires: Clack/Woo async architecture
- Complements: All other features

**Timeline:** Weeks 2-3 (8-12 hours of work)

**Trade-offs:**
- Adds 50-100ms latency vs. in-memory broadcasting
- Acceptable for collaboration (users won't notice)
- Can be optimized with Redis later if needed

---

### 3. AI Component Generation (Differentiator)

**File:** [`prd-ai-component-generation.md`](./prd-ai-component-generation.md)

**Why This Matters:**
This is the "wow factor" that sets CollabCanvas apart from Figma, Miro, and other tools. Natural language UI generation is a game-changer for rapid prototyping.

**Impact:**
- **10x faster prototyping**: Create complex layouts in seconds
- **Product differentiation**: Unique selling point
- **Accessibility**: Non-technical users can create professional UIs
- **Extensibility**: Foundation for AI-assisted design (style transfer, a11y checks)

**Dependencies:**
- Works with: Any architecture (can be added anytime)
- Enhanced by: Clack/Woo (async API calls don't block server)

**Timeline:** Weeks 3-4 (16-20 hours of work)

**Costs:**
- Claude API: ~$0.01 per command
- Estimated: $30-100/month for 3,000-10,000 commands

---

### 4. Auth0 OAuth2 Integration (Security)

**File:** [`prd-auth0-oauth2.md`](./prd-auth0-oauth2.md)

**Why Production Requires This:**
The current custom authentication is insecure (SHA-256 without salt, no MFA, no password reset). Auth0 provides enterprise-grade security without operational burden.

**Impact:**
- **Zero password handling**: No PII liability
- **Social login**: Google, GitHub, Microsoft out-of-the-box
- **Enterprise features**: SSO, MFA, breach detection
- **Compliance ready**: SOC2, GDPR, HIPAA foundations

**Dependencies:**
- Independent: Can be added anytime
- Recommended: After Clack/Woo (cleaner OAuth flow with middleware)

**Timeline:** Weeks 4-5 (12-16 hours of work)

**Costs:**
- Free: Up to 7,500 monthly active users
- $240-500/month: For 10,000+ users

---

## Dependency Graph

```
┌─────────────────────────────────────────────────────────────────┐
│                      Implementation Order                        │
└─────────────────────────────────────────────────────────────────┘

Week 1-2: Clack/Woo Migration (Foundation)
  │
  ├─ Enables async I/O
  ├─ Unlocks high concurrency
  └─ Required for efficient polling
     │
     ▼
Week 2-3: SQLite Message Queue (Scalability)
  │
  ├─ Builds on async architecture
  ├─ Enables multi-instance deployment
  └─ Independent of other features
     │
     ▼
Week 3-4: AI Component Generation (Differentiator)
  │        [Parallel with Auth0]
  ├─ Can start anytime
  ├─ Async API calls benefit from Woo
  └─ Independent feature
     │
     ▼
Week 4-5: Auth0 OAuth2 (Security)
          [Parallel with AI Agent]
  │
  ├─ Can start anytime
  ├─ Middleware-friendly with Clack
  └─ Production requirement

Week 6: Integration Testing & Production Deployment
```

---

## Architecture Vision: Before vs. After

### Current Architecture (Proof-of-Concept)

```
┌─────────────────────────────────────────────────────────┐
│                    Hunchentoot                          │
│                (Thread-per-connection)                  │
│                                                         │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐               │
│  │Thread 1 │  │Thread 2 │  │Thread N │  ... (max 100) │
│  │  WS 1   │  │  WS 2   │  │  WS N   │               │
│  └─────────┘  └─────────┘  └─────────┘               │
│                                                         │
│  In-Memory Room Broadcast (single process only)        │
│                                                         │
│  Custom Email/Password Auth (insecure SHA-256)         │
│                                                         │
│  No AI Features                                        │
└─────────────────────────────────────────────────────────┘

Limitations:
- Max ~100 concurrent connections
- Single server instance only
- No failover or load balancing
- Security vulnerabilities
- Manual UI creation only
```

### Target Architecture (Production-Ready)

```
┌────────────────────────────────────────────────────────────────┐
│                     Load Balancer (Nginx)                       │
│                 Round-robin WebSocket routing                   │
└───────────┬────────────────────┬───────────────────────────────┘
            │                    │
      ┌─────▼─────┐        ┌─────▼─────┐
      │  Woo 1    │        │  Woo 2    │   ... (N instances)
      │  (Async)  │        │  (Async)  │
      │           │        │           │
      │ 1000+ WS  │        │ 1000+ WS  │
      └─────┬─────┘        └─────┬─────┘
            │                    │
            └──────────┬─────────┘
                       │
             ┌─────────▼─────────┐
             │  SQLite Message   │
             │  Queue (Shared)   │
             │                   │
             │  ┌──────────────┐ │
             │  │ message_queue│ │
             │  │ canvas_states│ │
             │  │ users        │ │
             │  └──────────────┘ │
             └───────────────────┘

Features:
✅ 10,000+ concurrent connections
✅ Multi-instance deployment
✅ Horizontal scaling
✅ Message persistence
✅ Async I/O (libev)

       ┌─────────────────────────┐
       │   Claude API (AI Agent) │
       │                         │
       │  Natural Language UI    │
       │  Component Generation   │
       └─────────────────────────┘

       ┌─────────────────────────┐
       │      Auth0 OAuth2       │
       │                         │
       │  Social Login + SSO     │
       │  Enterprise Security    │
       └─────────────────────────┘
```

---

## Performance Comparison

| Metric | Current | Target | Improvement |
|--------|---------|--------|-------------|
| Max concurrent WebSocket connections | 100 | 10,000+ | **100x** |
| Memory per connection | ~2MB | <500KB | **4x** |
| Multi-instance support | ❌ | ✅ | **Enabled** |
| Message persistence | ❌ | ✅ | **Enabled** |
| WebSocket latency (P95) | 50ms | <10ms | **5x** |
| UI prototyping time | 5-10 min | 30 sec | **10-20x** |
| Authentication security | ⚠️ Weak | ✅ Enterprise | **Critical** |
| Password handling | ❌ Unsafe | ✅ Zero PII | **Compliant** |

---

## Implementation Roadmap

### Week 1-2: Foundation (Clack/Woo)

**Goals:**
- Replace Hunchentoot with Woo
- Migrate HTTP routes to Clack middleware
- Port WebSocket handlers to async model
- Performance test (100+ concurrent connections)

**Deliverables:**
- [ ] `src/app.lisp` - Clack application
- [ ] `src/websocket-adapter.lisp` - WebSocket bridge
- [ ] `src/server.lisp` - Woo lifecycle
- [ ] Load test results (wrk + thor)

**Success Criteria:**
- All HTTP endpoints work
- All WebSocket messages flow correctly
- 100+ concurrent connections stable
- Memory <500KB per connection

---

### Week 2-3: Scalability (Message Queue)

**Goals:**
- Add message queue tables to database
- Implement enqueue/dequeue logic
- Create polling threads per canvas room
- Deploy 2 instances behind Nginx

**Deliverables:**
- [ ] `db/migrations/001_message_queue.sql`
- [ ] `src/message-queue.lisp` - Queue operations
- [ ] `src/polling-thread.lisp` - Background polling
- [ ] `src/queue-cleanup.lisp` - Expiry cleanup

**Success Criteria:**
- Messages propagate across instances
- Latency <100ms P95
- No message loss
- Cleanup removes expired messages

---

### Week 3-4: Differentiator (AI Agent)

**Goals:**
- Integrate Claude API
- Implement component builders (login form, button, card, etc.)
- Create theme system
- Add frontend command input UI

**Deliverables:**
- [ ] `src/ai-agent.lisp` - Claude API client
- [ ] `src/components.lisp` - Component builders
- [ ] `src/themes.lisp` - Color palettes
- [ ] Frontend: AI command dialog

**Success Criteria:**
- "Create a button" works
- "Create a login form" generates 5+ objects
- Response time <3 seconds P95
- Rate limiting prevents abuse

---

### Week 4-5: Security (Auth0)

**Goals:**
- Configure Auth0 tenant
- Implement OAuth2 flow
- Add database schema changes
- Migrate existing users (optional linking)

**Deliverables:**
- [ ] `src/auth0-config.lisp` - Configuration
- [ ] `src/auth0-oauth.lisp` - OAuth handlers
- [ ] `db/migrations/002_auth0.sql`
- [ ] Frontend: Auth0 login UI

**Success Criteria:**
- New users can sign up via Auth0
- Social login (Google, GitHub) works
- JWT validation correct
- Existing users can link accounts

---

### Week 6: Integration & Deployment

**Goals:**
- Full integration testing
- Performance testing
- Production deployment
- Monitoring setup

**Tasks:**
- [ ] End-to-end testing (all features)
- [ ] Load test with realistic workload
- [ ] Deploy to staging
- [ ] Deploy to production
- [ ] Monitor for 48 hours

---

## Risk Mitigation

### Risk 1: Breaking Changes During Migration

**Mitigation:**
- Feature flags for each component
- Parallel implementation (keep old code)
- Incremental rollout (HTTP → WebSocket → Production)
- Comprehensive integration tests

### Risk 2: SQLite Locking Under Load

**Mitigation:**
- WAL mode (concurrent reads during writes)
- Connection pool with exclusive writes
- Queue writes to background thread if needed
- Monitor SQLite busy errors

### Risk 3: Auth0 API Failures

**Mitigation:**
- Timeout and retry logic
- Fallback to cached JWKS
- Graceful degradation (show error, allow retry)
- Monitoring and alerts

### Risk 4: Cost Overruns (Claude API)

**Mitigation:**
- Rate limiting (10 commands/user/hour)
- Use Haiku for simple commands ($0.25/$1.25 per million)
- Cache system prompt (50% savings)
- Monitor usage daily

---

## Cost Analysis

### Development Time

| Component | Hours | Developer Cost (@$150/hr) |
|-----------|-------|---------------------------|
| Clack/Woo Migration | 12-16 | $1,800-2,400 |
| SQLite Message Queue | 8-12 | $1,200-1,800 |
| AI Component Generation | 16-20 | $2,400-3,000 |
| Auth0 OAuth2 | 12-16 | $1,800-2,400 |
| Integration & Testing | 8-12 | $1,200-1,800 |
| **Total** | **56-76** | **$8,400-11,400** |

### Operational Costs (Monthly)

| Service | Free Tier | Paid Tier | Notes |
|---------|-----------|-----------|-------|
| Claude API | N/A | $30-100 | 3K-10K commands @ $0.01 each |
| Auth0 | 7,500 MAU | $240-500 | Upgrade at >7,500 users |
| Infrastructure | N/A | $50-200 | Fly.io or similar |
| **Total (MVP)** | **$0** | **$80-300** | With free tiers |
| **Total (Scale)** | **N/A** | **$320-800** | At 10K users |

### ROI Analysis

**Without Migration:**
- Security incidents: High risk (data breach = $100K+)
- Scalability: Limited to 100 users (growth ceiling)
- Differentiation: Generic tool (low conversion)

**With Migration:**
- Security: Enterprise-grade (breach risk → near zero)
- Scalability: 10,000+ users (10x revenue potential)
- Differentiation: AI-powered (2x conversion rate)

**Estimated ROI:** 5-10x within 6 months

---

## Success Metrics

### Technical Metrics

| Metric | Current | Target | Measurement |
|--------|---------|--------|-------------|
| Concurrent WebSocket connections | 100 | 1,000+ | Load test |
| Message latency (P95) | 50ms | <100ms | WebSocket ping |
| Memory per connection | 2MB | <500KB | Process monitor |
| Uptime | 95% | 99.5% | Monitoring |
| Security score | C | A+ | OWASP audit |

### Business Metrics

| Metric | Current | Target | Measurement |
|--------|---------|--------|-------------|
| Monthly active users | 50 | 500+ | Analytics |
| Conversion rate | 2% | 5%+ | Funnel |
| Time to prototype | 10 min | 1 min | User testing |
| Churn rate | 20% | <10% | Cohort analysis |
| Revenue per user | $5 | $15+ | Billing |

---

## Documentation Updates Required

### Code Documentation

- [ ] Update `README.md` with new architecture
- [ ] Update `COLLABCANVAS_ARCHITECTURE.md`
- [ ] Create `docs/clack-migration-guide.md`
- [ ] Create `docs/message-queue-design.md`
- [ ] Create `docs/ai-agent-guide.md`
- [ ] Create `docs/auth0-setup-guide.md`

### Operational Documentation

- [ ] Deployment guide (Nginx + multiple instances)
- [ ] Monitoring and alerting setup
- [ ] Incident response playbook
- [ ] Scaling guide (when to add instances)

### User Documentation

- [ ] AI command examples and tutorials
- [ ] Social login setup guide
- [ ] Collaboration best practices

---

## Conclusion

These four architectural improvements transform CollabCanvas from a proof-of-concept into a production-ready, scalable, and differentiated product:

1. **Clack/Woo**: Unlocks high-performance async I/O
2. **Message Queue**: Enables horizontal scaling and resilience
3. **AI Agent**: Provides 10x faster prototyping and differentiation
4. **Auth0**: Delivers enterprise security without operational burden

**Total Timeline:** 6 weeks
**Total Cost:** $8,400-11,400 (dev) + $80-800/month (ops)
**Expected Impact:** 10x capacity, 10x faster prototyping, production security

The migration is designed to be incremental, with each component providing value independently while building toward the final architecture vision.

---

**Document Version:** 1.0
**Last Reviewed:** October 2025
**Next Review:** Weekly during implementation
**Owner:** Engineering Team

**Related Documents:**
- [PRD: Clack/Woo Migration](./prd-clack-woo-migration.md)
- [PRD: SQLite Message Queue](./prd-sqlite-message-queue.md)
- [PRD: AI Component Generation](./prd-ai-component-generation.md)
- [PRD: Auth0 OAuth2](./prd-auth0-oauth2.md)
