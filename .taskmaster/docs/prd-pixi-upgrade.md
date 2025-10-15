Of course. This is an excellent task. Upgrading a core dependency like PixiJS is a significant undertaking that requires careful planning. A well-structured Product Requirements Document (PRD) is the perfect tool to ensure the migration is successful, comprehensive, and takes full advantage of the new version's benefits.

Here is a detailed PRD for upgrading the Common Lisp project's frontend from PixiJS v7 to v8, incorporating lessons from the provided migration guide and performance tips.

---

### **Product Requirements Document: PixiJS v8 Upgrade**

| **Document ID:** | PRD-PIXI-V8-UPGRADE | **Version:** | 1.0 |
| :--------------- | :------------------ | :----------- | :-- |
| **Date:**        | October 15, 2025    | **Author:**  | AI Architect |
| **Project:**     | CollabCanvas        | **Status:**  | Proposed |
| **Stakeholders:**| Lead Developer, Product Owner, QA Team |

---

### **1. Overview**

This document outlines the requirements for upgrading the frontend of the CollabCanvas application from PixiJS version 7 to version 8. The primary goals of this migration are to enhance rendering performance, modernize the codebase by adopting the new asynchronous and streamlined APIs of PixiJS v8, and lay a future-proof foundation for potential WebGPU integration.

This upgrade is a critical technical initiative that will improve developer experience, reduce bundle size through better modularity, and ensure the long-term maintainability of the rendering engine.

### **2. Scope**

#### **2.1. In-Scope**

*   Update all PixiJS dependencies from v7 to v8 in `package.json`.
*   Refactor the application's initialization logic to accommodate the new asynchronous `app.init()` model.
*   Migrate all `Graphics` API calls to the new v8 "builder" pattern (`shape().fill().stroke()`).
*   Refactor all code that manually creates textures to use the new `TextureSource` model.
*   Update custom shaders and filters (if any) to the new resource-based format.
*   Address all breaking changes and deprecated APIs as outlined in the official v8 migration guide.
*   Implement specific performance optimizations recommended by PixiJS, including the new `CullerPlugin`.
*   Conduct thorough functional and performance testing to ensure no regressions.

#### **2.2. Out-of-Scope**

*   Implementing new canvas features (e.g., new shapes, drawing tools) not directly related to the upgrade.
*   Implementing the AI Agent or other backend features.
*   A full migration to WebGPU. This upgrade will make the application WebGPU-ready, but the actual implementation is a separate initiative.

### **3. Rationale: Why Upgrade to PixiJS v8?**

The upgrade from v7 to v8 provides significant benefits that align with our goals of building a high-performance, feature-rich, and scalable application.

1.  **Performance Enhancements:**
    *   The core rendering logic has been overhauled for significant speed improvements.
    *   The new `ParticleContainer` allows for rendering vastly more particles, which can be leveraged for UI effects or complex visualizations.
    *   A new, more controllable culling system allows for fine-grained performance tuning in large scenes.

2.  **Modernization and Future-Proofing:**
    *   Asynchronous initialization and WebGPU support prepare the application for the future of web graphics.
    *   The streamlined, single-package structure (`import { ... } from 'pixi.js'`) simplifies dependency management.
    *   The API has been made more intuitive and consistent (e.g., object-based constructors, new Graphics API).

3.  **Developer Experience and Maintainability:**
    *   The new Graphics API is more readable and chainable.
    *   Custom builds allow for smaller bundle sizes by including only necessary components.
    *   A clearer separation of concerns (e.g., `TextureSource`) makes texture management more robust.

### **4. Technical Requirements & Implementation Plan**

The migration shall be executed by addressing the following technical changes in the frontend codebase, primarily within `frontend/src/canvas.js` and `frontend/src/main.js`.

#### **4.1. Core Application Refactoring**

*   **Requirement 4.1.1: Update Package Structure**
    *   The `package.json` file must be updated to remove all individual `@pixi/*` sub-packages.
    *   A single dependency on `"pixi.js": "^8.x.x"` shall be added.
    *   All imports throughout the codebase must be changed from `@pixi/app`, `@pixi/sprite`, etc., to a single import from `'pixi.js'`.

*   **Requirement 4.1.2: Implement Asynchronous Initialization**
    *   The application entry point in `main.js` must be refactored into an `async` function.
    *   The `new Application()` constructor should be separated from its initialization. The `app.init()` method must be `await`-ed before any PixiJS objects are created or interacted with.
    *   Application options previously passed to the constructor must now be passed to `app.init()`.

*   **Requirement 4.1.3: Replace `app.view` with `app.canvas`**
    *   All references to `app.view` for accessing the canvas DOM element must be replaced with `app.canvas`.

#### **4.2. Graphics API Migration**

This is the most significant visual change and must be applied to `createRectangle`, `createCircle`, and any other shape-drawing functions.

*   **Requirement 4.2.1: Adopt Builder Pattern**
    *   All Graphics drawing logic must be refactored from the old `beginFill()` -> `drawShape()` -> `endFill()` pattern.
    *   The new pattern shall be `shape()` -> `fill()` and/or `stroke()`.
    *   *Example:*
        *   **Old:** `new PIXI.Graphics().beginFill(0xff0000).drawRect(0, 0, 100, 100).endFill()`
        *   **New:** `new PIXI.Graphics().rect(0, 0, 100, 100).fill(0xff0000)`

*   **Requirement 4.2.2: Update Shape Function Names**
    *   All `drawShape` methods must be renamed to their shorter v8 equivalents (e.g., `drawRect` -> `rect`, `drawCircle` -> `circle`).

*   **Requirement 4.2.3: Use Style Objects for Fills and Strokes**
    *   Calls to `fill()` and `stroke()` should use style objects for clarity where multiple properties are needed.
    *   *Example:* `stroke({ width: 2, color: 'white' })`

#### **4.3. Addressing Other Breaking Changes**

*   **Requirement 4.3.1: Replace `updateTransform`**
    *   The current implementation may use `updateTransform` for custom logic. This must be replaced with the new `onRender` function hook. A new method should be created and bound in the constructor (e.g., `this.onRender = this._onRender.bind(this)`).

*   **Requirement 4.3.2: Update `getBounds` Usage**
    *   All calls to `container.getBounds()` that expect a `PIXI.Rectangle` must be updated to `container.getBounds().rectangle`.

*   **Requirement 4.3.3: Update Ticker Callback**
    *   The callback function for `Ticker.shared.add()` no longer receives `deltaTime` directly. It now receives the `ticker` instance. The logic must be updated to use `ticker.deltaTime`.

*   **Requirement 4.3.4: Replace `utils` Imports**
    *   All calls to `PIXI.utils.*` must be refactored to use direct imports (e.g., `import { isMobile } from 'pixi.js'`).

#### **4.4. Implementing Performance Optimizations**

This section leverages both the v8 changes and the general performance tips.

*   **Requirement 4.4.1: Implement Explicit Culling**
    *   The application shall use the new `CullerPlugin` to restore automatic culling behavior.
    *   In `main.js`, add `import { extensions, CullerPlugin } from 'pixi.js';` and `extensions.add(CullerPlugin);`.
    *   Verify that `cullable = true` is set on the main viewport container in `canvas.js`.

*   **Requirement 4.4.2: Optimize Event System**
    *   A review of the scene graph must be conducted. For any `PIXI.Container` that holds non-interactive visuals (like a grid background or a group of static decorations), the `interactiveChildren` property must be set to `false` to prevent unnecessary event system traversal.

### **5. Testing and Validation Strategy**

*   **5.1. Functional Testing:**
    *   All existing user stories must be re-validated. A QA checklist must be executed, including:
        *   [ ] User can successfully log in and register.
        *   [ ] Canvas loads without visual artifacts or console errors.
        *   [ ] User can pan the canvas with the middle mouse button.
        *   [ ] User can zoom in and out with the mouse wheel.
        *   [ ] The correct tool is selected when clicking toolbar icons or using keyboard shortcuts (V, R, C).
        *   [ ] User can create rectangles and circles by clicking and dragging.
        *   [ ] User can select, move, and delete objects.
        *   [ ] Real-time cursor movements from other users are displayed smoothly.
        *   [ ] Objects created, moved, or deleted by other users are updated in real-time.

*   **5.2. Performance Benchmarking:**
    *   A baseline performance measurement must be taken on the v7 branch *before* the upgrade.
    *   The same performance test must be run on the v8 branch *after* the upgrade.
    *   **Success Criteria:**
        *   Average and minimum FPS during a stress test (e.g., 1000 static objects) must be **equal to or greater than** the v7 baseline.
        *   The application must maintain a minimum of **55 FPS** during standard pan and zoom operations on a moderately complex scene (200-300 objects).
        *   The `PerformanceMonitor` already in `canvas.js` will be the primary tool for these measurements.

### **6. Acceptance Criteria**

The upgrade will be considered complete when all of the following criteria are met:

*   [ ] All PixiJS dependencies are successfully migrated to version 8.
*   [ ] The application builds and runs without any compilation or runtime errors.
*   [ ] All functional tests listed in section 5.1 pass successfully.
*   [ ] All performance benchmarks in section 5.2 are met or exceeded.
*   [ ] There are no new warnings in the browser's developer console related to deprecated PixiJS features.
*   [ ] The codebase has been fully refactored to remove all old v7 API patterns.

### **7. Risks and Mitigation**

*   **Risk 1:** Unforeseen breaking changes disrupt core functionality in subtle ways.
    *   **Mitigation:** Allocate time for a thorough, feature-by-feature manual testing phase after the initial code refactoring is complete.
*   **Risk 2:** Initial performance is worse after the upgrade due to incorrect usage of new APIs.
    *   **Mitigation:** Perform incremental performance tests after each major change (e.g., after the Graphics migration, after implementing culling) to quickly identify the source of any degradation.
*   **Risk 3:** The migration takes significantly longer than estimated.
    *   **Mitigation:** Timebox an initial investigation phase (4-6 hours) to perform a "dry run" on a small part of the application to better estimate the full effort.
