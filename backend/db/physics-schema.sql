-- Physics Engine Database Schema
-- Extends CollabCanvas with physics simulation capabilities

-- Enable foreign key constraints
PRAGMA foreign_keys = ON;

-- Physics canvas settings table
-- Stores physics configuration per canvas
CREATE TABLE IF NOT EXISTS physics_canvas_settings (
    canvas_id TEXT PRIMARY KEY,
    gravity_x REAL DEFAULT 0.0,
    gravity_y REAL DEFAULT 9.8,
    simulation_rate INTEGER DEFAULT 60,  -- Hz (updates per second)
    max_objects INTEGER DEFAULT 2000,    -- Maximum number of physics objects
    created_at TEXT DEFAULT (datetime('now')),
    updated_at TEXT DEFAULT (datetime('now'))
);

-- Create index for physics_canvas_settings
CREATE INDEX IF NOT EXISTS idx_physics_canvas_settings_updated_at
    ON physics_canvas_settings(updated_at);

-- Physics components table
-- Stores physics-enabled components (balls, fans, blocks, emitters, magnets)
CREATE TABLE IF NOT EXISTS physics_components (
    id TEXT PRIMARY KEY,
    canvas_id TEXT NOT NULL,
    component_type TEXT NOT NULL,  -- 'ball', 'fan', 'block', 'emitter', 'magnet'
    properties_json TEXT NOT NULL, -- JSON blob with component-specific properties
    created_by INTEGER NOT NULL,
    created_at TEXT DEFAULT (datetime('now')),
    FOREIGN KEY (canvas_id) REFERENCES canvas_states(canvas_id) ON DELETE CASCADE,
    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE CASCADE
);

-- Create indexes for physics_components
CREATE INDEX IF NOT EXISTS idx_physics_components_canvas_id
    ON physics_components(canvas_id);
CREATE INDEX IF NOT EXISTS idx_physics_components_type
    ON physics_components(component_type);
CREATE INDEX IF NOT EXISTS idx_physics_components_created_by
    ON physics_components(created_by);
CREATE INDEX IF NOT EXISTS idx_physics_components_created_at
    ON physics_components(created_at);

-- Physics bodies table (optional persistence layer)
-- Stores runtime physics state for persistence/recovery
CREATE TABLE IF NOT EXISTS physics_bodies (
    id TEXT PRIMARY KEY,
    component_id TEXT NOT NULL,
    position_x REAL NOT NULL,
    position_y REAL NOT NULL,
    velocity_x REAL DEFAULT 0.0,
    velocity_y REAL DEFAULT 0.0,
    rotation REAL DEFAULT 0.0,         -- Radians
    is_sleeping INTEGER DEFAULT 0,     -- Boolean: 0=active, 1=sleeping
    updated_at TEXT DEFAULT (datetime('now')),
    FOREIGN KEY (component_id) REFERENCES physics_components(id) ON DELETE CASCADE
);

-- Create indexes for physics_bodies
CREATE INDEX IF NOT EXISTS idx_physics_bodies_component_id
    ON physics_bodies(component_id);
CREATE INDEX IF NOT EXISTS idx_physics_bodies_updated_at
    ON physics_bodies(updated_at);
CREATE INDEX IF NOT EXISTS idx_physics_bodies_is_sleeping
    ON physics_bodies(is_sleeping);

-- Component types constraint (optional check for validation)
-- Note: SQLite doesn't enforce CHECK constraints by default in older versions,
-- but they're documented for clarity and newer SQLite versions support them
CREATE TABLE IF NOT EXISTS physics_component_types (
    type_name TEXT PRIMARY KEY,
    description TEXT
);

INSERT OR IGNORE INTO physics_component_types (type_name, description) VALUES
    ('ball', 'Dynamic spherical physics object'),
    ('fan', 'Force field generator that applies directional forces'),
    ('block', 'Static or kinematic rectangular obstacle'),
    ('emitter', 'Spawns physics objects at intervals'),
    ('magnet', 'Applies attractive/repulsive forces to nearby objects');
