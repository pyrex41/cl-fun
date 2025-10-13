-- CollabCanvas Database Schema

-- Enable foreign key constraints
PRAGMA foreign_keys = ON;

-- Users table
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT UNIQUE NOT NULL,
    username TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    created_at TEXT DEFAULT (datetime('now')),
    updated_at TEXT DEFAULT (datetime('now'))
);

-- Create indexes for users
CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);
CREATE INDEX IF NOT EXISTS idx_users_username ON users(username);

-- Sessions table
CREATE TABLE IF NOT EXISTS sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    session_id TEXT UNIQUE NOT NULL,
    created_at TEXT DEFAULT (datetime('now')),
    expires_at TEXT NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- Create indexes for sessions
CREATE INDEX IF NOT EXISTS idx_sessions_session_id ON sessions(session_id);
CREATE INDEX IF NOT EXISTS idx_sessions_user_id ON sessions(user_id);
CREATE INDEX IF NOT EXISTS idx_sessions_expires_at ON sessions(expires_at);

-- Canvas states table
CREATE TABLE IF NOT EXISTS canvas_states (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    canvas_id TEXT NOT NULL,
    state_json TEXT NOT NULL,
    version INTEGER DEFAULT 1,
    created_at TEXT DEFAULT (datetime('now')),
    updated_at TEXT DEFAULT (datetime('now'))
);

-- Create indexes for canvas_states
CREATE INDEX IF NOT EXISTS idx_canvas_states_canvas_id ON canvas_states(canvas_id);
CREATE INDEX IF NOT EXISTS idx_canvas_states_updated_at ON canvas_states(updated_at);

-- Canvas history table (for undo/redo and version control)
CREATE TABLE IF NOT EXISTS canvas_history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    canvas_id TEXT NOT NULL,
    user_id INTEGER,
    action_type TEXT NOT NULL, -- 'create', 'update', 'delete'
    object_data TEXT NOT NULL, -- JSON data of the change
    timestamp TEXT DEFAULT (datetime('now')),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE SET NULL
);

-- Create indexes for canvas_history
CREATE INDEX IF NOT EXISTS idx_canvas_history_canvas_id ON canvas_history(canvas_id);
CREATE INDEX IF NOT EXISTS idx_canvas_history_timestamp ON canvas_history(timestamp);

-- Collaborators table (track who has access to which canvas)
CREATE TABLE IF NOT EXISTS collaborators (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    canvas_id TEXT NOT NULL,
    user_id INTEGER NOT NULL,
    role TEXT DEFAULT 'editor', -- 'owner', 'editor', 'viewer'
    joined_at TEXT DEFAULT (datetime('now')),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    UNIQUE(canvas_id, user_id)
);

-- Create indexes for collaborators
CREATE INDEX IF NOT EXISTS idx_collaborators_canvas_id ON collaborators(canvas_id);
CREATE INDEX IF NOT EXISTS idx_collaborators_user_id ON collaborators(user_id);