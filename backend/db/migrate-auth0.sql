-- Migration: Remove password_hash column and make Auth0-only
-- Run this after schema.sql on existing databases

-- Remove password_hash column if it exists
-- SQLite doesn't support DROP COLUMN directly, so we need to recreate the table

-- Step 1: Create new users table without password_hash
CREATE TABLE IF NOT EXISTS users_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT UNIQUE NOT NULL,
    username TEXT UNIQUE NOT NULL,
    auth0_sub TEXT UNIQUE,
    last_login_at TEXT,
    created_at TEXT DEFAULT (datetime('now')),
    updated_at TEXT DEFAULT (datetime('now'))
);

-- Step 2: Copy data from old table (excluding password_hash)
INSERT INTO users_new (id, email, username, auth0_sub, last_login_at, created_at, updated_at)
SELECT id, email, username, auth0_sub, last_login_at, created_at, updated_at
FROM users;

-- Step 3: Drop old table
DROP TABLE users;

-- Step 4: Rename new table
ALTER TABLE users_new RENAME TO users;

-- Step 5: Recreate indexes
CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);
CREATE INDEX IF NOT EXISTS idx_users_username ON users(username);
CREATE INDEX IF NOT EXISTS idx_users_auth0_sub ON users(auth0_sub);

-- Clean up: Delete users without Auth0 accounts (legacy users)
-- Uncomment the line below if you want to remove legacy users:
-- DELETE FROM users WHERE auth0_sub IS NULL;
