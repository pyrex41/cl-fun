-- Migration: Add preferred_color to users table
-- Run this migration once to update existing databases

ALTER TABLE users ADD COLUMN preferred_color TEXT DEFAULT '#3498db';
