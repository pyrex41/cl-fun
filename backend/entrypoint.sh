#!/bin/bash
set -e

# entrypoint.sh - Docker entrypoint for CollabCanvas with Woo
# Handles database initialization, server startup, and graceful shutdown

# Get port from environment or use default
PORT="${PORT:-5000}"

# Database initialization
DB_PATH="/app/data/canvas.db"
SCHEMA_PATH="/app/db/schema.sql"

echo "=== CollabCanvas Docker Entrypoint ==="
echo "Port: $PORT"
echo "Database: $DB_PATH"

# Initialize database if it doesn't exist
if [ ! -f "$DB_PATH" ]; then
    echo "Database not found. Initializing..."
    if [ -f "$SCHEMA_PATH" ]; then
        sqlite3 "$DB_PATH" < "$SCHEMA_PATH"
        echo "✓ Database initialized from schema"
    else
        echo "Warning: Schema file not found at $SCHEMA_PATH"
        echo "Database will be initialized by application on first run"
    fi
else
    echo "✓ Database found"
fi

# Graceful shutdown handler
cleanup() {
    echo ""
    echo "Received shutdown signal, stopping server gracefully..."
    # The Lisp process will handle its own cleanup via signal handlers
    exit 0
}

# Trap SIGTERM and SIGINT for graceful shutdown
trap cleanup SIGTERM SIGINT

# Start server based on command
case "${1:-start}" in
    start)
        echo ""
        echo "Starting CollabCanvas with Woo..."
        echo ""

        # Start server using Roswell with proper signal handling
        exec ros -e "(ql:quickload :collabcanvas :silent t)" \
                 -e "(in-package :collabcanvas)" \
                 -e "(setf *port* ${PORT})" \
                 -e "(collabcanvas:main)"
        ;;

    repl)
        echo ""
        echo "Starting REPL..."
        exec ros run
        ;;

    test)
        echo ""
        echo "Running tests..."
        exec ros -e "(ql:quickload :collabcanvas/tests)" \
                 -e "(asdf:test-system :collabcanvas)"
        ;;

    shell)
        echo ""
        exec /bin/bash
        ;;

    *)
        echo "Unknown command: $1"
        echo "Available commands: start (default), repl, test, shell"
        exit 1
        ;;
esac
