#!/bin/bash

# Start script for CollabCanvas backend

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== CollabCanvas Backend Startup ===${NC}"

# Load environment variables from .env if it exists
if [ -f .env ]; then
    echo -e "${YELLOW}Loading environment variables from .env${NC}"
    export $(grep -v '^#' .env | xargs)
    echo -e "${GREEN}Environment variables loaded${NC}"
fi

# Check if Roswell is installed
if ! command -v ros &> /dev/null; then
    echo -e "${RED}Error: Roswell is not installed${NC}"
    echo "Please install Roswell first: https://github.com/roswell/roswell"
    exit 1
fi

# Create data directory if it doesn't exist
mkdir -p data

# Initialize database if it doesn't exist
if [ ! -f data/canvas.db ]; then
    echo -e "${YELLOW}Initializing database...${NC}"
    sqlite3 data/canvas.db < db/schema.sql
    echo -e "${GREEN}Database initialized${NC}"
fi

# Link project to Roswell if not already linked
if [ ! -L ~/.roswell/local-projects/collabcanvas ]; then
    echo -e "${YELLOW}Linking project to Roswell...${NC}"
    ln -s "$(pwd)" ~/.roswell/local-projects/collabcanvas
    ros -e '(ql:register-local-projects)' -q
fi

# Start the server
echo -e "${GREEN}Starting CollabCanvas server...${NC}"
ros run -e '(ql:quickload :collabcanvas :silent t)' \
        -e '(collabcanvas:start-server)' \
        -e '(loop (sleep 1))' \
        -q