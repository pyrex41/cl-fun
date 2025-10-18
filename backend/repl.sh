#!/bin/bash
# REPL for live code modification while server is running
# Usage: ./repl.sh

# Load environment variables (for REPL development)
if [ -f .env ]; then
  set -a
  source .env
  set +a
fi

echo "Starting interactive REPL..."
echo "Server is running on port 8080"
echo ""
echo "REPL Commands:"
echo "  (load \"src/physics-systems.lisp\")  ; Reload physics systems"
echo "  (load \"src/websocket.lisp\")        ; Reload websocket handlers"
echo "  (in-package :collabcanvas)           ; Switch to collabcanvas package"
echo "  (list-active-physics-canvases)       ; See active physics canvases"
echo ""

ros -e '(ql:quickload :collabcanvas :silent t)' \
    -e '(in-package :collabcanvas)' \
    -e '(format t "~%=== REPL Connected to CollabCanvas ===~%~%")' \
    run
