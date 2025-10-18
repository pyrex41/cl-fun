#!/bin/bash
#
# run-profiling.sh - Execute Physics Loop Profiling
#
# Usage: ./backend/run-profiling.sh
#
# This script:
#   1. Starts SBCL with the collabcanvas system
#   2. Loads the profiling script
#   3. Runs profiling scenarios
#   4. Captures output to profiling-results.txt
#
# Task 5.4 - Performance Profiling

set -e

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║       Physics Loop Performance Profiling                    ║${NC}"
echo -e "${GREEN}║       Task 5.4                                               ║${NC}"
echo -e "${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check for SBCL
if ! command -v sbcl &> /dev/null; then
    echo -e "${YELLOW}ERROR: SBCL not found. Please install SBCL first.${NC}"
    exit 1
fi

# Create output directory
mkdir -p .taskmaster/docs

# Output file
OUTPUT_FILE=".taskmaster/docs/profiling-raw-output.txt"

echo -e "${YELLOW}Starting profiling...${NC}"
echo "Output will be saved to: ${OUTPUT_FILE}"
echo ""
echo "This will take approximately 3-4 minutes (60 seconds per scenario x 3 scenarios)"
echo ""

# Run profiling
sbcl --noinform \
     --eval "(ql:quickload :collabcanvas :silent t)" \
     --eval "(setf *print-pretty* t)" \
     --load "backend/tests/profile-physics-loop.lisp" \
     --eval "(collabcanvas::profile-physics-scenarios)" \
     --eval "(quit)" \
     2>&1 | tee "${OUTPUT_FILE}"

echo ""
echo -e "${GREEN}Profiling complete!${NC}"
echo "Raw results saved to: ${OUTPUT_FILE}"
echo ""
echo "Next steps:"
echo "  1. Review the profiling output above"
echo "  2. Analyze the profiling results"
echo "  3. Create PROFILING_RESULTS.md with findings"
echo ""
