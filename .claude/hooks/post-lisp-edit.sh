#!/bin/bash
# Claude Code hook: Format and lint Lisp files after Write/Edit

set -e

# Read hook input from stdin
INPUT=$(cat)

# Extract file path from the JSON input
FILE_PATH=$(echo "$INPUT" | jq -r '.toolResult.file_path // .params.file_path // empty')

# Exit silently if no file path or not a Lisp file
if [ -z "$FILE_PATH" ] || [[ ! "$FILE_PATH" =~ \.lisp$ ]]; then
  exit 0
fi

# Exit if file doesn't exist
if [ ! -f "$FILE_PATH" ]; then
  exit 0
fi

echo "🔧 Linting: $FILE_PATH"

# Lint the file
if command -v sblint &> /dev/null; then
  echo ""
  echo "Running sblint..."
  if sblint "$FILE_PATH" 2>&1; then
    echo "✅ No linting issues found"
  else
    # Return exit code 2 to show Claude the linting errors
    echo "⚠️  Linting issues detected above" >&2
    exit 2
  fi
fi

exit 0
