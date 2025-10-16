#!/bin/bash
# Setup git hooks for Lisp linting
# Run this script in each new worktree to enable pre-commit linting

set -e

echo "Setting up git hooks for Lisp linting..."

# Check if we're in a git repository
if [ ! -d ".git" ]; then
  echo "❌ Error: Not in a git repository root"
  echo "   Make sure you're in the worktree root directory"
  exit 1
fi

# Create hooks directory if it doesn't exist
mkdir -p .git/hooks

# Create pre-commit hook
cat > .git/hooks/pre-commit << 'HOOK_EOF'
#!/bin/bash
# Pre-commit hook for Lisp linting and formatting

set -e

echo "Running Lisp code quality checks..."

# Get list of staged .lisp files
STAGED_LISP_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.lisp$' || true)

if [ -z "$STAGED_LISP_FILES" ]; then
  echo "No Lisp files to check."
  exit 0
fi

echo "Checking files:"
echo "$STAGED_LISP_FILES"
echo ""

# Note: Auto-formatting is disabled - use your editor's indentation
# or manually format before committing

echo "Running linter (sblint)..."

# Create a temporary file list for sblint
TEMP_FILE_LIST=$(mktemp)
echo "$STAGED_LISP_FILES" > "$TEMP_FILE_LIST"

# Run sblint on staged files
LINT_FAILED=0
while IFS= read -r file; do
  if [ -f "$file" ]; then
    echo "  Linting: $file"
    if ! sblint "$file"; then
      LINT_FAILED=1
    fi
  fi
done < "$TEMP_FILE_LIST"

# Clean up
rm "$TEMP_FILE_LIST"

if [ $LINT_FAILED -eq 1 ]; then
  echo ""
  echo "❌ Linting failed! Please fix the errors before committing."
  exit 1
fi

echo ""
echo "✅ All checks passed!"
exit 0
HOOK_EOF

# Make hook executable
chmod +x .git/hooks/pre-commit

echo "✅ Pre-commit hook installed successfully!"
echo ""
echo "The hook will now:"
echo "  - Automatically lint staged .lisp files before commits"
echo "  - Block commits if linting errors are found"
echo "  - Show clear error messages for any issues"
echo ""
echo "To bypass the hook (not recommended): git commit --no-verify"
