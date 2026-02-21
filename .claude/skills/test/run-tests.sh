#!/usr/bin/env bash
# Run eldev etest and output only a clean summary.
# Usage:
#   run-tests.sh                    # all tests
#   run-tests.sh test/unit/foo.el   # single file
#   run-tests.sh unit               # category (unit/integration/acceptance)
set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "$0")/../../.." && pwd)"
cd "$PROJECT_DIR"

# Build the eldev command
CMD=(~/bin/eldev etest -r dot)

case "${1:-all}" in
  all)
    ;; # no extra args
  unit|integration|acceptance)
    CMD+=("test/$1/")
    ;;
  *)
    # Treat as file path
    CMD+=("$1")
    ;;
esac

# Run tests, capture output and exit code
OUTPUT=$("${CMD[@]}" 2>&1) || true

# Extract the summary line, duration, and seed
SUMMARY=$(echo "$OUTPUT" | grep -E '^[0-9]+ tests:' || echo "")
DURATION=$(echo "$OUTPUT" | grep -E '^Finished in' || echo "")
SEED=$(echo "$OUTPUT" | grep -oP 'Using seed: \K\d+' || echo "")

if [ -z "$SUMMARY" ]; then
  # No summary found — something unexpected happened
  echo "UNEXPECTED OUTPUT (last 30 lines):"
  echo "$OUTPUT" | tail -30
  exit 1
fi

# Parse counts from summary
FAILED=$(echo "$SUMMARY" | grep -oP '\d+ failed' | grep -oP '\d+')
ERRORS=$(echo "$SUMMARY" | grep -oP '\d+ errors' | grep -oP '\d+')
TOTAL=$(echo "$SUMMARY" | grep -oP '^\d+')
TIME=$(echo "$DURATION" | grep -oP '[\d.]+s' || echo "?s")

if [ "$FAILED" = "0" ] && [ "$ERRORS" = "0" ]; then
  echo "PASS: $TOTAL tests in $TIME"
else
  echo "FAIL: $SUMMARY ($TIME)"
  [ -n "$SEED" ] && echo "Seed: $SEED"
  echo ""
  # Extract failure and error blocks — everything from "Failures:" or "Errors:" to "Finished in"
  # Strip file-save noise from the detail blocks
  echo "$OUTPUT" \
    | sed -n '/^\(Failures\|Errors\):/,/^Finished in/p' \
    | sed '/^Finished in/d' \
    | grep -v 'Save file /mock:' \
    | grep -v '(y, n, !, \., q,' \
    | sed '/^$/N;/^\n$/d'
  exit 1
fi
