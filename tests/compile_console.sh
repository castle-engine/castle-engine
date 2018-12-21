#!/bin/bash
set -euo pipefail

# Compile the auto-tests.
# In debug mode, with text runner.
# One optional additional parameter possible, like -dXXX.

if which castle-engine  > /dev/null; then
  castle-engine --mode=debug --compiler-option="$@" compile
else
  cd ../
  fpc -gh -dDEBUG -dTEXT_RUNNER "$@" \
    @castle-fpc.cfg tests/test_castle_game_engine.lpr
fi
