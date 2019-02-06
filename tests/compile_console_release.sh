#!/bin/bash
set -euo pipefail

# Compile the auto-tests.
# In release mode (tests must pass in both debug and release), with text runner.
# One optional additional parameter possible, like -dXXX.

if which castle-engine  > /dev/null; then
  castle-engine --mode=release --compiler-option="$@" compile
else
  cd ../
  fpc -gh -dRELEASE -dTEXT_RUNNER "$@" \
    @castle-fpc.cfg tests/test_castle_game_engine.lpr
fi
