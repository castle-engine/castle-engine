#!/bin/bash
set -euo pipefail

# Tests must pass both in -dDEBUG and -dRELEASE modes.
# This script recompiles engine (and fpcunit tests) in -dRELEASE mode.

if which castle-engine  > /dev/null; then
  castle-engine --mode=release compile
else
  cd ../
  fpc -gh -dRELEASE -dTEXT_RUNNER "$@" \
    @castle-fpc.cfg tests/test_castle_game_engine.lpr
fi
