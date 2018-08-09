#!/bin/bash
set -euo pipefail

if which castle-engine  > /dev/null; then
  castle-engine --mode=debug compile
else
  cd ../
  fpc -gh -dDEBUG -dTEXT_RUNNER "$@" \
    @castle-fpc.cfg tests/test_castle_game_engine.lpr
fi
