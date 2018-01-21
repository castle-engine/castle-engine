cd ../

# Tests must pass both in -dDEBUG and -dRELEASE modes.
# This script recompiles engine (and fpcunit tests) in -dRELEASE mode.

fpc -dRELEASE -dTEXT_RUNNER "$@" \
  @castle-fpc.cfg tests/test_castle_game_engine.lpr
