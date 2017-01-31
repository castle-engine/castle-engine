cd ../

# Tests must pass both in -dDEBUG and -dRELEASE modes.
# This script recompiles engine (and fpcunit tests) in -dRELEASE mode.

fpc -dRELEASE -dTEXT_RUNNER "$@" \
  -Fuexamples/research_special_rendering_methods/shadow_fields/ \
  -Fuexamples/curves_surfaces/ \
  @castle-fpc.cfg tests/test_castle_game_engine.lpr
