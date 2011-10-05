cd ../

fpc -gh -dDEBUG -dTEXT_RUNNER "$@" \
  -Fuexamples/shadow_fields/ \
  @castle-fpc.cfg tests/test_castle_game_engine.lpr
