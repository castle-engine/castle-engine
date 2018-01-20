cd ../

fpc -gh -dDEBUG -dTEXT_RUNNER "$@" \
  @castle-fpc.cfg tests/test_castle_game_engine.lpr
