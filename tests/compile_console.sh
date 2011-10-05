cd ../

fpc -gh -dDEBUG -dTEXT_RUNNER "$@" \
  -Fuexamples/shadow_fields/ \
  @kambi.cfg tests/test_castle_game_engine.lpr
