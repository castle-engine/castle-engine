cd ../

fpc -gh -dDEBUG -dTEXT_RUNNER "$@" \
  -Fuexamples/research_special_rendering_methods/shadow_fields/ \
  -Fuexamples/curves_surfaces/ \
  @castle-fpc.cfg tests/test_castle_game_engine.lpr
