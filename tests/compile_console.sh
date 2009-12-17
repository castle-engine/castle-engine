cd ../

fpc -gh -dDEBUG -dTEXT_RUNNER \
  -Fuvrml/opengl/examples/shadow_fields/ \
  @kambi.cfg tests/test_kambi_units.lpr
