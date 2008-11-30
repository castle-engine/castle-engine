cd ../

fpc -gh -dDEBUG -dTEXT_RUNNER \
  -Fu3dmodels.gl/examples/shadow_fields/ \
  @kambi.cfg tests/test_kambi_units.lpr
