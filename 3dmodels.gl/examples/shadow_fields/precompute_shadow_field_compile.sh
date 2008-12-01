#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f precompute_shadow_field.pasprogram ]; then
  cd ../../../
fi

# Call this from ../../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg 3dmodels.gl/examples/shadow_fields/precompute_shadow_field.pasprogram
