#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f plane_projected_shadow_demo.pasprogram ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg 3dmodels.gl/examples/plane_projected_shadow_demo.pasprogram
