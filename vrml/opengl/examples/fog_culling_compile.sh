#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f fog_culling.pasprogram ]; then
  cd ../../../
fi

# Call this from ../../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg 3dmodels/opengl/examples/fog_culling.pasprogram
