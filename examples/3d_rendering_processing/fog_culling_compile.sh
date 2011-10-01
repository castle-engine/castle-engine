#!/bin/bash
set -eu

# Allow calling this script from it's own dir.
if [ -f fog_culling.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').
fpc -dRELEASE @kambi.cfg examples/3d_rendering_processing/fog_culling.lpr
