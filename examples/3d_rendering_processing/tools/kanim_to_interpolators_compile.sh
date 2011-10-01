#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f kanim_to_interpolators.lpr ]; then
  cd ../../../
fi

# Call this from ../../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/3d_rendering_processing/tools/kanim_to_interpolators.lpr
