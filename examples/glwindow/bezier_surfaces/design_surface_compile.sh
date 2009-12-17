#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f design_surface.pasprogram ]; then
  cd ../../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/glwindow/bezier_surfaces/design_surface.pasprogram
