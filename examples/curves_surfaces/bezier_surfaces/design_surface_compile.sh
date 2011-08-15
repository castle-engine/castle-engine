#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f design_surface.lpr ]; then
  cd ../../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/curves_surfaces/bezier_surfaces/design_surface.lpr
