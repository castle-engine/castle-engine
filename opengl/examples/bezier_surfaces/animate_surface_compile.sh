#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f animate_surface.pasprogram ]; then
  cd ../../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg opengl/examples/bezier_surfaces/animate_surface.pasprogram
