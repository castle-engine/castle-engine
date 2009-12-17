#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f gl_primitive_performance.pasprogram ]; then
  cd ../../../
fi

# Call this from ../../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg vrml/opengl/examples/gl_primitive_performance.pasprogram
