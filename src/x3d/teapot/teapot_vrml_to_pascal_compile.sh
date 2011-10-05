#!/bin/bash
set -eu

# Allow calling this script from it's dir.
if [ -f teapot_vrml_to_pascal.lpr ]; then
  cd ../../../
fi

# Call this from ../../ (or just use `make examples').
fpc -dRELEASE @castle-fpc.cfg src/x3d/teapot/teapot_vrml_to_pascal.lpr
