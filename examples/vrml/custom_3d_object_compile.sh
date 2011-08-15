#!/bin/bash
set -eu

# Allow calling this script from it's dir.
if [ -f custom_3d_object.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').
fpc -dRELEASE @kambi.cfg examples/vrml/custom_3d_object.lpr
