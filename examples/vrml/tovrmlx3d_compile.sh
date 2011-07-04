#!/bin/bash
set -eu

# Allow calling this script from it's dir.
if [ -f tovrmlx3d.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/vrml/tovrmlx3d.lpr