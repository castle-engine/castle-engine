#!/bin/bash
set -eu

# Allow calling this script from it's dir.
if [ -f teapot_vrml_to_pascal.lpr ]; then
  cd ../../../
fi

# Call this from ../../ (or just use `make examples').
fpc -dRELEASE @kambi.cfg src/vrml/teapot/teapot_vrml_to_pascal.lpr
