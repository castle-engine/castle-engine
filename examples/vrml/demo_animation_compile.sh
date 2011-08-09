#!/bin/bash
set -eu

# Allow calling this script from it's own dir.
if [ -f demo_animation.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').
fpc -dRELEASE @kambi.cfg examples/vrml/demo_animation.lpr