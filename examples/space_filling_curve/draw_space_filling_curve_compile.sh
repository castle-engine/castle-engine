#!/bin/bash
set -eu

# Allow calling this script from it's own dir.
if [ -f draw_space_filling_curve.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').
fpc -dRELEASE @kambi.cfg examples/space_filling_curve/draw_space_filling_curve.lpr