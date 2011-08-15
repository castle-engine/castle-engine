#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f interpolated_curves.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/curves_surfaces/interpolated_curves.lpr
