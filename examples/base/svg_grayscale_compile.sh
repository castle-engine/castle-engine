#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f svg_grayscale.pasprogram ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg base/tools/svg_grayscale.pasprogram
