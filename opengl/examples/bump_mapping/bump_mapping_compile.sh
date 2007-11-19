#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f bump_mapping.pasprogram ]; then
  cd ../../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg opengl/examples/bump_mapping/bump_mapping.pasprogram
