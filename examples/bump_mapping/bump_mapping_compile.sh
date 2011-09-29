#!/bin/bash
set -eu

# Allow calling this script from it's own dir.
if [ -f bump_mapping.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').
fpc -dRELEASE @kambi.cfg examples/bump_mapping/bump_mapping.lpr
