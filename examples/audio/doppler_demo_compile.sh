#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f doppler_demo.pasprogram ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg audio/examples/doppler_demo.pasprogram
