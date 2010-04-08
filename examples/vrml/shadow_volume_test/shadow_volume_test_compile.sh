#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f shadow_volume_test.lpr ]; then
  cd ../../../
fi

# Call this from ../../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg \
  examples/vrml/shadow_volume_test/shadow_volume_test.lpr