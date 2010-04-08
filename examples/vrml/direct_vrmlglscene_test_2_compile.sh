#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f direct_vrmlglscene_test_2.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/vrml/direct_vrmlglscene_test_2.lpr