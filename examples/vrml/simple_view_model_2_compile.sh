#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f simple_view_model_2.pasprogram ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/vrml/simple_view_model_2.pasprogram