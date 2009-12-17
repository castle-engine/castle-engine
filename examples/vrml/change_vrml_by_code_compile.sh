#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f change_vrml_by_code.pasprogram ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/vrml/change_vrml_by_code.pasprogram
