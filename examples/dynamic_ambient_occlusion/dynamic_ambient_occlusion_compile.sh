#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f dynamic_ambient_occlusion.lpr ]; then
  cd ../../../
fi

# Call this from ../../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/vrml/dynamic_ambient_occlusion/dynamic_ambient_occlusion.lpr
