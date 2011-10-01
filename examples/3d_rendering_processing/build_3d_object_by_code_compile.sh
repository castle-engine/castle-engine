#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f build_3d_object_by_code.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/3d_rendering_processing/build_3d_object_by_code.lpr
