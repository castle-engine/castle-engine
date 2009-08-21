#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f dds_remove_small_mipmaps.pasprogram ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg images/tools/dds_remove_small_mipmaps.pasprogram
