#!/bin/bash
set -eu

# Allow calling this script from it's dir.
if [ -f image_convert.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/images_videos/image_convert.lpr