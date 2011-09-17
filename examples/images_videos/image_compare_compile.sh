#!/bin/bash
set -eu

# Call this script from it's own directory,
# or from base castle_game_engine directory.
# Or just do "make examples" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f image_compare.lpr ]; then cd ../../; fi

fpc -dRELEASE @kambi.cfg examples/images_videos/image_compare.lpr
