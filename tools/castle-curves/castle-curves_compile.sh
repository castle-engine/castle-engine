#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f castle-curves.lpr ]; then cd ../../; fi

fpc -dRELEASE @castle-fpc.cfg tools/castle-curves/castle-curves.lpr
