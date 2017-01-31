#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f sprite-sheet-to-x3d.lpr ]; then cd ../../; fi

fpc -dRELEASE @castle-fpc.cfg tools/sprite-sheet-to-x3d/sprite-sheet-to-x3d.lpr
