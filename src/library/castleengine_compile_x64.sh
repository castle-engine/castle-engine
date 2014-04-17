#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f castleengine.lpr ]; then cd ../../; fi

# To make sure that CastleWindow unit is compiled with LIBRARY backend.
make clean-window

fpc -Px86_64 -dRELEASE @castle-fpc.cfg -dCASTLE_WINDOW_LIBRARY src/library/castleengine.lpr

# Avoid other programs to be accidentally compiled
# with CastleWindow with LIBRARY backend.
make clean-window
