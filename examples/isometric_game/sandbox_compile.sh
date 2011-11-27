#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make examples" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f sandbox.lpr ]; then cd ../../; fi

# For Unix, consider adding here -dCASTLE_WINDOW_XLIB
# (and maybe "make clean-window" before)

fpc -dRELEASE @castle-fpc.cfg examples/isometric_game/sandbox.lpr
