#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make examples" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f lets_take_a_walk.lpr ]; then cd ../../; fi

# For Unix, consider adding here -dCASTLE_WINDOW_XLIB
# (and maybe "make clean-window" before)

# Find the build tool, use it to compile
if which tools/build-tool/castle-engine > /dev/null; then
  CASTLE_ENGINE="`pwd`/tools/build-tool/castle-engine"
else
  CASTLE_ENGINE=castle-engine
fi

"${CASTLE_ENGINE}" simple-compile examples/3d_sound_game/lets_take_a_walk.lpr
