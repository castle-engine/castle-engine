#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make examples" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f window_gtk_mix.lpr ]; then cd ../../; fi

# To make sure that CastleWindow unit is compiled with GTK2 backend
# (this is not the default on Windows).
make --quiet clean-window

fpc -dRELEASE @castle-fpc.cfg -dCASTLE_WINDOW_GTK_2 \
  examples/window/window_gtk_mix.lpr

# Allow other examples to be compiled by default with WinAPI backend
# (instead of GTK2).
make --quiet clean-window
