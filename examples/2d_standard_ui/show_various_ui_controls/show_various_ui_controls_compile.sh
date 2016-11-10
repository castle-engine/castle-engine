#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make examples" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f show_various_ui_controls.lpr ]; then cd ../../../; fi

fpc -dRELEASE @castle-fpc.cfg examples/2d_standard_ui/show_various_ui_controls/show_various_ui_controls.lpr
