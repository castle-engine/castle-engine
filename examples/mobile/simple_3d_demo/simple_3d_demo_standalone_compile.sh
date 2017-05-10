#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make examples" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f simple_3d_demo_standalone.lpr ]; then cd ../../../; fi

fpc -dRELEASE @castle-fpc.cfg examples/mobile/simple_3d_demo/simple_3d_demo_standalone.lpr
