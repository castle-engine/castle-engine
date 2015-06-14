#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make examples" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f generate_x3d_nodes_to_pascal.lpr ]; then cd ../../../../; fi

fpc -dRELEASE @castle-fpc.cfg src/x3d/nodes_specification/generate_x3d_nodes_helpers/generate_x3d_nodes_to_pascal.lpr
