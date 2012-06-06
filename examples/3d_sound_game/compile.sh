#!/bin/sh
set -eu

# Compile all programs here. It simply calls FPC
# with proper command-line options.

# We must do cd ../castle_game_engine/ (and call FPC from that directory)
# because castle-fpc.cfg file is there and it contains paths relative
# to that directory.
cd ../castle_game_engine/

# Force rebuilding CastleWindow unit with proper backend.
make clean-window

fpc -dRELEASE ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg -dCASTLE_WINDOW_BEST_NOGUI ../lets_take_a_walk/lets_take_a_walk.lpr
fpc -dRELEASE ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg ../lets_take_a_walk/devel/vrml/process_base_b.lpr
