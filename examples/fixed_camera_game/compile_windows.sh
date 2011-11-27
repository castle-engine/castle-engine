#! /bin/sh
set -eu

# This is automatically generated script that should compile
# all programs in this archive. It simply calls FPC
# with proper command-line options.
#
# We must do cd ../castle_game_engine/ (and call FPC from that directory)
# because castle-fpc.cfg file is there and it contains paths relative
# to that directory.

cd ../castle_game_engine/

# This program uses CastleWindow unit. CastleWindow unit may be compiled
# with various back-ends (e.g. under Unices two most useful back-ends
# are XLIB and GTK). To make sure that compilation of this program
# will produce exactly what you need, below we make sure that
# unit CastleWindow will be *always* *rebuild*.
#
# Of course this means that compilation time will suffer a little,
# since CastleWindow unit will be possibly rebuild without any real need.
# Comment out line below if you want.
make clean-window

fpc -dRELEASE "${CASTLE_FPC_OPTIONS:-}" @castle-fpc.cfg ../rift/rift.lpr
