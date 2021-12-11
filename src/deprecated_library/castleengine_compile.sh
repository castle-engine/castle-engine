#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.

# Change to the directory where castle-fpc.cfg is.
if [ -f castleengine.lpr ]; then cd ../../; fi

# We clean first, to
# 1. make sure that CastleWindow unit is compiled with LIBRARY backend.
# 2. make sure that every unit is recompiled with -fPIC (necessary for .so on x86_64)
make --quiet clean

fpc -fPIC -dRELEASE @castle-fpc.cfg -dCASTLE_WINDOW_LIBRARY src/deprecated_library/castleengine.lpr "$@"

# Clean units to avoid other programs to be accidentally compiled
# with CastleWindow with LIBRARY backend, or -fPIC.
# Note: do not just run "make clean", it would remove also
# the library we just compiled.
find . -type f '(' -iname '*.o' -or -iname '*.ppu' ')' -exec rm -f '{}' ';'
