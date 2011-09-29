#!/bin/bash
set -eu

# Allow calling this script from it's own dir.
if [ -f window_gtk_mix.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

# To make sure that CastleWindow unit is compiled with GTK2 backend
# (this is not the default on Windows).
make clean-window

fpc -dRELEASE @kambi.cfg -dCASTLE_WINDOW_GTK_2 \
  examples/window/window_gtk_mix.lpr

# Allow other examples to be compiled by default with WinAPI backend
# (instead of GTK2).
make clean-window
