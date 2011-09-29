#!/bin/bash
set -eu

# Call this from ../../ (or just use `make examples').

# To make sure that CastleWindow unit is compiled with GTK2 backend
# (this is not the default on Windows).
rm -f src/glwindow/castlewindow.ppu src/glwindow/castlewindow.o

fpc -dRELEASE @kambi.cfg -dCASTLE_WINDOW_GTK_2 \
  examples/glwindow/window_gtk_mix.lpr

# Allow other examples to be compiled by default with WinAPI backend
# (instead of GTK2)
rm -f src/glwindow/castlewindow.ppu src/glwindow/castlewindow.o
