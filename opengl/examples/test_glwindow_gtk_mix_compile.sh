#!/bin/bash
set -eu

# Call this from ../../../ (or just use `make examples').

# To make sure that GLWindow unit is compiled with GTK2 backend
# (this is not the default on Windows).
rm -f units/opengl/glwindow.ppu units/opengl/glwindow.o

fpc -dRELEASE @kambi.cfg -dGLWINDOW_GTK_2 \
  units/opengl/examples/test_glwindow_gtk_mix.dpr