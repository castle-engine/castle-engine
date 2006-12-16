#!/bin/bash
set -eu

# Call this from ../../ (or just use `make examples').

# To make sure that GLWindow unit is compiled with GTK2 backend
# (this is not the default on Windows).
rm -f opengl/glwindow.ppu opengl/glwindow.o

fpc -dRELEASE @kambi.cfg -dGLWINDOW_GTK_2 \
  opengl/examples/test_glwindow_gtk_mix.dpr
  
# Allow other examples to be compiled by default with WinAPI backend
# (instead of GTK2)
rm -f opengl/glwindow.ppu opengl/glwindow.o
