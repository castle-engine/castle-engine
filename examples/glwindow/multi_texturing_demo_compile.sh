#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f multi_texturing_demo.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/glwindow/multi_texturing_demo.lpr
