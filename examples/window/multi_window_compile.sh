#!/bin/bash
set -eu

# Allow calling this script from it's own dir.
if [ -f multi_window.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').
fpc -dRELEASE @kambi.cfg examples/window/multi_window.lpr