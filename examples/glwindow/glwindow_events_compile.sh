#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f glwindow_events.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/glwindow/glwindow_events.lpr
