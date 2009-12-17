#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f gl_win_events.pasprogram ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg glwindow/examples/gl_win_events.pasprogram
