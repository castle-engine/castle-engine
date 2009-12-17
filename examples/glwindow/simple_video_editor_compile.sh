#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f simple_video_editor.pasprogram ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg glwindow/examples/simple_video_editor.pasprogram
