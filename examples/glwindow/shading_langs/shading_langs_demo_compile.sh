#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f shading_langs_demo.pasprogram ]; then
  cd ../../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE -dUSE_GL_GLU_UNITS @kambi.cfg examples/glwindow/shading_langs/shading_langs_demo.pasprogram
