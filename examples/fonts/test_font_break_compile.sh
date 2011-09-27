#!/bin/bash
set -eu

# Allow calling this script from it's dir.
if [ -f test_font_break.lpr ]; then
  cd ../../
fi


# Call this from ../../ (or just use `make examples').
fpc -dRELEASE @kambi.cfg examples/fonts/test_font_break.lpr