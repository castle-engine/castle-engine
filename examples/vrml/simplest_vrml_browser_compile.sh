#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f simplest_vrml_browser.lpr ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/vrml/simplest_vrml_browser.lpr
