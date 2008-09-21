#!/bin/bash
set -eu

# Hack to allow calling this script from it's dir.
if [ -f view_model_with_events.pasprogram ]; then
  cd ../../
fi

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg 3dmodels.gl/examples/view_model_with_events.pasprogram
