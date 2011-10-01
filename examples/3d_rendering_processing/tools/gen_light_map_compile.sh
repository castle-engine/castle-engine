#!/bin/bash
set -eu

# Call this from ../../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/3d_rendering_processing/tools/gen_light_map.lpr