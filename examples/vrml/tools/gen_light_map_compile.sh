#!/bin/bash
set -eu

# Call this from ../../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/vrml/tools/gen_light_map.lpr