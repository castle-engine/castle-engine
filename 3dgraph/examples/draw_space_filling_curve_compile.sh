#!/bin/bash
set -eu

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg 3dgraph/examples/draw_space_filling_curve.dpr