#!/bin/bash
set -eu

# Call this from ../../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg units/opengl/examples/glWinEvents.dpr