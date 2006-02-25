#!/bin/bash
set -eu

# Call this from ../../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg units/3dmodels/examples/many2vrml.dpr