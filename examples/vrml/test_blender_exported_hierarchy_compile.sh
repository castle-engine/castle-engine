#!/bin/bash
set -eu

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg examples/vrml/test_blender_exported_hierarchy.lpr