#!/bin/bash
set -e
# Include current directory in LD_LIBRARY_PATH, to find Steam dynamic library
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:."
./steam_test "$@"
