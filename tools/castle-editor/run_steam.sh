#!/bin/bash
set -e

# Run on Linux with Steam integration enabled:
# - passes --steam
# - modifies LD_LIBRARY_PATH to find libsteam_api.so in the current dir.
#
# If you set up everything OK (see https://castle-engine.io/steam),
# you should see "Steam integration enabled OK." in the log.
#
# Note that for testing, it's also useful to have steam_appid.txt:
# - It's already inside our tools/castle-editor/ (for testing purposes).
# - It's not in packaged bin/ (because should not be used in production).

# Include current directory in LD_LIBRARY_PATH, to find Steam dynamic library
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:."
./castle-editor --steam "$@"
