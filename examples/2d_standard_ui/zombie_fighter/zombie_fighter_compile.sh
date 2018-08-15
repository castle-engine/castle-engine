#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make examples" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f zombie_fighter.lpr ]; then cd ../../../; fi

# Find the build tool, use it to compile
if which tools/build-tool/castle-engine > /dev/null; then
  CASTLE_ENGINE="`which tools/build-tool/castle-engine`"
else
  CASTLE_ENGINE=castle-engine
fi

cd examples/2d_standard_ui/zombie_fighter/
"${CASTLE_ENGINE}" compile
