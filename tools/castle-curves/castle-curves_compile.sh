#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f castle-curves.lpr ]; then cd ../../; fi

# Find the build tool, use it to compile
if which tools/build-tool/castle-engine > /dev/null; then
  CASTLE_ENGINE="`pwd`/tools/build-tool/castle-engine"
else
  CASTLE_ENGINE=castle-engine
fi

cd tools/castle-curves/
"${CASTLE_ENGINE}" compile ${CASTLE_BUILD_TOOL_OPTIONS:-}
