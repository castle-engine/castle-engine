#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make examples" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f x3d-nodes-to-pascal.lpr ]; then cd ../../../../; fi

# Find the build tool, use it to compile
if which tools/build-tool/castle-engine > /dev/null; then
  CASTLE_ENGINE="`which tools/build-tool/castle-engine`"
else
  CASTLE_ENGINE=castle-engine
fi

"${CASTLE_ENGINE}" simple-compile src/x3d/nodes_specification/x3d-nodes-to-pascal/x3d-nodes-to-pascal.lpr
