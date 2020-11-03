#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make examples" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f precompute_radiance_transfer.lpr ]; then cd ../../../; fi

# Find the build tool, use it to compile
if which tools/build-tool/castle-engine > /dev/null; then
  CASTLE_ENGINE="`pwd`/tools/build-tool/castle-engine"
else
  CASTLE_ENGINE=castle-engine
fi

"${CASTLE_ENGINE}" simple-compile \
  --compiler-option=-Fuexamples/research_special_rendering_methods/common_code/ \
  examples/research_special_rendering_methods/radiance_transfer/precompute_radiance_transfer.lpr
