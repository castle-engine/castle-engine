#!/bin/bash
set -eu

# Compile the Castle Game Engine build tool
# ("castle-engine" binary, "castle-engine.exe" on Windows).
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .
#
# Call this script from its directory, like this:
#
#   cd castle_game_engine/tools/build-tool/
#   ./castle-engine_compile.sh
#
# Or call it from the main castle_game_engine/ directory like this:
#
#   ./tools/build-tool/castle-engine_compile.sh
#
# You can also do "make tools" or "make examples" in the main
# castle_game_engine/ directory. They compile build tool along the way.
#
# Note: Once you have compiled the build tool and placed it on $PATH,
# you can later compile build tool by itself calling in this directory:
#
#   castle-engine compile
#
# This way build tool will compile itself (bootstrap).

# Allow calling this script from it's dir.
if [ -f castle-engine.lpr ]; then cd ../../; fi

mkdir -p tools/build-tool/castle-engine-output/build-tool-compilation

fpc -dRELEASE @castle-fpc.cfg \
  -FEtools/build-tool/ \
  -FUtools/build-tool/castle-engine-output/build-tool-compilation \
  -Futools/common-code/ \
  -Futools/build-tool/code/ \
  -Futools/build-tool/embedded_images/ \
  ${CASTLE_FPC_OPTIONS:-} \
  tools/build-tool/castle-engine.lpr
