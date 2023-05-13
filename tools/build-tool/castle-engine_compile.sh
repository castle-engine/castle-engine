#!/usr/bin/env bash
set -euo pipefail
# See http://redsymbol.net/articles/unofficial-bash-strict-mode/

# ----------------------------------------------------------------------------
# Compile the Castle Game Engine build tool
# ("castle-engine" binary, "castle-engine.exe" on Windows).
# See https://castle-engine.io/build_tool .
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
# ----------------------------------------------------------------------------
#

# Allow calling this script from it's dir.
if [ -f castle-engine.lpr ]; then cd ../../; fi

mkdir -p tools/build-tool/castle-engine-output/build-tool-compilation

COMPILE_OPTIONS='-dRELEASE
  -dCASTLE_STRICT_CLI
  @castle-fpc.cfg
  -FEtools/build-tool/
  -FUtools/build-tool/castle-engine-output/build-tool-compilation
  -Futools/common-code/
  -Futools/build-tool/code/
  -Futools/build-tool/embedded_images/'

if ! fpc ${COMPILE_OPTIONS} ${CASTLE_FPC_OPTIONS:-} tools/build-tool/castle-engine.lpr | tee tools/build-tool/castle-engine-output/build-tool-compilation/output.txt; then
  if grep -F 'Fatal: Internal error' tools/build-tool/castle-engine-output/build-tool-compilation/output.txt; then
    echo '-------------------------------------------------------------'
    echo 'It seems FPC crashed. If you can reproduce this problem, please report it to http://bugs.freepascal.org/ ! We want to help FPC developers to fix this problem, and the only way to do it is to report it. If you need help creating a good bugreport, speak up on the FPC mailing list or Castle Game Engine forum.'
    echo
    echo "As a workaround, right now we'll clean everything and try compiling again."
    echo '-------------------------------------------------------------'
    rm -Rf tools/build-tool/castle-engine-output/build-tool-compilation/*
    fpc ${COMPILE_OPTIONS} ${CASTLE_FPC_OPTIONS:-} tools/build-tool/castle-engine.lpr
  else
    exit 1
  fi
fi
