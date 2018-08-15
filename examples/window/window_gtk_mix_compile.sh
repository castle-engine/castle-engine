#!/bin/bash
set -eu

# Call this script from this directory,
# or from base castle_game_engine directory.
# Or just do "make examples" in base castle_game_engine directory.

# Allow calling this script from it's dir.
if [ -f window_gtk_mix.lpr ]; then cd ../../; fi

# Find the build tool, use it to compile
if which tools/build-tool/castle-engine > /dev/null; then
  CASTLE_ENGINE="`pwd`/tools/build-tool/castle-engine"
else
  CASTLE_ENGINE=castle-engine
fi

# Use -dCASTLE_WINDOW_GTK_2 to make sure that CastleWindow unit is compiled
# with GTK2 backend (this is not the default on Windows).
# Do not make other examples in this dir compiled with GTK_2 backend
rm -Rf castle-engine-output

"${CASTLE_ENGINE}" simple-compile --compiler-option=-dCASTLE_WINDOW_GTK_2 \
  examples/window/window_gtk_mix.lpr

# Do not make other examples in this dir compiled with GTK_2 backend
rm -Rf castle-engine-output
