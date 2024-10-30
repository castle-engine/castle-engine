#!/usr/bin/env bash
set -eu

# ----------------------------------------------------------------------------
# Compile the library.
#
# Executes the build tool, which in turn will execute FPC.
# The build tool searches for FPC on $PATH.
#
# Note that the compilation artifacts will be in ./castle-engine-output/ , and that's good,
# we want separate compilation artifacts (from other projects) to:
#
# 1. make sure that CastleWindow unit is compiled with LIBRARY backend,
# 2. make sure that every unit is recompiled with -fPIC (necessary for .so on x86_64).
# ----------------------------------------------------------------------------

# Never use cache, as it will likely contain CastleWindow with different backend, and units without -fPIC.
castle-engine cache-clean

castle-engine simple-compile \
  --compiler-option=-fPIC \
  --compiler-option=-dCASTLE_WINDOW_LIBRARY \
  --verbose \
  castleengine.lpr
