#!/usr/bin/env bash
set -eu

# ----------------------------------------------------------------------------
# Compile the library as universal binary for macOS (x86_64 and arm64 slices).
# ----------------------------------------------------------------------------

castle-engine simple-compile \
  --target=macos \
  --compiler-option=-fPIC \
  --compiler-option=-dCASTLE_WINDOW_LIBRARY \
  --verbose \
  castleengine.lpr
