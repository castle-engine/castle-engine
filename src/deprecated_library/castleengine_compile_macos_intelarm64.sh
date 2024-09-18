#!/usr/bin/env bash
set -eu

# ----------------------------------------------------------------------------
# Compile the library as universal binary for macOS (x86_64 and arm64 slices).
#
# Note: This script should not be used anymore, it is here to help to polish
#       the workflow, e.g. to check which steps are failing, notarization, etc.
# ----------------------------------------------------------------------------

# Compile x86_64 slice, Apple Notarization requires at least SDK 10.9, so we set it here for fpc
export MACOSX_DEPLOYMENT_TARGET=10.9.0
castle-engine simple-compile \
  --os=macos --cpu=x86_64 \
  --compiler-option=-fPIC \
  --compiler-option=-dCASTLE_WINDOW_LIBRARY \
  --verbose \
  castleengine.lpr
unset MACOSX_DEPLOYMENT_TARGET

mv libcastleengine.dylib libcastleengine.x86_64.dylib

# Compile aarch64 slice
castle-engine simple-compile \
  --os=macos --cpu=aarch64 \
  --compiler-option=-fPIC \
  --compiler-option=-dCASTLE_WINDOW_LIBRARY \
  --verbose \
  castleengine.lpr

mv libcastleengine.dylib libcastleengine.aarch64.dylib

lipo libcastleengine.x86_64.dylib libcastleengine.aarch64.dylib -output libcastleengine.uni.dylib -create