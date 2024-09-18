#!/usr/bin/env bash
set -eu

# ----------------------------------------------------------------------------
# Compile the library as universal binary for macOS (x86_64 and arm64 slices).
# ----------------------------------------------------------------------------

castle-engine compile --target macos
