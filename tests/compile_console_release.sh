#!/usr/bin/env bash
set -euo pipefail

# Compile the auto-tests.
# In release mode (tests must pass in both debug and release), with text runner.
# One optional additional parameter possible, like -dXXX.

castle-engine --mode=release compile
