#!/usr/bin/env bash
set -euo pipefail

# Compile the auto-tests.
# In release mode (tests must pass in both debug and release), with text runner.

castle-engine --mode=release compile
