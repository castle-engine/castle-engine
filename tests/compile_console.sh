#!/usr/bin/env bash
set -euo pipefail

# Compile the auto-tests.
# In debug mode, with text runner.

castle-engine --mode=debug compile --verbose
