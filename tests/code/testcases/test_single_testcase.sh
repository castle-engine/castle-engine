#!/bin/bash
set -eu

castle-engine --mode=debug compile
castle-engine run -- --console --filter="$1.*"
