#!/bin/bash
set -eu

INPUT_DIR=./nodes-specification/
OUTPUT_DIR=../../../src/scene/x3d/auto_generated_node_helpers/

rm -f "$OUTPUT_DIR"x3dnodes_*.inc

./x3d-nodes-to-pascal "$INPUT_DIR"*.txt --output-path "$OUTPUT_DIR"
# --verbose
