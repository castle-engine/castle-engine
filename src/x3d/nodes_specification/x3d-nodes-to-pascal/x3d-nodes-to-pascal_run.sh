#!/bin/bash
set -eu

rm -f ../../auto_generated_node_helpers/x3dnodes_*.inc

./x3d-nodes-to-pascal ../components/*.txt
