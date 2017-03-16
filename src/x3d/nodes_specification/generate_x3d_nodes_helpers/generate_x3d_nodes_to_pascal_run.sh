#!/bin/bash
set -eu

rm -f ../../auto_generated_node_helpers/x3dnodes_*.inc

./generate_x3d_nodes_to_pascal ../components/*.txt
