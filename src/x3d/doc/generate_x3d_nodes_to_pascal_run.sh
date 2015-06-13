#!/bin/bash
set -eu

./generate_x3d_nodes_to_pascal x3d_nodes/2/*.txt  ../../../../www/htdocs/x3d_extensions.txt > ../x3dnodes_node_helpers.inc
