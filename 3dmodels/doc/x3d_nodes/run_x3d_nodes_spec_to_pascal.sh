#!/bin/bash

for F in 2/*.txt; do
  echo "---- $F";
  x3d_nodes_spec_to_pascal < "$F" > `basename "$F" .txt`.inc
done
