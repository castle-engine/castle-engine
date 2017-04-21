#!/bin/bash

for F in ../components/*.txt; do
  echo "---- $F";
  BBB=`basename "$F" .txt`
  x3d_nodes_spec_to_pascal "$BBB" < "$F" > x3d_`stringoper LowerCase "$BBB"`.inc
done
