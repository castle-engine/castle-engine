#!/bin/bash
set -eu

# We create .x3d files using our
# 3d_rendering_processing/tools/kanim_to_interpolators.lpr tool.
# This script assumes you compiled kanim_to_interpolators and put it on $PATH.

doit ()
{
  N="$1"
  shift 1

  kanim_to_interpolators coords_ME_Knight ../knight_kanim/"$N".kanim "$N".x3dv
}

doit idle
doit walk
doit attack
doit die
doit hurt
