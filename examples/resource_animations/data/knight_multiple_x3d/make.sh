#!/bin/bash
set -eu

# We create .x3d files using our
# 3d_rendering_processing/tools/castle_anim_frames_to_interpolators.lpr tool.
# This script assumes you compiled castle_anim_frames_to_interpolators and put it on $PATH.

doit ()
{
  N="$1"
  shift 1

  castle_anim_frames_to_interpolators coords_ME_Knight ../knight_multiple_castle_anim_frames/"$N".castle-anim-frames "$N".x3dv
}

doit idle
doit walk
doit attack
doit die
doit hurt
