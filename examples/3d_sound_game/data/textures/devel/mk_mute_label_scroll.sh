#!/bin/sh
set -eu

TILE_WIDTH=`identify -format %w mute_label.png`
TILE_HEIGHT=`identify -format %h mute_label.png`
montage -geometry "$TILE_WIDTH"x"$TILE_HEIGHT"+0+0 -tile 4x1 \
  mute_label.png mute_label.png mute_label.png mute_label.png \
  ../../vrml/textures/mute_label_scroll.png
