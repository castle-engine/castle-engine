#!/bin/bash
set -eu

castle-engine compile

./combine_images_into_sprite_sheet \
  '../sprite_sheet_animation/data/hero_animation/single_frames_24fps/hawaii_exo-walking_@counter(1).png' \
  ../sprite_sheet_animation/data/hero_animation/hero_sprite_sheet_24fps.png \
  6

./combine_images_into_sprite_sheet \
  '../sprite_sheet_animation/data/hero_animation/single_frames_60fps/hawaii_exo-walking_@counter(1).png' \
  ../sprite_sheet_animation/data/hero_animation/hero_sprite_sheet_60fps.png \
  12
