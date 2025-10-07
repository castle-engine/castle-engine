#!/bin/bash
set -eu

echo '----------------------------------------------------------------------'
echo 'Before:'

find . \
  -type f \
  '(' -iname '*.png' -or -iname '*.jpg' ')' \
  '(' -not -ipath '*castle_game_engine_icon.png' ')' \
  '(' -not -ipath '*/skybox/*' ')' \
  -exec identify -format '%w %h %f\n' {} \;

echo '----------------------------------------------------------------------'
echo 'Converting'

find . \
  -type f \
  '(' -iname '*.png' -or -iname '*.jpg' ')' \
  '(' -not -ipath '*castle_game_engine_icon.png' ')' \
  '(' -not -ipath '*/skybox/*' ')' \
  -exec convert {} -resize 1024x1024 {} \;

echo '----------------------------------------------------------------------'
echo 'After:'

find . \
  -type f \
  '(' -iname '*.png' -or -iname '*.jpg' ')' \
  '(' -not -ipath '*castle_game_engine_icon.png' ')' \
  '(' -not -ipath '*/skybox/*' ')' \
  -exec identify -format '%w %h %f\n' {} \;
