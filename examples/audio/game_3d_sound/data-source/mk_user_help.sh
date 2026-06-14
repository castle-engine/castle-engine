#!/bin/bash
set -eu

convert user_help_blank.png \
  -font helvetica -fill blue -pointsize 11 \
  -draw "text 20,30 '$(<user_help_text.txt)'" \
  ../data/textures/user_help.png
