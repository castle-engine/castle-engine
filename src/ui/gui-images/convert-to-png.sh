#!/bin/bash
set -eu

# gimp -i -b "`cat convert-to-png.scm`" -b '(gimp-quit 0)'

for F in *.xcf; do
  NEW_F="`basename \"$F\" .xcf`.png"
  echo "Converting $F to $NEW_F using ImageMagick"
  convert -layers flatten -background transparent "$F" "$NEW_F"
done
