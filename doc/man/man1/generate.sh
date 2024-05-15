#!/usr/bin/env bash
set -eu

# Build tools and generate manual pages for them.

CGEDIR=../../..

generate_man ()
{
  DIR="$1"
  NAME="$2"
  SHORT_DESCRIPTION="$3"
  shift 3

  pushd "$CGEDIR"
  "tools/${DIR}/${NAME}_compile.sh"
  popd

  help2man --section=1 \
    --no-info \
    --source='Castle Game Engine' \
    --name="${SHORT_DESCRIPTION}" \
    --output="${NAME}.1" \
    "${CGEDIR}/tools/${DIR}/${NAME}"
}

generate_man texture-font-to-pascal texture-font-to-pascal 'convert ttf font to a Pascal source file'
generate_man image-to-pascal image-to-pascal 'convert image files into Pascal source code'
generate_man castle-curves castle-curves 'create and edit curves for Castle Game Engine'
generate_man build-tool castle-engine 'build and package Castle Game Engine programs'
