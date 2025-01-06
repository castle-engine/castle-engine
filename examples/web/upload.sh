#!/bin/bash
set -eu

# ------------------------------------------------------------------------
# Rebuild all web demos linked from https://castle-engine.io/web .
# Copy their output to the cge-www repository.
# ------------------------------------------------------------------------

CGE_WWW_DEMOS="${CASTLE_ENGINE_PATH}/../cge-www/htdocs/web-demos"

# Rebuild and copy project $1
do_project ()
{
  cd "${CASTLE_ENGINE_PATH}/examples/web/$1"

  castle-engine clean
  castle-engine compile --target=web --mode=release

  CGE_WWW_DIST="${CGE_WWW_DEMOS}/${1}"
  rm -Rf "${CGE_WWW_DIST}"
  cp -R castle-engine-output/web/dist "${CGE_WWW_DIST}"
}

# main code

do_project simplest
do_project simplest_viewport
do_project simplest_invaders

# now in cge-www: commit + push, and update website
