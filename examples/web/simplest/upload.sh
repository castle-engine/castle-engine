#!/bin/bash
set -eu

castle-engine clean
castle-engine compile --target=web --mode=release

# upload?
# rsync -av castle-engine-output/web/dist/ \
#   ssh.castle-engine.io:/home/michalis/cge-www/htdocs/web-demos/simplest

CGE_WWW_DIST="${CASTLE_ENGINE_PATH}/../cge-www/htdocs/web-demos/simplest"
rm -Rf "${CGE_WWW_DIST}"
cp -R castle-engine-output/web/dist "${CGE_WWW_DIST}"

# now commit in cge-www and update website
