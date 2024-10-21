#!/bin/bash
set -eu

# --------------------------------------------------------------------
# Make internal documentation of IFC classes.
# --------------------------------------------------------------------

rm -Rf ifc-doc/
mkdir ifc-doc/
pasdoc castleinternalloadsaveifc.pas \
  --define FPC \
  --define PASDOC \
  --include ../../../common_includes/ \
  --output=ifc-doc \
  --title="IFC classes in Castle Game Engine" \
  --auto-link \
  --write-uses-list \
  --auto-abstract \
  --auto-link-exclude=../../../../doc/pasdoc/auto_link_exclude.txt \
  --external-class-hierarchy=../../../../doc/pasdoc/external_class_hierarchy.txt \
  --visible-members public,published,automated,protected
zip -r ifc-doc.zip ifc-doc/

scp ifc-doc.zip 'michalis@ssh.castle-engine.io:/home/michalis/cge-www/htdocs/tmp/'
ssh michalis@ssh.castle-engine.io <<EOF
cd /home/michalis/cge-www/htdocs/tmp/
rm -Rf ifc-doc/
unzip ifc-doc.zip
rm ifc-doc.zip
~/bin/purge_cloudflare.sh
EOF

echo 'Done making IFC documentation.'
echo 'Open it now: https://castle-engine.io/tmp/ifc-doc/'
