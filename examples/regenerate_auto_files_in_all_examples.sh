#!/bin/bash
set -eu

# ----------------------------------------------------------------------------
# For most projects, run "castle-engine generate-program" to regenerate
#
# - lpi
# - dproj
# - dpr (if game_units are specified in manifest)
#
# Exceptions are:
#
# - LCL projects (leave LPI to be managed using Lazarus and specify LCL packages,
#   and DPROJ doesn't make sense for these projects)
# - simplest_manifest_test (we deliberately do not store dpr/lpi/dproj in this demo).
# ----------------------------------------------------------------------------

FIND='find'
if which cygpath.exe > /dev/null; then
  FIND='/bin/find' # On Cygwin, make sure to use Cygwin's find, not the one from Windows
fi

"${FIND}" \
  '(' -iname CastleEngineManifest.xml ')' -and \
  '(' -not -iwholename '*/simplest_manifest_test/*' ')' -and \
  '(' -not -iwholename '*/delphi/*' ')' -and \
  '(' -execdir bash -c 'if ls *.lfm > /dev/null 2>&1; then echo `pwd`": LCL project"; else castle-engine generate-program; fi' ';' ')'
