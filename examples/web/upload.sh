#!/bin/bash
set -eu

# ------------------------------------------------------------------------
# Rebuild all web demos linked from https://castle-engine.io/web .
# Copy their output to the cge-www repository.
# ------------------------------------------------------------------------

CGE_WWW_DEMOS="${CASTLE_ENGINE_PATH}/../cge-www/htdocs/web-demos"

# define $FIND, we want GNU find
FIND='find'
if which cygpath.exe > /dev/null; then
  # On Cygwin/MSYS2, make sure to use Cygwin/MSYS2's find, not the one from Windows
  FIND='/bin/find'
fi
if [ "`uname -s`" '=' 'Darwin' ]; then
  FIND='gfind'
fi

# Rebuild and copy project $1 .
#
# $1 should be a directory in CGE, like examples/web/simplest_viewport
#
# (it can start with "./" to indicate current dir -- doesn't change anything,
# but it's accepted, as "find" will return paths like
# "./examples/web/simplest_viewport").
do_project ()
{
  local PROJECT_SUBDIR="$1"
  shift 1

  cd "${CASTLE_ENGINE_PATH}/${PROJECT_SUBDIR}"
  castle-engine clean
  castle-engine compile --target=web --mode=release

  CGE_WWW_DIST="${CGE_WWW_DEMOS}/${PROJECT_SUBDIR}"
  rm -Rf "${CGE_WWW_DIST}"
  mkdir -p "${CGE_WWW_DIST}"
  cp -R castle-engine-output/web/dist/* "${CGE_WWW_DIST}"
}

# Find and build *all* projects in CGE, with only some exceptions.
do_all_projects ()
{
  cd "${CASTLE_ENGINE_PATH}"

	"${FIND}" . \
	  '(' -path ./tools/ -prune ')' -o \
	  '(' -path ./examples/network/tcp_connection -prune ')' -o \
	  '(' -path ./tools/castle-editor/data/project_templates -prune ')' -o \
	  '(' -path ./tools/build-tool -prune ')' -o \
	  '(' -path ./tests/delphi_tests -prune ')' -o \
	  '(' -path ./examples/delphi -prune ')' -o \
	  '(' -path ./examples/deprecated_library/lazarus_library_tester -prune ')' -o \
	  '(' -type d -iname castle-engine-output -prune ')' -o \
	  '(' -type f -iname CastleEngineManifest.xml -print ')' > \
	  /tmp/cge-projects.txt
	echo 'Found projects: '`wc -l < /tmp/cge-projects.txt`

	for MANIFEST in `cat /tmp/cge-projects.txt`; do
    local PROJECT_SUBDIR=`dirname ${MANIFEST}`
	  echo 'Compiling project '${PROJECT_SUBDIR}
    # do_project `dirname ${MANIFEST}`
	done
}

# main code

#do_all_projects

do_project examples/web/simplest
do_project examples/web/simplest_viewport
do_project examples/web/simplest_invaders

# now in cge-www: commit + push, and update website
