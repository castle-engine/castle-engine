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
# Unused for now: This is cool, but for now it's OK to list ~20 projects.
# do_all_projects ()
# {
#   cd "${CASTLE_ENGINE_PATH}"
#
# 	"${FIND}" . \
# 	  '(' -path ./tools -prune ')' -o \
# 	  '(' -path ./examples/network/tcp_connection -prune ')' -o \
# 	  '(' -path ./tests/delphi_tests -prune ')' -o \
# 	  '(' -path ./examples/delphi -prune ')' -o \
#     '(' -path ./src/deprecated_library -prune ')' -o \
# 	  '(' -path ./examples/deprecated_library -prune ')' -o \
# 	  '(' -type d -iname castle-engine-output -prune ')' -o \
# 	  '(' -type f -iname CastleEngineManifest.xml -print ')' > \
# 	  /tmp/cge-projects.txt
# 	echo 'Found projects: '`wc -l < /tmp/cge-projects.txt`
#
# 	for MANIFEST in `cat /tmp/cge-projects.txt`; do
#     local PROJECT_SUBDIR=`dirname ${MANIFEST}`
# 	  echo 'Compiling project '${PROJECT_SUBDIR}
#     do_project ${PROJECT_SUBDIR}
# 	done
# }

# main code ---------------------------------------------------------------

#do_all_projects

# Examples tested on the web now:

# initial (simplest) web demos
do_project examples/web/simplest
do_project examples/web/simplest_viewport
do_project examples/web/simplest_invaders

# 2d
do_project examples/platformer
do_project examples/component_gallery
do_project examples/animations/play_animation
do_project examples/space_shooter
do_project examples/tiled/map_viewer # TODO: web: no open dialog (works, but could be more impressive)
do_project examples/tiled/strategy_game_demo

# 3d
do_project examples/ifc # TODO: web: no open dialog (works, but could be more impressive)
do_project examples/user_interface/zombie_fighter
do_project examples/viewport_and_scenes/cars_demo
do_project examples/viewport_and_scenes/collisions

# TODO: web: screen effects don't show, also demo crashes on effects switch
# do_project examples/screen_effects_demo

# TODO: web: without WebAudio support, these will be silent; also game_3d_sound likely crashes due to PBR
# do_project examples/audio/game_3d_sound
# do_project examples/audio/play_sounds # proably works, but silent without WebAudio

# TODO: web: PBR shaders fail compilaton on WebGL
# do_project examples/eye_of_beholder
# do_project examples/creature_behaviors
# do_project examples/fps_game
# do_project examples/terrain
# do_project examples/third_person_navigation

# now in cge-www: commit + push, and update website
