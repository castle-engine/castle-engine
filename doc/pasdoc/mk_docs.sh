#!/bin/bash
set -eu

# Use pipefail to fail if the "pasdoc"
# command in "pasdoc ... | grep ..." pipe fails.
# See http://unix.stackexchange.com/questions/14270/get-exit-status-of-process-thats-piped-to-another
set -o pipefail

# Generates docs for all units in castle_game_engine/src.
# $1 is format (allowed values as for pasdoc's --format option),
# docs will be placed in subdirectory $1 of current dir
# so $1 means also subdirectory name.
#
# If you will not supply argument $2,
# then documentation for almost every unit in castle_game_engine/src
# (without automatically generated fonts units
# and without units that pasdoc can't correctly parse)
# will be generated.
#
# Else documentation for units $2...${$#} (all these arguments
# should be filenames of units relative to $CASTLE_ENGINE_UNITS_PATH
# directory) will be generated.
# E.g.: ./mk_docs.sh html audio/alutils.pas
# This is useful for quickly checking generated documentation
# for some particular unit, because generating documentation
# for all units takes a while.

# "os-native path" in this file means "under Windows it must
# *not* be Cygwin/MinGW/MSys/Git-for-Windows POSIX path,
# because I pass it to pasdoc as filename".

# TARGET_OS is windows lub unix.
# This says which subdirectories of sources are meaningfull
# (e.g. add base/unix/ or base/windows/ to include file path ?).
# Moreover pasdoc will be run with --define $TARGET_OS.
TARGET_OS=unix

CASTLE_ENGINE_UNITS_PATH=../../src/

# Autodetect if we're under Cygwin/MinGW/MSys/Git-for-Windows --
# so paths from Unix tools (like pwd) need conversion to Windows-native.
#if uname | grep --quiet -i cygwin; then # not good, does not detect MinGW/MSys
if which cygpath > /dev/null; then
  CYGWIN_OR_SIMILAR='t'
else
  CYGWIN_OR_SIMILAR=''
fi

PASDOC_FORMAT="$1"
shift 1

# calculate OUTPUT_PATH (os-native path)
OUTPUT_PATH=`pwd`/
if [ -n "$CYGWIN_OR_SIMILAR" ]; then
  OUTPUT_PATH="`cygpath --windows \"$OUTPUT_PATH\"`"
fi

FIND='find'
if [ -n "$CYGWIN_OR_SIMILAR" ]; then
  FIND='/bin/find' # On Cygwin, make sure to use Cygwin's find, not the one from Windows
fi

# calculate PASDOC_CACHE (os-native path)
# I use --cache-dir with pasdoc, as this greatly speeds up generation
# of these docs.
#
# (However, this presents possibe threat, because cache timestamp is based
# on unit source file timestamp. So when some unit myunit.pas will include
# myinclude.inc then cache will NOT be updated when myinclude.inc file
# changes. So I'll have to remember to call `make clean-cache' here before
# regenerating docs.)
PASDOC_CACHE="${OUTPUT_PATH}cache/"
mkdir -p "$PASDOC_CACHE"

# finish calculating OUTPUT_PATH
OUTPUT_PATH="$OUTPUT_PATH""$PASDOC_FORMAT"/
mkdir -p "$OUTPUT_PATH"

cd "$CASTLE_ENGINE_UNITS_PATH"

# calculate TMP_PAS_LIST (os-native path)
TMP_PAS_LIST=/tmp/mk_docs_list
if [ -n "$CYGWIN_OR_SIMILAR" ]; then
  TMP_PAS_LIST="`cygpath --windows \"$TMP_PAS_LIST\"`"
fi

# make sure we have clean way to create "$TMP_PAS_LIST"
rm -f "$TMP_PAS_LIST"

# Now generate $TMP_PAS_LIST contents -- those are units we want to
# be documented by pasdoc.

if (( $# == 0 )); then
  # We don't generate docs for
  #
  # - Automatically generated units (fonts).
  #
  # - Internal units.
  #   All the non-internal units should have nice PasDoc documentation.
  #
  #   Internal units sometimes have internal API, sometimes they even
  #   describe an "external API", not interesting to CGE developers,
  #   like castleinternalpk3dconnexion.pas and castleinternaltdxinput_tlb.pas.
  #
  #   There are two units that are "somewhat internal":
  #   glext.pas, castlegles.pas. We want them to be treated as internal
  #   for most games. But sometimes users can also use them directly,
  #   to do some advanced tricks (direct OpenGL / OpenGLES rendering,
  #   mixed with CGE rendering).
  #   In any case, their docs are not ready for PasDoc (and never will be
  #   --- this is OpenGL and OpenGL ES API).
  #
  #   Exception: we still generate docs for castlevectorsinternal*.pas.
  #   Although these units are not supposed to be used directly,
  #   but they document API of TVector3 and TVector3Double.

  "${FIND}" .  \
    '(' -type f -iname '*.pas' \
            -not '(' \
              '(' -iwholename '*/base/android/*.pas' ')' -or \
              '(' -iwholename '*/castlelib_dynloader.pas' ')' -or \
              '(' -iwholename '*/castlegles.pas' ')' -or \
              '(' -iname 'x3dloadinternal*.pas' ')' -or \
              '(' -iname 'castleinternal*.pas' ')' -or \
              '(' -iname 'fmx.castleinternal*.pas' ')' -or \
              '(' -iname 'castleshapeinternal*.pas' ')' -or \
              '(' -iname 'kraft.pas' ')' -or \
              '(' -iname 'dglopengl*.pas' ')' -or \
              '(' -iname 'castlegl.pas' ')' -or \
              '(' -iwholename '*/compatibility/*' ')' -or \
              '(' -iwholename '*/deprecated_units/*' ')' -or \
              '(' -iwholename '*/pasgltf/*' ')' -or \
              '(' -iwholename '*/vampyre_imaginglib/*' ')' -or \
              '(' -iwholename '*/x3d/nodes_specification/*' ')' -or \
              '(' -iwholename '*fonts/castletexturefont_*.pas' ')' \
            ')' \
            -print ')' >> "$TMP_PAS_LIST"
else
  # Put all "$@" arguments in file "$TMP_PAS_LIST",
  # separated by newlines
  for FFF; do
    echo "$FFF" >> "$TMP_PAS_LIST"
  done
fi

PASDOC_INCLUDE_DIRS="\
  --include common_includes/\
  --include transform/\
  --include scene/\
  --include scene/load/\
  --include scene/load/spine/\
  --include scene/load/md3/\
  --include scene/load/collada/\
  --include scene/x3d/\
  --include audio/\
  --include base/\
  --include base/$TARGET_OS/\
  --include base_rendering/\
  --include fonts/
  --include fonts/$TARGET_OS/\
  --include images/\
  --include window/\
  --include window/$TARGET_OS/\
  --include window/gtk/
"

if [ "${PASDOC_FORMAT}" = 'html' ]; then
  FORMAT_OPTIONS='--use-tipue-search'
else
  FORMAT_OPTIONS=''
fi

# Run pasdoc.
#
# Filter result through grep.
# Thanks to "set -o pipefail" defined above, failure of "pasdoc"
# will still cause the entire script to fail.
#
# We filter out some known pasdoc bugs/missing features:
# - lack of @groupbegin/groupend implementation for now,
# - reporting as missing links the exceptions from standard units.

pasdoc \
  --format "$PASDOC_FORMAT" \
  $PASDOC_INCLUDE_DIRS \
  --output "$OUTPUT_PATH" \
  --define "$TARGET_OS" \
  --define FPC --define VER3 --define VER3_0 --define VER3_2_0 --define PASDOC \
  --write-uses-list \
  --title "Castle Game Engine" \
  --source "$TMP_PAS_LIST" \
  --cache-dir "$PASDOC_CACHE" \
  --auto-abstract \
  --introduction=../doc/pasdoc/introduction.pasdoc \
  --auto-link \
  --auto-link-exclude=../doc/pasdoc/auto_link_exclude.txt \
  --external-class-hierarchy=../doc/pasdoc/external_class_hierarchy.txt \
  --visible-members public,published,automated,protected \
  --html-head ../doc/pasdoc/html-parts/head.html \
  --html-body-begin ../doc/pasdoc/html-parts/body-begin.html \
  --html-body-end ../doc/pasdoc/html-parts/body-end.html \
  --css ../doc/pasdoc/html-parts/cge-pasdoc.css \
  $FORMAT_OPTIONS

  # TODO: Commented out grep filtering -- fails with "Disk Full" on GH Actions,
  # seems some problem with pipes.
  #
  #  \
  # | \
  # grep --ignore-case --invert-match --fixed-strings \
  #   --regexp='Tag "groupbegin" is not implemented yet, ignoring' \
  #   --regexp='Tag "groupend" is not implemented yet, ignoring' \
  #   --regexp='Could not resolve link "EConvertError"' \
  #   --regexp='Could not resolve link "EReadError"' \
  #   --regexp='Could not resolve link "Exception"' \
  #   --regexp='Could not resolve link "EOSError"' \
  #   --regexp='Could not resolve link "EInvalidArgument"' \
  #   --regexp='Could not resolve link "EFOpenError"' \
  #   --regexp='Could not resolve link "EStreamError"'

# Not anymore:
# We hide protected members, for now. Makes a cleaner documentation,
# more useful for engine users, not interested in engine internals.

#  \
# --graphviz-classes --link-gv-classes jpg \
# --graphviz-uses    --link-gv-uses    jpg

# Useful to find new items for auto_link_exclude.txt:
# grep for "Automatically linked identifier", remove duplicates,
# and see which ones shouldn't be autolinked.
# --verbosity=3

# Classes graph is too large, dot answers with
#   dot: width (135399 >= 32768) is too large.
# and then segfaults.
#dot -Tjpg -o"$OUTPUT_PATH"GVClasses.jpg "$OUTPUT_PATH"GVClasses.dot

# Units graph is possible, but still very large for human eye
# and so practically useless for us now...
#dot -Tjpg -o"$OUTPUT_PATH"GVUses.jpg    "$OUTPUT_PATH"GVUses.dot

# clean after ourselves
rm -f "$TMP_PAS_LIST"
