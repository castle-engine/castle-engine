#!/bin/bash
set -eu

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
# *not* be Cygwin's POSIX path, because I pass it to pasdoc as filename".

# TARGET_OS is windows lub unix.
# This says which subdirectories of sources are meaningfull
# (e.g. add base/unix/ or base/windows/ to include file path ?).
# Moreover pasdoc will be run with --define $TARGET_OS.
TARGET_OS=unix

CASTLE_ENGINE_UNITS_PATH=../../src/

# Autodetect if we're under Cygwin
if uname | grep --quiet -i cygwin; then
  KAMBI_IS_CYGWIN='t'
else
  KAMBI_IS_CYGWIN=''
fi

PASDOC_FORMAT="$1"
shift 1

# calculate OUTPUT_PATH (os-native path)
OUTPUT_PATH=`pwd`/
if [ -n "$KAMBI_IS_CYGWIN" ]; then
  OUTPUT_PATH="`cygpath --windows \"$OUTPUT_PATH\"`"
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
if [ -n "$KAMBI_IS_CYGWIN" ]; then
  TMP_PAS_LIST="`cygpath --windows \"$TMP_PAS_LIST\"`"
fi

# make sure we have clean way to create "$TMP_PAS_LIST"
rm -f "$TMP_PAS_LIST"

# Now generate $TMP_PAS_LIST contents -- those are units we want to
# be documented by pasdoc.

if (( $# == 0 )); then
  # We don't generate docs for a lot of automatically generated units
  # CastleOutlineFont_Xxx and CastleBitmapFont_Xxx, docs for these units would be useless.
  # (all these units have the same structure, after all).
  #
  # Don't generate docs for units created only for example programs.
  #
  # Don't generate docs for pk3dconnexion.pas, tdxinput_tlb.pas:
  # external code, not ready for pasdoc.
  #
  # Don't generate docs for units in base/android/: these should be treated
  # as internal units.
  find .  \
    '(' -type d '(' -iname old -or \
                    -iname private \
                ')' -prune ')' -or \
    '(' -type f -iname '*.pas' \
            -not '(' \
              '(' -iwholename '*/base/android/*.pas' ')' -or \
              '(' -iwholename '*/castlelib_dynloader.pas' ')' -or \
              '(' -iwholename '*/castlegles20.pas' ')' -or \
              '(' -iwholename '*/opengl/x86_64/glext.pas' ')' -or \
              '(' -iwholename '*ui/pk3dconnexion.pas' ')' -or \
              '(' -iwholename '*ui/windows/tdxinput_tlb.pas' ')' -or \
              '(' -iname 'x3dloadinternal*.pas' ')' -or \
              '(' -iname 'castleinternal*.pas' ')' -or \
              '(' -iname 'castleshapeinternal*.pas' ')' -or \
              '(' -iwholename '*fonts/castleoutlinefont_*.pas' ')' -or \
              '(' -iwholename '*fonts/castlebitmapfont_*.pas' ')' \
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
  --include 3d/\
  --include 3d/opengl/\
  --include x3d/\
  --include x3d/opengl/\
  --include audio/\
  --include base/\
  --include base/$TARGET_OS/\
  --include base/opengl/\
  --include fonts/
  --include fonts/$TARGET_OS/\
  --include fonts/opengl/\
  --include images/\
  --include images/opengl/\
  --include window/\
  --include window/$TARGET_OS/\
  --include window/gtk/
"

pasdoc \
   --format "$PASDOC_FORMAT" \
  $PASDOC_INCLUDE_DIRS \
  --output "$OUTPUT_PATH" \
  --define "$TARGET_OS" \
  --define FPC --define VER2 --define VER2_6 --define VER2_6_0 --define PASDOC \
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
  --footer ../doc/pasdoc/footer.html \
  --description=../src/x3d/x3dnodes_documentation.txt

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
