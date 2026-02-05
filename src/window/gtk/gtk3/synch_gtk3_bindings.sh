#!/bin/bash
set -euo pipefail

# ------------------------------------------------------------------------------
# Create units in gtk3bindings/ subdirectory,
# GTK 3 bindings to be used by Castle Game Engine,
# based on Lazarus gtk3bindings.
# Set $LAZARUS_DIR environment variable to point to Lazarus source directory.
# ------------------------------------------------------------------------------

cp -R "${LAZARUS_DIR}/lcl/interfaces/gtk3/gtk3bindings/" .
cd gtk3bindings/
for F in laz*.pas; do mv "$F" "${F/laz/castleinternal}"; done

do_rename_units ()
{
  local PASCAL_CASE
  PASCAL_CASE="$1"
  shift 1
  local LOWER_CASE
  LOWER_CASE="$(echo "$PASCAL_CASE" | tr '[:upper:]' '[:lower:]')"

  # add castle_gtk3bindings_conf.inc to every unit
  local THIS_UNIT_FILE_NAME
  THIS_UNIT_FILE_NAME="castleinternal${LOWER_CASE}.pas"
  # shellcheck disable=SC2016
  sed --in-place \
    -e 's|^interface$|{$I ../castle_gtk3bindings_conf.inc}\ninterface|' \
    "${THIS_UNIT_FILE_NAME}"

  # replace in all files, to replace also references from other units here
  for UNIT_FILE_NAME in castleinternal*.pas; do
    # sed --in-place \
    #   -e "s|unit laz${LOWER_CASE};|unit CastleInternal${PASCAL_CASE};|gi" \
    #   "${UNIT_FILE_NAME}"
    echo "Replacing laz${LOWER_CASE} -> CastleInternal${PASCAL_CASE} in ${UNIT_FILE_NAME}"
    sed --in-place \
      -e "s|unit laz${LOWER_CASE};|unit CastleInternal${PASCAL_CASE};|gi" \
      -e "s|laz${LOWER_CASE}\.|CastleInternal${PASCAL_CASE}.|gi" \
      -e "s|, laz${LOWER_CASE}|, CastleInternal${PASCAL_CASE}|gi" \
      "${UNIT_FILE_NAME}"
  done
}

do_rename_units Atk1
do_rename_units Cairo1
do_rename_units Freetype2_2
do_rename_units Gdk3
do_rename_units GdkPixbuf2
do_rename_units Gio2
do_rename_units Glib2
do_rename_units GModule2
do_rename_units GObject2
do_rename_units Gtk3
do_rename_units Harfbuzz0
do_rename_units JSCore3
do_rename_units Pango1
do_rename_units PangoCairo1
do_rename_units Soup2_4
do_rename_units WebKit3
do_rename_units Xlib2

# remove some unnecessary units from LCL gtk3bindings
rm -f castleinternaljscore3.pas \
      castleinternalpangocairo1.pas \
      castleinternalsoup2_4.pas \
      castleinternalwebkit3.pas
