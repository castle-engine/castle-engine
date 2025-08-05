#!/bin/bash
set -eu

# ------------------------------------------------------------------
# Simple script that sets up desktop file for Castle Game Engine Editor
# in your home directory, so that you have a nice icon in your desktop
# (GNOME, MATE, KDE...) to start the editor.
#
# Relies on CASTLE_ENGINE_PATH being set
# (the current working directory then doesn't matter when you run this script,
# so you can do "cd ../doc/install/linux/ && ./install.sh" or you can do
# "../doc/install/linux/install.sh", it's all the same effect)
# or uses ../../../ .
# ------------------------------------------------------------------

if [ -z "${CASTLE_ENGINE_PATH:-}" ]; then
  echo "Detecting CASTLE_ENGINE_PATH from current working dir."
  CASTLE_ENGINE_PATH=`pwd`
  CASTLE_ENGINE_PATH="`dirname \"${CASTLE_ENGINE_PATH}\"`"
  CASTLE_ENGINE_PATH="`dirname \"${CASTLE_ENGINE_PATH}\"`"
  CASTLE_ENGINE_PATH="`dirname \"${CASTLE_ENGINE_PATH}\"`"
fi

echo "Setting up desktop file for Castle Game Engine in:"
echo "  ${CASTLE_ENGINE_PATH}"

DESKTOP_FILE="${CASTLE_ENGINE_PATH}/doc/install/linux/castle-editor.desktop"
if [ ! -f "${DESKTOP_FILE}" ]; then
  echo "Error: Desktop file not found: ${DESKTOP_FILE}"
  exit 1
fi

desktop-file-install --dir=${HOME}/.local/share/applications ${DESKTOP_FILE}
sed --in-place "s|Exec=.*|Exec=${CASTLE_ENGINE_PATH}/bin/castle-editor|g" ${HOME}/.local/share/applications/castle-editor.desktop
sed --in-place "s|Icon=.*|Icon=${CASTLE_ENGINE_PATH}/doc/pasdoc/logo/castle_game_engine_icon.svg|g" ${HOME}/.local/share/applications/castle-editor.desktop

echo "Desktop file installed OK."

# Debug:
#cat ${HOME}/.local/share/applications/castle-editor.desktop
