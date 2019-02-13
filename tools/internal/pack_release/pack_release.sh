#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Pack Castle Game Engine release (source + binaries).
# Uses bash strict mode, see http://redsymbol.net/articles/unofficial-bash-strict-mode/

do_pack_platform ()
{
  local OS="$1"
  local CPU="$2"
  local EXE_EXTENSION="$3"
  shift 2

  cd "$CASTLE_ENGINE_PATH"
  make cleanmore > /dev/null

  export CASTLE_FPC_OPTIONS="-T${OS} -P${CPU}"
  export CASTLE_BUILD_TOOL_OPTIONS="--os=${OS} --cpu=${CPU}"
  make tools
  lazbuild --os="${OS}" --cpu="${CPU}" tools/castle-editor/code/castle_editor.lpi

  local TEMP_PATH=/tmp/castle-engine-release-$$/
  mkdir -p "$TEMP_PATH"bin
  cp tools/build-tool/castle-engine"${EXE_EXTENSION}" \
     tools/texture-font-to-pascal/texture-font-to-pascal"${EXE_EXTENSION}" \
     tools/image-to-pascal/image-to-pascal"${EXE_EXTENSION}" \
     tools/castle-curves/castle-curves"${EXE_EXTENSION}" \
     tools/sprite-sheet-to-x3d/sprite-sheet-to-x3d"${EXE_EXTENSION}" \
     tools/to-data-uri/to-data-uri"${EXE_EXTENSION}" \
     tools/castle-editor/castle-editor"${EXE_EXTENSION}" \
     "$TEMP_PATH"bin

  make cleanmore > /dev/null

  # TODO: Add pasdoc docs:
  # cd "$MK_ARCHIVE_TEMP_PATH"castle_game_engine/doc/pasdoc/
  # make clean html
  # rm -Rf "$MK_ARCHIVE_TEMP_PATH"castle_game_engine/doc/pasdoc/cache/

  # TODO: On Windows, add DLLs from
  #   tools/build-tool/data/external_libraries/${CPU}-${OS}/*.dll
  # to bin/

  mv "$TEMP_PATH"bin .

  local ARCHIVE_NAME="castle-engine-${CGE_VERSION}-${OS}-${CPU}.zip"
  cd ../
  rm -f "${ARCHIVE_NAME}"
  zip --exclude='*/.git*' --exclude='*/.svn*' -r "${ARCHIVE_NAME}" castle-engine/
}

CGE_VERSION=`castle-engine --version | awk '{print $2}'`
echo "Detected CGE version ${CGE_VERSION}"

# Michalis script to switch my FPC/Lazarus installation to given version.
set_fpclazarus_current 3.0.4

# do_pack_platform win64 x86_64 .exe
# do_pack_platform win32 i386 .exe
do_pack_platform linux x86_64 ''
