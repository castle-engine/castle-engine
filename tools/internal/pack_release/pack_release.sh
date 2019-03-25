#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Pack Castle Game Engine release (source + binaries).
# Uses bash strict mode, see http://redsymbol.net/articles/unofficial-bash-strict-mode/

do_pack_platform ()
{
  local OS="$1"
  local CPU="$2"
  shift 2

  case "$OS" in
    win32|win64) local EXE_EXTENSION='.exe' ;;
    *)           local EXE_EXTENSION=''     ;;
  esac

  # pass options to compile to this OS/CPU downward to everything
  export CASTLE_FPC_OPTIONS="-T${OS} -P${CPU}"
  export CASTLE_BUILD_TOOL_OPTIONS="--os=${OS} --cpu=${CPU}"
  local  CASTLE_LAZBUILD_OPTIONS="--os=${OS} --cpu=${CPU}"

  cd "$CASTLE_ENGINE_PATH"
  make cleanmore

  if ! which castle-engine > /dev/null; then
    echo 'pack_release: After "make cleanmore" in CGE, castle-engine tool is no longer available'
    exit 1
  fi

  make tools
  IFS=$' \n\t' # split on space too
  lazbuild $CASTLE_LAZBUILD_OPTIONS tools/castle-editor/code/castle_editor.lpi
  IFS=$'\n\t'

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
  # Add DLLs on Windows
  case "$OS" in
    win32|win64)
      cp "${CASTLE_ENGINE_PATH}"/tools/build-tool/data/external_libraries/"${CPU}"-"${OS}"/*.dll \
         "${CASTLE_ENGINE_PATH}"/tools/build-tool/data/external_libraries/"${CPU}"-"${OS}"/openssl/*.dll \
         "$TEMP_PATH"bin
      ;;
  esac

  make cleanmore

  # TODO: Add pasdoc docs:
  # cd "$MK_ARCHIVE_TEMP_PATH"castle_game_engine/doc/pasdoc/
  # make clean html
  # rm -Rf "$MK_ARCHIVE_TEMP_PATH"castle_game_engine/doc/pasdoc/cache/

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

do_pack_platform win64 x86_64
do_pack_platform win32 i386
do_pack_platform linux x86_64
