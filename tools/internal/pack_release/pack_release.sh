#!/usr/bin/env bash
set -euo pipefail

# ----------------------------------------------------------------------------
# Pack Castle Game Engine release (source + binaries).
#
# Call with:
#
# - 2 arguments, OS and CPU (names matching CGE build tool and FPC),
#   to pack for the given platform. Example:
#
#     ./pack_release.sh linux x86_64
#     ./pack_release.sh win64 x86_64
#     ./pack_release.sh darwin x86_64
#     ./pack_release.sh freebsd x86_64
#
# - no arguments, to pack for "default" platforms.
#   "Default" are desktop target platforms possible thanks to Docker image
#   https://hub.docker.com/r/kambi/castle-engine-cloud-builds-tools/
#   -- right now this means Linux and Windows.
#
# - 1 argument 'windows_installer' to build a Windows installer.
#   This requires InnoSetup installed.
#
# Define CGE_PACK_BUNDLE=yes for a special behavior:
# - The generated archive will be named -bundle
# - We will expect fpc-OS-CPU.zip, and we will unpack it and distribute along with CGE
#
# Uses bash strict mode, see http://redsymbol.net/articles/unofficial-bash-strict-mode/
# (but without IFS modification, deliberately, we want to split on space).
#
# Requires a few Unix utilities, like
# - make, sed
# - and fpc, lazbuild on $PATH
#
# Works on
# - Linux,
# - Windows (with Cygwin),
#   Note: It assumes that Cygwin tools, like cp, are first on $PATH, before equivalent from MinGW in FPC.
#   A simple solution to make sure it's true is to execute
#     export PATH="/bin:$PATH"
#   in Cygwin shell right before this script.
# - FreeBSD (install GNU make and sed),
# - macOS (install GNU sed from Homebrew).
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Define global variables

OUTPUT_DIRECTORY=`pwd`
if which cygpath.exe > /dev/null; then
  OUTPUT_DIRECTORY="`cygpath --mixed \"${OUTPUT_DIRECTORY}\"`"
fi

VERBOSE=false

ORIGINAL_CASTLE_ENGINE_PATH="${CASTLE_ENGINE_PATH}"

# ----------------------------------------------------------------------------
# Define functions

# Require building release with latest stable FPC, as supported by CGE,
# see https://castle-engine.io/supported_compilers.php .
check_fpc_version ()
{
  local FPC_VERSION=`fpc -iV | tr -d '\r'`
  local REQUIRED_FPC_VERSION='3.2.2'
  if [ "${FPC_VERSION}" '!=' "${REQUIRED_FPC_VERSION}" ]; then
    echo "pack_release: Expected FPC version ${REQUIRED_FPC_VERSION}, but got ${FPC_VERSION}"
    exit 1
  fi
}

# Detect some platform-dependent variables
detect_platform ()
{
  if which make.exe > /dev/null; then
    HOST_EXE_EXTENSION='.exe'
  else
    HOST_EXE_EXTENSION=''
  fi
  echo "Host exe extension: '${HOST_EXE_EXTENSION}' (should be empty on Unix, '.exe' on Windows)"

  MAKE='make'
  FIND='find'
  SED='sed'

  if which cygpath.exe > /dev/null; then
    MAKE='/bin/make' # On Cygwin, make sure to use Cygwin's make, not the one from Embarcadero
    FIND='/bin/find' # On Cygwin, make sure to use Cygwin's find, not the one from Windows
  fi

  if [ "`uname -s`" '=' 'FreeBSD' ]; then
    MAKE='gmake'
    SED='gsed'
  fi

  if [ "`uname -s`" '=' 'Darwin' ]; then
    SED='gsed'
  fi
}

# Compile build tool, put it on $PATH
prepare_build_tool ()
{

  if [ "${VERBOSE}" '!=' 'true' ]; then
    CASTLE_FPC_OPTIONS="-vi-"
  fi

  cd "${CASTLE_ENGINE_PATH}"
  tools/build-tool/castle-engine_compile.sh
  local BIN_TEMP_PATH="/tmp/castle-engine-release-bin-$$/"
  mkdir -p "${BIN_TEMP_PATH}"
  cp "tools/build-tool/castle-engine${HOST_EXE_EXTENSION}" "${BIN_TEMP_PATH}"
  export PATH="${BIN_TEMP_PATH}:${PATH}"

  # sanity checks
  if ! which castle-engine > /dev/null; then
    echo 'pack_release: After installing CGE build tool, we still cannot find it on $PATH'
    exit 1
  fi
  FOUND_CGE_BUILD_TOOL="`which castle-engine${HOST_EXE_EXTENSION}`"
  # remove double slashes, may happen in which output because the $PATH component we added ends with slash
  FOUND_CGE_BUILD_TOOL="`echo -n \"${FOUND_CGE_BUILD_TOOL}\" | $SED -e 's|//|/|' -`"
  EXPECTED_CGE_BUILD_TOOL="${BIN_TEMP_PATH}castle-engine${HOST_EXE_EXTENSION}"
  if [ "${FOUND_CGE_BUILD_TOOL}" '!=' "${EXPECTED_CGE_BUILD_TOOL}" ]; then
    echo "pack_release: Unexpected CGE build tool on \$PATH: found ${FOUND_CGE_BUILD_TOOL}, expected ${EXPECTED_CGE_BUILD_TOOL}"
    exit 1
  fi
}

# Calculate $CGE_VERSION
calculate_cge_version ()
{
  CGE_VERSION="`castle-engine --version | awk '{print $2}' | tr -d '\r'`"
  echo "Detected CGE version ${CGE_VERSION}"
}

# Call lazbuild $@.
# If it fails, try again.
#
# Workarounds lazbuild crashes with Lazarus 1.8,
# at least for Win32/i386 (when using cross-compiler from Linux/x86_64):
#   $0000000000563D4A line 1220 of ideexterntoolintf.pas
#   $00000000005AB34E line 590 of exttools.pas
#   $00000000005B0061 line 1525 of exttools.pas
#   $00000000005B15DE line 1814 of exttools.pas
lazbuild_twice ()
{
  if ! lazbuild "$@"; then
    echo 'lazbuild failed, trying again'
    lazbuild "$@"
  fi
}

# Download URL $1 into filename $2.
download ()
{
  # Both wget and curl should work OK.
  # But on my Cygwin (possibly some problem specific on Michalis Windows machine), wget fails with "GnuTLS: The request is invalid."
  if which cygpath.exe > /dev/null; then
    curl "$1" > "$2"
  else
    wget "$1" --output-document "$2"
  fi
}

# Download another repository from GitHub, compile with current build tool,
# move result to $3 .
# Assumes $CASTLE_BUILD_TOOL_OPTIONS defined.
# Changes current dir.
add_external_tool ()
{
  local GITHUB_NAME="$1"
  local EXE_NAME="$2"
  local OUTPUT_BIN="$3"
  shift 2

  local TEMP_PATH_TOOL="/tmp/castle-engine-release-$$/${GITHUB_NAME}/"
  mkdir -p "${TEMP_PATH_TOOL}"
  cd "${TEMP_PATH_TOOL}"
  download https://codeload.github.com/castle-engine/"${GITHUB_NAME}"/zip/master "${GITHUB_NAME}".zip
  unzip "${GITHUB_NAME}".zip
  cd "${GITHUB_NAME}"-master

  # special exceptional addition for pascal-language-server, that has jsonstream as a submodule
  if [ "${GITHUB_NAME}" = 'pascal-language-server' ]; then
    download https://codeload.github.com/Isopod/jsonstream/zip/master jsonstream.zip
    unzip jsonstream.zip
    rm -Rf server/deps/jsonstream # zip contains empty dir with it
    mv jsonstream-master server/deps/jsonstream
    lazbuild_twice $CASTLE_LAZBUILD_OPTIONS server/deps/jsonstream/pascal/package/jsonstreampkg.lpk
  fi

  if [ '(' "$OS" '=' 'darwin' ')' -a '(' "${GITHUB_NAME}" != 'pascal-language-server' ')' ]; then
    # on macOS, build app bundle, and move it to output path
    castle-engine $CASTLE_BUILD_TOOL_OPTIONS package --package-format=mac-app-bundle
    mv "${EXE_NAME}".app "${OUTPUT_BIN}"
  else
    castle-engine $CASTLE_BUILD_TOOL_OPTIONS compile
    mv "${EXE_NAME}" "${OUTPUT_BIN}"
  fi
}

# Prepare directory with precompiled CGE.
#
# Parameters:
# - $1: OS
# - $2: CPU
#
# Output:
# - $TEMP_PATH: absolute directory that contains castle_game_engine subdir
#   (guaranteed to end with path delimiter,
#   i.e. slash on Unix or backslash on Windows).
# - $ARCHIVE_NAME_BUNDLE: empty string or '-bundle',
#   depending on whether CGE_PACK_BUNDLE was defined.
pack_platform_dir ()
{
  OS="$1"
  CPU="$2"
  shift 2

  # comparisons in this script assume lowercase OS name, like darwin or win32
  OS=`echo -n $OS | tr '[:upper:]' '[:lower:]'`

  # restore CGE path, otherwise it points to a temporary (and no longer existing)
  # dir after one execution of do_pack_platform
  export CASTLE_ENGINE_PATH="${ORIGINAL_CASTLE_ENGINE_PATH}"

  case "$OS" in
    win32|win64) local EXE_EXTENSION='.exe' ;;
    *)           local EXE_EXTENSION=''     ;;
  esac

  # Pass options to compile indicating target OS/CPU for everything
  export CASTLE_FPC_OPTIONS="-T${OS} -P${CPU}"
  export CASTLE_BUILD_TOOL_OPTIONS="--os=${OS} --cpu=${CPU}"
  local  CASTLE_LAZBUILD_OPTIONS="--os=${OS} --cpu=${CPU}"
  # Note: always use it like ${MAKE_OPTIONS}, without double quotes,
  # to *allow* treating spaces inside as argument sepaators.
  # Otherwise we'd get errors that castle-engine doesn't support --quiet.
  local  MAKE_OPTIONS="BUILD_TOOL=castle-engine" # use build tool on $PATH

  if [ "${VERBOSE}" '!=' 'true' ]; then
    CASTLE_FPC_OPTIONS="${CASTLE_FPC_OPTIONS} -vi-"
    CASTLE_BUILD_TOOL_OPTIONS="${CASTLE_BUILD_TOOL_OPTIONS} --compiler-option=-vi-"
    CASTLE_LAZBUILD_OPTIONS="${CASTLE_LAZBUILD_OPTIONS} -q"
    MAKE_OPTIONS="${MAKE_OPTIONS} --quiet"
  fi

  # Create temporary CGE copy, for packing
  TEMP_PATH="/tmp/castle-engine-release-$$/"
  if which cygpath.exe > /dev/null; then
    # must be native (i.e. cannot be Unix path on Cygwin) as this path
    # (or paths derived from it) is used by CGE native tools
    # e.g. for compiling fpc-cge.
    TEMP_PATH="`cygpath --mixed \"${TEMP_PATH}\"`"
  fi
  mkdir -p "$TEMP_PATH"
  local TEMP_PATH_CGE="${TEMP_PATH}castle_game_engine/"
  cp -R "${CASTLE_ENGINE_PATH}" "${TEMP_PATH_CGE}"

  cd "${TEMP_PATH_CGE}"

  # Initial cleanups after "cp -R ...".
  # .cache and .cge-jenkins-lazarus are created in Jenkins + Docker job, where $HOME is equal to CGE dir.
  rm -Rf .git .svn .cache .cge-jenkins-lazarus

  # Extend castleversion.inc with GIT hash
  # (useful to have exact version in case of snapshots).
  # $GIT_COMMIT is defined by Jenkins, see https://wiki.jenkins.io/display/JENKINS/Building+a+software+project#Buildingasoftwareproject-belowJenkinsSetEnvironmentVariables
  if [ -n "${GIT_COMMIT:-}" ]; then
    echo "+ ' (commit ${GIT_COMMIT})'" >> src/base/castleversion.inc
  fi

  # update environment to use CGE in temporary location
  export CASTLE_ENGINE_PATH="${TEMP_PATH_CGE}"

  lazbuild_twice $CASTLE_LAZBUILD_OPTIONS src/vampyre_imaginglib/src/Packages/VampyreImagingPackage.lpk
  lazbuild_twice $CASTLE_LAZBUILD_OPTIONS src/vampyre_imaginglib/src/Packages/VampyreImagingPackageExt.lpk
  lazbuild_twice $CASTLE_LAZBUILD_OPTIONS packages/castle_base.lpk
  lazbuild_twice $CASTLE_LAZBUILD_OPTIONS packages/castle_window.lpk
  lazbuild_twice $CASTLE_LAZBUILD_OPTIONS packages/castle_components.lpk
  lazbuild_twice $CASTLE_LAZBUILD_OPTIONS packages/castle_editor_components.lpk

  # Make sure no leftovers from previous compilations remain, to not affect tools, to not pack them in release
  "${MAKE}" cleanmore ${MAKE_OPTIONS}

  # Cleanup .exe more brutally.
  # TODO: This should not be needed "castle-engine clean" done by "make clean"
  # should clean all relevant exes, cross-platform.
  # This is just a temporary workaround of the fact that our Delphi projects right now
  # sometimes leave artifacts -- xxx_standalone.exe, base_tests/Win32/Debug/xxx.exe .
  "${FIND}" examples/ -iname '*.exe' -execdir rm -f '{}' ';'

  # Remove Vampyre Demos - take up 60 MB space, and are not necessary for users of CGE.
  rm -Rf src/vampyre_imaginglib/src/Demos/

  # Compile tools (except editor) with just FPC
  "${MAKE}" tools ${MAKE_OPTIONS} BUILD_TOOL="castle-engine ${CASTLE_BUILD_TOOL_OPTIONS}"

  # Compile fpc-cge internal tool
  castle-engine $CASTLE_BUILD_TOOL_OPTIONS --project "${TEMP_PATH_CGE}"tools/internal/fpc-cge/ compile

  # Place tools (except editor) binaries in bin-to-keep subdirectory
  mkdir -p "${TEMP_PATH_CGE}"bin-to-keep
  cp tools/build-tool/castle-engine"${EXE_EXTENSION}" \
     tools/texture-font-to-pascal/texture-font-to-pascal"${EXE_EXTENSION}" \
     tools/image-to-pascal/image-to-pascal"${EXE_EXTENSION}" \
     tools/castle-curves/castle-curves"${EXE_EXTENSION}" \
     tools/to-data-uri/to-data-uri"${EXE_EXTENSION}" \
     tools/internal/fpc-cge/fpc-cge"${EXE_EXTENSION}" \
     "${TEMP_PATH_CGE}"bin-to-keep

  # Compile castle-editor with lazbuild (or CGE build tool, to get macOS app bundle),
  # place it in bin-to-keep subdirectory
  if [ "$OS" '=' 'darwin' ]; then
    cd tools/castle-editor/
    ../build-tool/castle-engine"${EXE_EXTENSION}" $CASTLE_BUILD_TOOL_OPTIONS package --package-format=mac-app-bundle
    cd ../../
    cp -R tools/castle-editor/castle-editor.app \
       "${TEMP_PATH_CGE}"bin-to-keep
  else
    lazbuild_twice $CASTLE_LAZBUILD_OPTIONS tools/castle-editor/castle_editor.lpi
    cp tools/castle-editor/castle-editor"${EXE_EXTENSION}" \
       "${TEMP_PATH_CGE}"bin-to-keep
  fi

  # Add DLLs on Windows
  case "$OS" in
    win32|win64)
      cp "${CASTLE_ENGINE_PATH}"/tools/build-tool/data/external_libraries/"${CPU}"-"${OS}"/*.dll \
         "${CASTLE_ENGINE_PATH}"/tools/build-tool/data/external_libraries/"${CPU}"-"${OS}"/openssl/*.dll \
         "${TEMP_PATH_CGE}"bin-to-keep
      ;;
  esac

  # Make sure no leftovers from tools compilation remain
  "${MAKE}" cleanmore ${MAKE_OPTIONS}

  # After make clean, make sure bin/ exists and is filled with what we need
  mv "${TEMP_PATH_CGE}"bin-to-keep "${TEMP_PATH_CGE}"bin

  # Add PasDoc docs
  "${MAKE}" -C doc/pasdoc/ clean html ${MAKE_OPTIONS}
  # Remove pasdoc leftovers,
  # including pasdoc dir and zip/tar.gz left after tasks like '(Windows) Get PasDoc' and '(macOS) Get PasDoc'.
  # Otherwise they'd get packaged.
  rm -Rf doc/pasdoc/cache/ pasdoc/ pasdoc-*.zip pasdoc-*.tar.gz

  # Add tools
  add_external_tool view3dscene view3dscene"${EXE_EXTENSION}" "${TEMP_PATH_CGE}"bin
  add_external_tool castle-view-image castle-view-image"${EXE_EXTENSION}" "${TEMP_PATH_CGE}"bin
  add_external_tool pascal-language-server server/pasls"${EXE_EXTENSION}" "${TEMP_PATH_CGE}"bin

  # Add bundled tools (FPC)
  ARCHIVE_NAME_BUNDLE=''
  if [ "${CGE_PACK_BUNDLE:-}" == 'yes' ]; then
    cd "${TEMP_PATH_CGE}"tools/contrib/
    unzip "${ORIGINAL_CASTLE_ENGINE_PATH}/fpc-${OS}-${CPU}.zip"
    ARCHIVE_NAME_BUNDLE='-bundle'
    mv "${TEMP_PATH_CGE}"bin/fpc-cge"${EXE_EXTENSION}" "${TEMP_PATH_CGE}"tools/contrib/fpc/bin
  else
    # remove useless fpc-cge in this case
    rm -f "${TEMP_PATH_CGE}"tools/contrib/fpc/bin/fpc-cge"${EXE_EXTENSION}"
  fi
}

# Prepare zip with precompiled CGE.
# zip is placed in OUTPUT_DIRECTORY.
# Parameters:
# - $1: OS
# - $2: CPU
pack_platform_zip ()
{
  OS="$1"
  CPU="$2"
  shift 2

  pack_platform_dir "${OS}" "${CPU}"

  local ARCHIVE_NAME="castle-engine-${CGE_VERSION}-${OS}-${CPU}${ARCHIVE_NAME_BUNDLE}.zip"

  cd "${TEMP_PATH}"
  rm -f "${ARCHIVE_NAME}"
  zip -r "${ARCHIVE_NAME}" castle_game_engine/
  mv -f "${ARCHIVE_NAME}" "${OUTPUT_DIRECTORY}"
  rm -Rf "${TEMP_PATH}"
}

# Prepare Windows installer with precompiled CGE.
# Requires InnoSetup installed.
# Result is placed in OUTPUT_DIRECTORY.
pack_windows_installer ()
{
  OS="win64"
  CPU="x86_64"

  pack_platform_dir "${OS}" "${CPU}"

  local ARCHIVE_NAME="castle-engine-setup-${CGE_VERSION}"

  # Detect iscc location
  INNO_SETUP_CLI='iscc'
  if ! which "${INNO_SETUP_CLI}" > /dev/null; then
    # if not on $PATH, try default location
    INNO_SETUP_CLI='c:/Program Files (x86)/Inno Setup 6/iscc.exe'
  fi

  # See https://jrsoftware.org/ishelp/index.php?topic=compilercmdline
  # and https://jrsoftware.org/ispphelp/index.php?topic=isppcc (for preprocessor additional options).
  "${INNO_SETUP_CLI}" \
    "${ORIGINAL_CASTLE_ENGINE_PATH}/tools/internal/pack_release/cge-windows-setup.iss" \
    "/O${OUTPUT_DIRECTORY}" \
    "/F${ARCHIVE_NAME}" \
    "/DMyAppSrcDir=${TEMP_PATH}castle_game_engine" \
    "/DMyAppVersion=${CGE_VERSION}"

  # cleanup to save disk space
  rm -Rf "${TEMP_PATH}"
}

# ----------------------------------------------------------------------------
# Main body

detect_platform
check_fpc_version
prepare_build_tool
calculate_cge_version
if [ -n "${1:-}" ]; then
  if [ "$1" '=' 'windows_installer' ]; then
    pack_windows_installer
  else
    pack_platform_zip "${1}" "${2}"
  fi
else
  # build for default platforms (expected by Jenkinsfile)
  pack_platform_zip win64 x86_64
  pack_platform_zip win32 i386
  pack_platform_zip linux x86_64
fi
