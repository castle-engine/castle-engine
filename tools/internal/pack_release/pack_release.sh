#!/usr/bin/env bash
set -euxo pipefail

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
# - Linux
# - Windows (with Cygwin, MSYS2, Git Bash...)
#   Note: We assume that tools that understand Unix paths
#   (like "cp" from Cygwin, MSYS2, Git Bash) are first on $PATH,
#   before equivalent tools in FPC (from old MinGW version).
#   A simple solution to make sure it's true is to execute
#     export PATH="/bin:$PATH"
#   in shell right before this script.
# - FreeBSD (install GNU make and GNU sed)
# - macOS (install GNU sed from Homebrew)
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Define global variables

OUTPUT_DIRECTORY=`pwd`
if which cygpath.exe > /dev/null; then
  OUTPUT_DIRECTORY="`cygpath --mixed \"${OUTPUT_DIRECTORY}\"`"
fi

VERBOSE=true

ORIGINAL_CASTLE_ENGINE_PATH="${CASTLE_ENGINE_PATH}"

# Deal with temporary dir -------------------------------------------------

# Calculate temporary directory, where we will put all the temporary files
# during packing.
#
# Notes:
# - We make it unique, using process ID, just in case multiple jobs run in parallel.
# - This cannot be subdirectory of CI workspace (like ${GITHUB_WORKSPACE})
#   as then we'll have "cp -R ..." fail "we cannot copy directory into itself".
#   To clean it up, we use bash trap.
TEMP_PARENT="/tmp/castle-engine-release-$$/"

cleanup_temp ()
{
  echo "Cleaning up temporary dir ${TEMP_PARENT}"
  rm -Rf "${TEMP_PARENT}"
}

trap cleanup_temp EXIT

# ----------------------------------------------------------------------------
# Define functions

# Require building release with latest stable FPC, as supported by CGE,
# see https://castle-engine.io/supported_compilers.php .
check_fpc_version ()
{
  local FPC_VERSION=`fpc -iV | tr -d '\r'`
  echo "FPC version: ${FPC_VERSION}"

  local REQUIRED_FPC_VERSION='3.2.2'

  if [ "${CASTLE_PACK_DISABLE_FPC_VERSION_CHECK:-}" '!=' 'true' ]; then
    if [ "${FPC_VERSION}" '!=' "${REQUIRED_FPC_VERSION}" ]; then
      echo "pack_release: Expected FPC version ${REQUIRED_FPC_VERSION}, but got ${FPC_VERSION}"
      exit 1
    fi
  fi
}

# Require building release with a supported Lazarus (also LCL, lazbuild) version.
# See https://castle-engine.io/supported_compilers.php .
check_lazarus_version ()
{
  # Note that we have to remove lines "using config file", since "lazbuild --version"
  # can answer something like
  #   using config file /Users/jenkins/installed/fpclazarus/fpc322-lazfixes30/lazarus/lazarus.cfg
  #   3.5

  local LAZARUS_VERSION=`lazbuild --version | grep --invert-match 'using config file' | tr -d '\r'`
  echo "Lazarus version: ${LAZARUS_VERSION}"

  # Note that we have to support Lazarus 3.0,
  # since it's the last supported by https://github.com/gcarreno/setup-lazarus for now,
  # see https://github.com/gcarreno/setup-lazarus/issues/30 .
  if [ "${LAZARUS_VERSION}" '!=' '3.0' -a \
       "${LAZARUS_VERSION}" '!=' '3.2' -a \
       "${LAZARUS_VERSION}" '!=' '3.4' -a \
       "${LAZARUS_VERSION}" '!=' '3.5' ]; then
    echo "pack_release: Incorrect Lazarus version to pack release, see ${LAZARUS_VERSION}"
    exit 1
  fi

  # To avoid https://gitlab.com/freepascal.org/lazarus/lazarus/-/merge_requests/291
  # we need Lazarus 3.5 on macOS.
  if [ "`uname -s`" '=' 'Darwin' ]; then
    if [ "${LAZARUS_VERSION}" '!=' '3.5' ]; then
      echo "pack_release: macOS: Incorrect Lazarus version to pack release, see ${LAZARUS_VERSION}"
      exit 1
    fi
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

    # If we're inside Cygwin/MSYS2 (despite the name, cygpath also is in MSYS2,
    # and this is the case on GH hosted runner), then we want to use Cygwin/MSYS2
    # tools. They will call bash properly, and our "make" must be able to call
    # e.g. "tools/build-tool/castle-engine_compile.sh".
    #
    # We don't want to use Embarcadero's make (we need GNU make).
    #
    # we don't want to use FPC make (FPC on Windows is distributed with
    # GNU make 3.8, from MinGW).

    if [ -f /bin/make ]; then
      MAKE='/bin/make'
    else
      if which mingw32-make > /dev/null; then
        MAKE='mingw32-make'
      fi
    fi

    # On Cygwin/MSYS2, make sure to use Cygwin/MSYS2's find, not the one from Windows
    FIND='/bin/find'
  fi

  if [ "`uname -s`" '=' 'FreeBSD' ]; then
    MAKE='gmake'
    SED='gsed'
  fi

  if [ "`uname -s`" '=' 'Darwin' ]; then
    SED='gsed'
    FIND='gfind'
  fi

  # for debugging, output versions of tools
  echo "Using make: ${MAKE}" `${MAKE} --version | head -n 1`
  echo "Using find: ${FIND}" `${FIND} --version | head -n 1`
  echo "Using sed: ${SED}" `${SED} --version | head -n 1`
}

# Compile build tool, put it on $PATH
prepare_build_tool ()
{

  if [ "${VERBOSE}" '!=' 'true' ]; then
    CASTLE_FPC_OPTIONS="-vi-"
  fi

  cd "${CASTLE_ENGINE_PATH}"
  tools/build-tool/castle-engine_compile.sh
  local BIN_TEMP_PATH="${TEMP_PARENT}bin/"
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

  TOOL_BRANCH_NAME='master'
  # Temporary, may be useful again in future: use castle-model-viewer from another branch, to compile with this CGE branch
  # if [ "${GITHUB_NAME}" = 'castle-model-viewer' ]; then
  #   TOOL_BRANCH_NAME='shapes-rendering-2'
  # fi

  local TEMP_PATH_TOOL="${TEMP_PARENT}tool-${GITHUB_NAME}/"
  mkdir -p "${TEMP_PATH_TOOL}"
  cd "${TEMP_PATH_TOOL}"
  download "https://codeload.github.com/castle-engine/${GITHUB_NAME}/zip/${TOOL_BRANCH_NAME}" "${GITHUB_NAME}".zip
  unzip "${GITHUB_NAME}".zip
  cd "${GITHUB_NAME}-${TOOL_BRANCH_NAME}"

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

    if [ "${GITHUB_NAME}" = 'castle-model-viewer' ]; then
      castle-engine $CASTLE_BUILD_TOOL_OPTIONS compile --manifest-name=CastleEngineManifest.converter.xml
      mv castle-model-converter"${EXE_EXTENSION}" "${OUTPUT_BIN}"
    fi
  fi
}

# Followup to "make clean" that cleans even more stuff,
# good to really have 100% clean state for packing.
# Deletes files in current working directory (and doesn't change current working directory).
#
# Note: It doesn't delete bin-to-keep, created and used in this script.
cge_clean_all ()
{
  # Delete
  # - backup files from
  #     Emacs (*~),
  #     Lazarus (backup),
  #     Delphi (*.~???),
  #     Blender (*.blend?),
  #     QtCreator (*.pro.user).
  # - macOS app bundles (made by "make examples-laz", not cleaned up by "make clean").
	"${FIND}" . \
    '(' -type d -name 'bin-to-keep' -prune ')' -or \
    '(' -type f '(' \
          -iname '*~' -or \
          -iname '*.bak' -or \
          -iname '*.~???' -or \
          -iname '*.pro.user' -or \
          -iname '*.blend?' \
        ')' -exec rm -f '{}' ';' ')' -or \
    '(' -type d '(' \
          -iname 'backup' -or \
          -iname '*.app' \
        ')' -exec rm -Rf '{}' ';' -prune ')'

  # Delete pasdoc generated documentation in doc/pasdoc/ and doc/reference/
	"${MAKE}" -C doc/pasdoc/ clean

  # Delete closed-source libs you may have left in tools/build-tool/data
  # (as some past instructions recommended to copy them into CGE tree).
	rm -Rf tools/build-tool/data/android/integrated-services/chartboost/app/libs/*.jar \
	       tools/build-tool/data/android/integrated-services/startapp/app/libs/*.jar \
	       tools/build-tool/data/ios/services/game_analytics/cge_project_name/game_analytics/GameAnalytics.h \
	       tools/build-tool/data/ios/services/game_analytics/cge_project_name/game_analytics/libGameAnalytics.a

  # Delete previous pack_release.sh leftovers
	rm -f castle-engine*.zip tools/internal/pack_release/castle-engine*.zip

  # Delete bundled FPC leftovers
	rm -Rf fpc-*.zip tools/contrib/fpc/

  # Cleanup .exe more brutally.
  # TODO: This should not be needed "castle-engine clean" done by "make clean"
  # should clean all relevant exes, cross-platform.
  # This is just a temporary workaround of the fact that our Delphi projects right now
  # sometimes leave artifacts -- xxx_standalone.exe, base_tests/Win32/Debug/xxx.exe .
  "${FIND}" examples/ -iname '*.exe' -execdir rm -f '{}' ';'

  # Delete installed subdir (in case you did
  # "make install PREFIX=${CASTLE_ENGINE_PATH}/installed/", as some CI jobs do)
  rm -Rf installed/

  # Remove Vampyre Demos - take up 60 MB space, and are not necessary for users of CGE.
  rm -Rf src/vampyre_imaginglib/src/Demos/

  # Made by "make examples-laz", not cleaned up by "make clean".
  rm -f examples/audio/test_sound_source_allocator/mainf.lrs \
        examples/lazarus/model_3d_with_2d_controls/model_3d_with_2d_controls.obj
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
  TEMP_PATH="${TEMP_PARENT}release/"
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

  lazbuild_twice $CASTLE_LAZBUILD_OPTIONS packages/castle_base.lpk
  lazbuild_twice $CASTLE_LAZBUILD_OPTIONS packages/castle_window.lpk
  lazbuild_twice $CASTLE_LAZBUILD_OPTIONS packages/castle_components.lpk
  lazbuild_twice $CASTLE_LAZBUILD_OPTIONS packages/castle_editor_components.lpk

  # Make sure no leftovers from previous compilations remain,
  # to not pack them in release.
  "${MAKE}" clean ${MAKE_OPTIONS}
  cge_clean_all

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

  # On Linux compile also Qt5 editor version.
  #
  # But do not do this on Raspberry Pi (Arm), as it fails at linking
  # (too old libqt5-pas-dev on Raspbian, it seems)
  # with
  # /usr/bin/ld: /home/jenkins/.lazarus/lib/LazOpenGLContext/lib/arm-linux/qt5/qlclopenglwidget.o: in function `TQTOPENGLWIDGET__EVENTFILTER':
  # /usr/local/fpc_lazarus/fpc3.2.2-lazarus2.2.2/src/lazarus/2.2.2/components/opengl/qlclopenglwidget.pas:106: undefined reference to `QLCLOpenGLWidget_Create'
  # /usr/bin/ld: /usr/local/fpc_lazarus/fpc3.2.2-lazarus2.2.2/src/lazarus/2.2.2/components/opengl/qlclopenglwidget.pas:104: undefined reference to `QLCLOpenGLWidget_InheritedPaintGL'
  # /usr/bin/ld: /usr/local/fpc_lazarus/fpc3.2.2-lazarus2.2.2/src/lazarus/2.2.2/components/opengl/qlclopenglwidget.pas:105: undefined reference to `QLCLOpenGLWidget_override_paintGL'
  # /usr/bin/ld: /usr/local/fpc_lazarus/fpc3.2.2-lazarus2.2.2/src/lazarus/2.2.2/components/opengl/qlclopenglwidget.pas:106: undefined reference to `QLCLOpenGLWidget_override_paintGL'
  #
  # Same with Lazarus 3.0.0 on Raspberry Pi 64-bit,
  # /usr/bin/ld: /home/michalis/installed/fpc_lazarus/fpc3.2.3-lazarus3.0.0/src/lazarus/3.0.0/lcl/units/aarch64-linux/qt5/qtint.o: in function `CREATE':
  # /home/michalis/installed/fpc_lazarus/fpc3.2.3-lazarus3.0.0/src/lazarus/3.0.0/lcl/interfaces//qt5/qtobject.inc:44: undefined reference to `QGuiApplication_setFallbackSessionManagementEnabled'
  # /usr/bin/ld: /home/michalis/installed/fpc_lazarus/fpc3.2.3-lazarus3.0.0/src/lazarus/3.0.0/lcl/units/aarch64-linux/qt5/qtobjects.o: in function `ENDX11SELECTIONLOCK':
  # /home/michalis/installed/fpc_lazarus/fpc3.2.3-lazarus3.0.0/src/lazarus/3.0.0/lcl/interfaces//qt5/qtobjects.pas:3873: undefined reference to `QTimer_singleShot3'
  #
  # 2024-03-27: Disable building castle-editor-qt5, because Lazarus 3.2 is not compatible
  # with libqt5pas in Debian.
  # We could update libqt5pas using https://github.com/davidbannon/libqt5pas/releases ,
  # but this is pointless if it will fail for users anyway.
  # See also
  # https://github.com/gcarreno/setup-lazarus?tab=readme-ov-file
  # https://forum.lazarus.freepascal.org/index.php/topic,65619.msg500216.html#msg500216
  #
  # if [ "$OS" '=' 'linux' -a "${CPU}" '!=' 'arm' -a "${CPU}" '!=' 'aarch64' ]; then
  #   lazbuild_twice $CASTLE_LAZBUILD_OPTIONS tools/castle-editor/castle_editor.lpi --widgetset=qt5
  #   cp tools/castle-editor/castle-editor"${EXE_EXTENSION}" \
  #      "${TEMP_PATH_CGE}"bin-to-keep/castle-editor-qt5
  # fi

  # Add DLLs on Windows
  case "$OS" in
    win32|win64)
      cp "${CASTLE_ENGINE_PATH}"/tools/build-tool/data/external_libraries/"${CPU}"-"${OS}"/*.dll \
         "${CASTLE_ENGINE_PATH}"/tools/build-tool/data/external_libraries/"${CPU}"-"${OS}"/openssl/*.dll \
         "${TEMP_PATH_CGE}"bin-to-keep
      ;;
  esac

  # Make sure no leftovers from tools compilation remain
  "${MAKE}" clean ${MAKE_OPTIONS}
  cge_clean_all

  # After make clean, make sure bin/ exists and is filled with what we need
  mv "${TEMP_PATH_CGE}"bin-to-keep "${TEMP_PATH_CGE}"bin

  if [ "$OS" '=' 'darwin' ]; then
    if [ ! -d "${TEMP_PATH_CGE}"bin/castle-editor.app ]; then
      echo "Error: castle-editor.app not found in bin/ at packaging macOS release"
      exit 1
    fi
  fi

  # Add PasDoc docs
  "${MAKE}" -C doc/pasdoc/ clean html ${MAKE_OPTIONS}
  # Remove pasdoc leftovers,
  # including pasdoc dir and zip/tar.gz left after tasks like '(Windows) Get PasDoc' and '(macOS) Get PasDoc'.
  # Otherwise they'd get packaged.
  rm -Rf doc/pasdoc/cache/ pasdoc/ pasdoc-*.zip pasdoc-*.tar.gz

  # Add tools
  add_external_tool castle-model-viewer castle-model-viewer"${EXE_EXTENSION}" "${TEMP_PATH_CGE}"bin
  add_external_tool castle-image-viewer castle-image-viewer"${EXE_EXTENSION}" "${TEMP_PATH_CGE}"bin
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
  # seems to sometimes fail with "rm: fts_read failed: No such file or directory" on GH hosted windows runner
  set +e
  rm -Rf "${TEMP_PATH}"
  set -e
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
check_lazarus_version
prepare_build_tool
calculate_cge_version
if [ -n "${1:-}" ]; then
  if [ "$1" '=' 'windows_installer' ]; then
    pack_windows_installer
  else
    pack_platform_zip "${1}" "${2}"
  fi
else
  echo 'pack_release: Requires 2 arguments, OS and CPU, or 1 argument "windows_installer"'
  exit 1
fi
