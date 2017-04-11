#!/bin/bash
set -e

# Compile CGE library for 4 iOS targets.
# See https://github.com/castle-engine/castle-engine/wiki/iOS

# Configurable variables -----------------------------------------------------

# Whether to compiler a version for iPhoneSimulator
COMPILE_SIM=1

# Whether to compiler a version for a real device (32-bit arm7 or arm64)
COMPILE_ARM=1

# Commands to use to run FPC cross-compilers for the appropriate targets.
# The -V3.0.3 parameters are necessary if you got FPC from the
# fpc-3.0.3.intel-macosx.cross.ios.dmg (official "FPC for iOS" installation).
FPC_SIM_COMPILER="fpc -Pi386 -V3.0.3"
FPC_SIM64_COMPILER="fpc -Px86_64 -V3.0.3"
FPC_ARM_COMPILER="fpc -Parm"
FPC_ARM64_COMPILER="fpc -Paarch64"

# Functions ------------------------------------------------------------------

run_logging ()
{
  echo "Running: " "$@"
  "$@"
}

# Run libtool to create library from .o files referenced in $1/link.res.
# Output library is $2.
# Also update global OUTPUT_LIBRARIES_COUNT and OUTPUT_LIBRARIES vars.
run_libtool ()
{
  local LINK_RES_DIR="$1"
  local OUTPUT_LIB="$2"
  shift 2

  LINK_RES="${LINK_RES_DIR}"/link.res
  grep '\.o$' "${LINK_RES}" > compile_ios_filelist.tmp
  echo libtool -static -o "${OUTPUT_LIB}" -filelist compile_ios_filelist.tmp
  if [ ! -e "${OUTPUT_LIB}" ]; then
    echo "Error: Output ${OUTPUT_LIB} not found"
    exit 1
  fi
  rm -f compile_ios_filelist.tmp

  OUTPUT_LIBRARIES[$OUTPUT_LIBRARIES_COUNT]="${OUTPUT_LIB}"
  OUTPUT_LIBRARIES_COUNT=$(( $OUTPUT_LIBRARIES_COUNT + 1 ))
}

# Main code ------------------------------------------------------------------

# Go to the main CGE directory,
# because the paths inside castle-fpc.cfg are relative to that directory,
# so we have to execute "fpc @castle-fpc.cfg ..." from there.
cd ../..

EXECUTABLE_NAME="cge_library.a"
FPC_MAIN_FILE="src/library/castleengine.lpr"
FPC_COMMON="-Cn -WP5.1 ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg -dCASTLE_WINDOW_LIBRARY"
LIBRARY_PATH="src/library"
PROJECT_DIR="."
OUTPUT_SIM="${LIBRARY_PATH}/out-iphonesim"
OUTPUT_ARM="${LIBRARY_PATH}/out-armv7"
OUTPUT_SIM64="${LIBRARY_PATH}/out-iphonesim64"
OUTPUT_ARM64="${LIBRARY_PATH}/out-arm64"
#debug
FPC_CONFIG="-CirotR -gltw -Sa"
#release
#FPC_CONFIG="-O2"
#FPC_CONFIG="-gw"

SIM_LIB=$OUTPUT_SIM/$EXECUTABLE_NAME
ARM_LIB=$OUTPUT_ARM/$EXECUTABLE_NAME
SIM64_LIB=$OUTPUT_SIM64/$EXECUTABLE_NAME
ARM64_LIB=$OUTPUT_ARM64/$EXECUTABLE_NAME

declare -a OUTPUT_LIBRARIES
OUTPUT_LIBRARIES_COUNT=0

# compile for iPhone simulator
if [ $COMPILE_SIM -eq 1 ]; then
  mkdir -p $OUTPUT_SIM
  OUT_FILES="-FU$OUTPUT_SIM -o$SIM_LIB"
  run_logging $FPC_SIM_COMPILER -Tiphonesim $FPC_COMMON $FPC_CONFIG $OUT_FILES "${FPC_MAIN_FILE}"
  run_libtool "$OUTPUT_SIM" "$SIM_LIB"

  # 64 bit
  mkdir -p $OUTPUT_SIM64
  OUT_FILES="-FU$OUTPUT_SIM64 -o$SIM64_LIB"
  run_logging $FPC_SIM64_COMPILER -Tiphonesim $FPC_COMMON $FPC_CONFIG $OUT_FILES "${FPC_MAIN_FILE}"
  run_libtool "$OUTPUT_SIM64" "$SIM64_LIB"
fi

# compile for a physical device
if [ $COMPILE_ARM -eq 1 ]; then
  mkdir -p $OUTPUT_ARM
  OUT_FILES="-FU$OUTPUT_ARM -o$ARM_LIB"
  run_logging $FPC_ARM_COMPILER -Cparmv7 -Cfvfpv3 $FPC_COMMON $FPC_CONFIG $OUT_FILES "${FPC_MAIN_FILE}"
  run_libtool "$OUTPUT_ARM" "$ARM_LIB"

  # 64 bit
  mkdir -p $OUTPUT_ARM64
  OUT_FILES="-FU$OUTPUT_ARM64 -o$ARM64_LIB"
  run_logging $FPC_ARM64_COMPILER -dCPUARM64 $FPC_COMMON $FPC_CONFIG $OUT_FILES "${FPC_MAIN_FILE}"
  run_libtool "$OUTPUT_ARM64" "$ARM64_LIB"
fi

if [ $OUTPUT_LIBRARIES_COUNT -eq 0 ]; then
  echo 'Nothing generated (you turned off generating output for both iPhoneSimulator and a real device)'
  exit 1
else
  echo "echo COMBINING TOGETHER"
  libtool -static "${OUTPUT_LIBRARIES[@]}" -o "$PROJECT_DIR/$EXECUTABLE_NAME"
  mv $EXECUTABLE_NAME "${LIBRARY_PATH}"
fi
