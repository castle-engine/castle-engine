#!/bin/bash
set -e

# Compile CGE library for 4 iOS targets.
# See https://github.com/castle-engine/castle-engine/wiki/iOS

# Configurable variables -----------------------------------------------------

# Whether to compile a version for iPhoneSimulator
COMPILE_SIM=1

# Whether to compile a version for a real device (32-bit arm7 or arm64)
COMPILE_ARM=1

# Commands to use to run FPC cross-compilers for the appropriate targets.
# The -V3.0.3 parameters are necessary if you got FPC from the
# fpc-3.0.3.intel-macosx.cross.ios.dmg (official "FPC for iOS" installation).
FPC_SIM_COMPILER="fpc -Pi386 -V3.0.3"
FPC_SIM64_COMPILER="fpc -Px86_64 -V3.0.3"
FPC_ARM_COMPILER="fpc -Parm"
FPC_ARM64_COMPILER="fpc -Paarch64"

# debug or release (for the exact meaning, see ../../castle-fpc.cfg)
#FPC_CONFIG="-dDEBUG"
FPC_CONFIG="-dRELEASE"

# You can pass additional FPC parameters as this script parameters
# So you can call, for example, 'compile-iOS.sh -dSOME_SYMBOL'
FPC_CONFIG="${FPC_CONFIG} $@"

# Functions ------------------------------------------------------------------

run_logging ()
{
  echo "----------------------------------------------------------------------"
  echo "Running: " "$@"
  "$@"
}

# Run libtool to create library from .o files referenced in $1/link.res.
# Output library is $1/$EXECUTABLE_NAME
# Also update global OUTPUT_LIBRARIES_COUNT and OUTPUT_LIBRARIES vars.
run_libtool ()
{
  local DIR="$1"
  shift 1

  local LINK_RES="${DIR}/link.res"
  local OUTPUT_LIB="${DIR}/${EXECUTABLE_NAME}"

  grep '\.o$' "${LINK_RES}" > compile_ios_filelist.tmp
  run_logging libtool -static -o "${OUTPUT_LIB}" -filelist compile_ios_filelist.tmp
  if [ ! -e "${OUTPUT_LIB}" ]; then
    echo "Error: Output ${OUTPUT_LIB} not found"
    exit 1
  fi
  rm -f compile_ios_filelist.tmp

  OUTPUT_LIBRARIES[$OUTPUT_LIBRARIES_COUNT]="${OUTPUT_LIB}"
  OUTPUT_LIBRARIES_COUNT=$(( $OUTPUT_LIBRARIES_COUNT + 1 ))
}

# Run fpc and libtool to build library inside dir $1.
# The $2 and following parameters should specify OS/CPU specific
# FPC command and switches.
run_build ()
{
  local OUTPUT_DIR="$1"
  shift 1

  mkdir -p "${OUTPUT_DIR}"
  # Note: $FPC_CONFIG must be specified before $FPC_COMMON,
  # as $FPC_CONFIG contains -dDEBUG / -dRELEASE that determines
  # switches set by $FPC_COMMON inside @castle-fpc.cfg.
  run_logging "$@" $FPC_CONFIG $FPC_COMMON \
    "-FU${OUTPUT_DIR}" "-o${OUTPUT_DIR}/${EXECUTABLE_NAME}" "${FPC_MAIN_FILE}"
  run_libtool "${OUTPUT_DIR}"
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
OUTPUT_SIM="${LIBRARY_PATH}/ios-output/i386-iphonesim"
OUTPUT_SIM64="${LIBRARY_PATH}/ios-output/x86_64-iphonesim"
OUTPUT_ARM="${LIBRARY_PATH}/ios-output/arm-darwin"
OUTPUT_ARM64="${LIBRARY_PATH}/ios-output/aarch64-darwin"
SIMULATOR_SDK='/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk'
DEVICE_SDK='/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk'

declare -a OUTPUT_LIBRARIES
OUTPUT_LIBRARIES_COUNT=0

# compile for iPhone simulator
if [ $COMPILE_SIM -eq 1 ]; then
  run_build "${OUTPUT_SIM}"   $FPC_SIM_COMPILER   -Tiphonesim "-XR${SIMULATOR_SDK}"
  run_build "${OUTPUT_SIM64}" $FPC_SIM64_COMPILER -Tiphonesim "-XR${SIMULATOR_SDK}"
fi

# compile for a physical device
if [ $COMPILE_ARM -eq 1 ]; then
  run_build "${OUTPUT_ARM}"   $FPC_ARM_COMPILER   -Tdarwin "-XR${DEVICE_SDK}" -Cparmv7 -Cfvfpv3
  run_build "${OUTPUT_ARM64}" $FPC_ARM64_COMPILER -Tdarwin "-XR${DEVICE_SDK}"
fi

if [ $OUTPUT_LIBRARIES_COUNT -eq 0 ]; then
  echo 'Nothing generated (you turned off generating output for both iPhoneSimulator and a real device)'
  exit 1
else
  echo "----------------------------------------------------------------------"
  echo "Combining together into a single library:"
  run_logging libtool -static "${OUTPUT_LIBRARIES[@]}" -o "./$EXECUTABLE_NAME"
  mv $EXECUTABLE_NAME "${LIBRARY_PATH}"
  echo "----------------------------------------------------------------------"
  echo "Done, output inside ${LIBRARY_PATH}/${EXECUTABLE_NAME}"
fi
