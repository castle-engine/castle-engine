#!/bin/bash
set -e

CWD=$(pwd)
cd ../..

echo "set -e" >ppccmd.sh
COMPILE_SIM=1
COMPILE_ARM=1
EXECUTABLE_NAME="cge_library.a"
FPC_MAIN_FILE="src/library/castleengine.lpr"
#FPC_SIM_COMPILER="fpc -Pi386 -V3.0.1"  #this selects x64 - probably bug in FPC setup script
FPC_SIM_COMPILER="/usr/local/lib/fpc/3.0.1/ppc386"
FPC_SIM64_COMPILER="fpc -Px86_64 -V3.0.1"
FPC_ARM_COMPILER="fpc -Parm"
FPC_ARM64_COMPILER="fpc -Paarch64"
FPC_COMMON="-Cn -WP5.1 ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg -dCASTLE_WINDOW_LIBRARY"
OBJECT_FILE_DIR_normal="src/library"
PROJECT_DIR="."
OUTPUT_SIM="$OBJECT_FILE_DIR_normal/out-iphonesim"
OUTPUT_ARM="$OBJECT_FILE_DIR_normal/out-armv7"
OUTPUT_SIM64="$OBJECT_FILE_DIR_normal/out-iphonesim64"
OUTPUT_ARM64="$OBJECT_FILE_DIR_normal/out-arm64"
#debug
FPC_CONFIG="-CirotR -gltw -Sa"
#release
#FPC_CONFIG="-O2"
#FPC_CONFIG="-gw"

SIM_LIB=$OUTPUT_SIM/$EXECUTABLE_NAME
ARM_LIB=$OUTPUT_ARM/$EXECUTABLE_NAME
SIM64_LIB=$OUTPUT_SIM64/$EXECUTABLE_NAME
ARM64_LIB=$OUTPUT_ARM64/$EXECUTABLE_NAME

# compile for simulator
if [ $COMPILE_SIM -eq 1 ]
then
  if [ ! -d "$OUTPUT_SIM" ]; then
    mkdir $OUTPUT_SIM
  fi
  OUT_FILES="-FU'$OUTPUT_SIM' -o'$SIM_LIB'"
  CMD_LINE="$FPC_SIM_COMPILER -Tiphonesim $FPC_COMMON $FPC_CONFIG $OUT_FILES '$FPC_MAIN_FILE'"
  echo "echo " $CMD_LINE >>ppccmd.sh
  echo $CMD_LINE >>ppccmd.sh
  echo "grep '\.o$' $OUTPUT_SIM/link.res > filelist.tmp" >>ppccmd.sh
  CMD_LINE="libtool -static -o '$SIM_LIB' -filelist filelist.tmp"
  echo $CMD_LINE >>ppccmd.sh

# 64 bit
  if [ ! -d "$OUTPUT_SIM64" ]; then
    mkdir $OUTPUT_SIM64
  fi
  OUT_FILES="-FU'$OUTPUT_SIM64' -o'$SIM64_LIB'"
  CMD_LINE="$FPC_SIM64_COMPILER -Tiphonesim $FPC_COMMON $FPC_CONFIG $OUT_FILES '$FPC_MAIN_FILE'"
  echo "echo " $CMD_LINE >>ppccmd.sh
  echo $CMD_LINE >>ppccmd.sh
  echo "grep '\.o$' $OUTPUT_SIM64/link.res > filelist.tmp" >>ppccmd.sh
  CMD_LINE="libtool -static -o '$SIM64_LIB' -filelist filelist.tmp"
  echo $CMD_LINE >>ppccmd.sh
fi

# compile for armv7
if [ $COMPILE_ARM -eq 1 ]
then
  if [ ! -d "$OUTPUT_ARM" ]; then
    mkdir $OUTPUT_ARM
  fi
  OUT_FILES="-FU'$OUTPUT_ARM' -o'$ARM_LIB'"
  CMD_LINE="$FPC_ARM_COMPILER -Cparmv7 -Cfvfpv3 $FPC_COMMON $FPC_CONFIG $OUT_FILES '$FPC_MAIN_FILE'"
  echo "echo " $CMD_LINE >>ppccmd.sh
  echo $CMD_LINE >>ppccmd.sh
  echo "grep '\.o$' '$OUTPUT_ARM/link.res' > filelist.tmp" >>ppccmd.sh
  CMD_LINE="libtool -static -arch_only armv7 -o '$ARM_LIB' -filelist filelist.tmp"
  echo $CMD_LINE >>ppccmd.sh

#64 bit
  if [ ! -d "$OUTPUT_ARM64" ]; then
    mkdir $OUTPUT_ARM64
  fi
  OUT_FILES="-FU'$OUTPUT_ARM64' -o'$ARM64_LIB'"
  CMD_LINE="$FPC_ARM64_COMPILER -dCPUARM64 $FPC_COMMON $FPC_CONFIG $OUT_FILES '$FPC_MAIN_FILE'"
  echo "echo " $CMD_LINE >>ppccmd.sh
  echo $CMD_LINE >>ppccmd.sh
  echo "grep '\.o$' '$OUTPUT_ARM64/link.res' > filelist.tmp" >>ppccmd.sh
  CMD_LINE="libtool -static -arch_only arm64 -o '$ARM64_LIB' -filelist filelist.tmp"
  echo $CMD_LINE >>ppccmd.sh

  if [ $COMPILE_SIM -eq 0 ]
  then
    echo "echo COMBINING TOGEHTER" >>ppccmd.sh
    CMD_LINE="libtool -static '$ARM_LIB' '$ARM64_LIB' -o '$PROJECT_DIR/$EXECUTABLE_NAME'"
    echo $CMD_LINE >>ppccmd.sh
  fi
fi

#combine them together
echo "if [ -e '$SIM_LIB' ] && [ $COMPILE_SIM -eq 1 ]; then" >>ppccmd.sh
 echo "if [ -e '$ARM_LIB' ] && [ $COMPILE_ARM -eq 1 ]; then" >>ppccmd.sh
  echo "if [ -e '$SIM64_LIB' ] && [ $COMPILE_SIM -eq 1 ]; then" >>ppccmd.sh
   echo "if [ -e '$ARM64_LIB' ] && [ $COMPILE_ARM -eq 1 ]; then" >>ppccmd.sh
    echo "echo COMBINING TOGEHTER" >>ppccmd.sh
    CMD_LINE="libtool -static '$ARM_LIB' '$SIM_LIB' '$ARM64_LIB' '$SIM64_LIB' -o '$PROJECT_DIR/$EXECUTABLE_NAME'"
    echo $CMD_LINE >>ppccmd.sh
   echo "fi" >>ppccmd.sh
  echo "fi" >>ppccmd.sh
 echo "fi" >>ppccmd.sh
echo "fi" >>ppccmd.sh
/bin/sh ppccmd.sh
rm ppccmd.sh
rm filelist.tmp
mv $EXECUTABLE_NAME "$CWD"
