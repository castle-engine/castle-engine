#!/bin/bash
set -e
cd ../..

echo "set -e" >ppccmd.sh
COMPILE_SIM=0
COMPILE_ARM=1
EXECUTABLE_NAME="cge_library.a"
FPC_MAIN_FILE="src/library/castleengine.lpr"
FPC_SIM_COMPILER="/usr/local/lib/fpc/2.6.2/ppc386"
FPC_ARM_COMPILER="/usr/local/lib/fpc/2.6.2/ppcarm"
FPC_COMMON="-Cn ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg"
OBJECT_FILE_DIR_normal="src/library"
PROJECT_DIR="."
OUTPUT_SIM="$OBJECT_FILE_DIR_normal/out-iphonesim"
OUTPUT_ARM="$OBJECT_FILE_DIR_normal/out-armv7"
#debug
#FPC_CONFIG="-CirotR -gltw -Sa"
#release
FPC_CONFIG="-gw"

# compile for simulator
if [ $COMPILE_SIM -eq 1 ]
then
  if [ ! -d "$OUTPUT_SIM" ]; then
    mkdir $OUTPUT_SIM
  fi
  OUT_FILES="-FU'$OUTPUT_SIM' -o'$OUTPUT_SIM/$EXECUTABLE_NAME'"
  CMD_LINE="$FPC_SIM_COMPILER -Tiphonesim $FPC_COMMON $FPC_CONFIG $OUT_FILES '$FPC_MAIN_FILE'"
  echo "echo " $CMD_LINE >>ppccmd.sh
  echo $CMD_LINE >>ppccmd.sh
  echo "grep '\.o$' $OUTPUT_SIM/link.res > filelist.tmp" >>ppccmd.sh
  CMD_LINE="libtool -static -o '$OUTPUT_SIM/$EXECUTABLE_NAME' -filelist filelist.tmp"
  echo $CMD_LINE >>ppccmd.sh
fi
# compile for armv7
if [ $COMPILE_ARM -eq 1 ]
then
  if [ ! -d "$OUTPUT_ARM" ]; then
    mkdir $OUTPUT_ARM
  fi
  OUT_FILES="-FU'$OUTPUT_ARM' -o'$OUTPUT_ARM/$EXECUTABLE_NAME'"
  CMD_LINE="$FPC_ARM_COMPILER -Cparmv7 -Cfvfpv3 $FPC_COMMON $FPC_CONFIG $FPC_CONFIG $OUT_FILES '$FPC_MAIN_FILE'"
  echo "echo " $CMD_LINE >>ppccmd.sh
  echo $CMD_LINE >>ppccmd.sh
  echo "grep '\.o$' '$OUTPUT_ARM/link.res' > filelist.tmp" >>ppccmd.sh
  CMD_LINE="libtool -static -arch_only armv7 -o '$OUTPUT_ARM/$EXECUTABLE_NAME' -filelist filelist.tmp"
  echo $CMD_LINE >>ppccmd.sh
  if [ $COMPILE_SIM -eq 0 ]
  then
    CMD_LINE="cp '$OUTPUT_ARM/$EXECUTABLE_NAME' '$PROJECT_DIR'"
    echo $CMD_LINE >>ppccmd.sh
  fi
fi
#combine them together
SIM_LIB=$OUTPUT_SIM/$EXECUTABLE_NAME
ARM_LIB=$OUTPUT_ARM/$EXECUTABLE_NAME
echo "if [ -e '$SIM_LIB' ]; then" >>ppccmd.sh
  echo "if [ -e '$ARM_LIB' ]; then" >>ppccmd.sh
    echo "echo COMBINING TOGEHTER" >>ppccmd.sh
    CMD_LINE="libtool -static '$ARM_LIB' '$SIM_LIB' -o '$PROJECT_DIR/$EXECUTABLE_NAME'"
    echo $CMD_LINE >>ppccmd.sh
 echo "fi" >>ppccmd.sh
echo "fi" >>ppccmd.sh
/bin/sh ppccmd.sh
rm ppccmd.sh
rm filelist.tmp
mv $EXECUTABLE_NAME src/library
