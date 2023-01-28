#!/bin/bash

# FPC build scripts also work in Windows in WSL, Cygwin, MSYS, etc.

# FPC compiler command
FPC=fpc
# You can override default FPC target (to cross-compile etc.)
TARGET=#-Ttarget
# Uncomment if you run this shell script in WSL and want to run Windows-side FPC instead of Linux FPC
#FPC=fpc.exe

set -e
# Make sure there are no CR characters (by fpc.exe) otherwise variable concatenation of FPCTARGET would fail
FPCCPU=$($FPC -iTP | tr -d '\r')
FPCOS=$($FPC -iTO | tr -d '\r')
FPCTARGET=$FPCCPU-$FPCOS

ROOTDIR=".."
DEMOPATH="$ROOTDIR/Demos/ObjectPascal" 
BINPATH="$ROOTDIR/Demos/Bin"
UNITPATH="$ROOTDIR/Demos/Bin/Dcu/$FPCTARGET"
# FPC does not like creating any new directories passed by -FE -FU
mkdir -p $UNITPATH
mkdir -p $BINPATH
set +e

OUTPUT="-FE$BINPATH -FU$UNITPATH"
# This is how you suppress -vn set in fpc.cfg
OPTIONS="-B -O3 -Xs -vn-"
INCLUDE="-Fi$ROOTDIR/Source"
LIBS="-Fl$ROOTDIR/Extensions/J2KObjects -Fl$ROOTDIR/Extensions/LibTiff/Compiled"  

DEMOSBUILD=0
DEMOCOUNT=0

function buildDemo {
  $FPC $OPTIONS $OUTPUT $TARGET $DEFINES $UNITS $INCLUDE $LIBS $DEMOPATH/$1
  if [ $? = 0 ]; then 
    ((DEMOSBUILD++))
  fi   
  echo
} 

function printResult {
  SWITCH="\033["
  NORMAL="${SWITCH}0m"
  RED="${SWITCH}0;31m"
  GREEN="${SWITCH}0;32m"

  if [ $DEMOSBUILD = $DEMOCOUNT ]; then
    echo -e "${GREEN}Build Successful - all $DEMOSBUILD of $DEMOCOUNT in Demos/Bin directory${NORMAL}"
  else
    echo -e "${RED}Errors during building - only $DEMOSBUILD of $DEMOCOUNT demos build${NORMAL}"
  fi
}
