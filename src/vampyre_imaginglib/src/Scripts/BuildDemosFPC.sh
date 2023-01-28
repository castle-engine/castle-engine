#!/bin/bash

echo "Building Demos using Free Pascal"
echo

source ./Common.sh

DEFINES="-dFULL_FEATURE_SET"
UNITS="-Fu$ROOTDIR/Source -Fu$ROOTDIR/Source/JpegLib -Fu$ROOTDIR/Source/ZLib
  -Fu$ROOTDIR/Extensions -Fu$ROOTDIR/Extensions/LibTiff -Fu$DEMOPATH/Common"

DEMOCOUNT=2

buildDemo "Benchmark/Bench.dpr" 
buildDemo "VampConvert/VampConvert.dpr" 

printResult