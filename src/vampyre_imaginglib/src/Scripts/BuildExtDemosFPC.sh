#!/bin/bash

# Ext. demos have some build and/or runtime dependencies like SDL or GL.
# libsdl1.2-dev package should take care of both.
echo "Building Extended Demos using Free Pascal"
echo

source ./Common.sh

DEFINES="-dDONT_LINK_EXTRAS"
UNITS="-Fu$ROOTDIR/Source -Fu$ROOTDIR/Source/JpegLib -Fu$ROOTDIR/Source/ZLib -Fu$ROOTDIR/Extensions -Fu$DEMOPATH/Common"  

DEMOCOUNT=2

buildDemo "SDLDemo/SDLDemo.dpr" 
buildDemo "OpenGLDemo/OpenGLDemo.dpr" 

printResult