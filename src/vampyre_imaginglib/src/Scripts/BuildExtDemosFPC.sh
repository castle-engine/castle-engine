#!/bin/bash

echo "Building Extension Demos using Free Pascal"

# Important! Set this dirs on your system 
SDLDIR=""
OPENGLDIR=""

ROOTDIR=".."
DEMOPATH="$ROOTDIR/Demos/ObjectPascal" 
UNITS="-Fu$ROOTDIR/Source -Fu$ROOTDIR/Source/JpegLib -Fu$ROOTDIR/Source/ZLib
  -Fu$DEMOPATH/Common -Fu$ROOTDIR/Source/Extensions -Fu$ROOTDIR/Extras/Extensions -Fu$SDLDIR -Fu$OPENGLDIR"
INCLUDE="-Fi$ROOTDIR/Source -Fi$SDLDir -Fi$OPENGLDIR"
LIBS="-Fl$ROOTDIR/Extras/Extensions/J2KObjects"
OUTPUT="-FE$ROOTDIR/Demos/Bin"
OPTIONS="-Sgi -OG2 -Xs"

fpc $OPTIONS $OUTPUT "$DEMOPATH/SDLDemo/SDLDemo.dpr" $UNITS $INCLUDE $LIBS -oSDLDemo
if test $? = 0; then
fpc $OPTIONS $OUTPUT "$DEMOPATH/OpenGLDemo/OpenGLDemo.dpr" $UNITS $INCLUDE $LIBS -oOpenGLDemo
fi

if test $? = 0; then 
  echo "Extension demos successfuly build in Demos/Bin directory"
else
  echo "Error when building demos!"
fi


