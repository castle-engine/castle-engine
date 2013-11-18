First example of using our engine (2D controls and 3D rendering,
using full-featured SceneManager (TCastleScene underneath)).
This compiles to a library libcge_android_lib.so.

After compiling, copy also the data/* files to /sdcard/kambitest/,
e.g. using
  adb push data/ /sdcard/kambitest

Then copy the libcge_android_lib.so to the android_project/libs/
(just like ndk-build does for libraries written in C for Android),
and compile and run the project using normal Android SDK tools.
It's easiest to do it just by
  cd android_project/
  make

Right now, android_project/ contains a Java wrapper (from hello-gl2
from NDK samples), that already initializes OpenGL context for us.
TODO:
A version that allows us to initialize context (using EGL) will follow soon :),
and also our own Java wrapper, possibly even just use Android's
NativeActivity without any custom Java code.
