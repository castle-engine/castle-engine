First example of using our engine (2D controls and 3D rendering,
using full-featured SceneManager (TCastleScene underneath)).
This compiles to a library libcge_android_lib.so.

After compiling, copy also the data/* files to /sdcard/kambitest/,
e.g. using
  adb push data/ /sdcard/kambitest
(TODO: The need for this will disappear once we implement getting
data using Android asset manager, then the data will be just packed
inside .apk.)

Then copy the libcge_android_lib.so to the android/libs/
(just like ndk-build does for libraries written in C for Android),
and compile and run the project inside android/
using normal Android SDK tools. It's easiest to do it just by
  cd android/
  make
