First example of using our engine (2D controls and 3D rendering,
using full-featured SceneManager (TCastleScene underneath)).
This compiles to a library libcge_android_lib.so.

If you want to take a look at the Android integration source code,
most important part of it is now inside
castle_game_engine/src/window/castlewindow_android.inc .
Using Android API inside castle_game_engine/src/base/android/ .

After compiling, copy also the data/* files to /sdcard/castle_game_engine/cge_android_lib/,
e.g. using
  adb shell mkdir /sdcard/castle_game_engine/cge_android_lib/
  adb push data/ /sdcard/castle_game_engine/cge_android_lib/
(TODO: The need for this will disappear once we implement getting
data using Android asset manager, then the data will be just packed
inside .apk.)

Then copy the libcge_android_lib.so to the android/libs/
(just like ndk-build does for libraries written in C for Android),
and compile and run the project inside android/
using normal Android SDK tools. It's easiest to do it just by
  cd android/
  make
