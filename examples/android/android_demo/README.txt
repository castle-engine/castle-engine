First example of using our engine on Android (2D controls and 3D rendering
using full-featured SceneManager (TCastleScene underneath)).
This compiles to a library libcge_android_lib.so.

After compiling the library,
- copy the libcge_android_lib.so to the android/libs/
  (just like ndk-build does for libraries written in C for Android),
- compile the project inside android/ using normal Android SDK tools (like ant),
- install and run the resulting apk using normal Android SDK tools.

It's easiest to do everything by
  cd android/
  make

You have to also copy the data/* files to
/sdcard/castle_game_engine/cge_android_lib/, e.g. like this:
  adb shell mkdir /sdcard/castle_game_engine/cge_android_lib/
  adb push data/ /sdcard/castle_game_engine/cge_android_lib/
(TODO: The need for this will disappear once we implement getting
data using Android asset manager, then the data will be just packed
inside .apk.)

If you want to take a look at the Android integration source code,
most important part of it is now inside
castle_game_engine/src/window/castlewindow_android.inc .
Using Android API inside castle_game_engine/src/base/android/ .
