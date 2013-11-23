First example of using our engine on Android (2D controls and 3D rendering
using full-featured SceneManager (TCastleScene underneath)).
This compiles to a library libandroiddemo.so.

After compiling the library,
- copy the libandroiddemo.so to the android/libs/
  (just like ndk-build does for libraries written in C for Android),
- One time: be sure to call
  android update lib-project -p . --target 1
  to generate android/local.properties (it contains a path to your sdk,
  do NOT commit it)
- compile the project inside android/ using normal Android SDK tools (like ant),
- install and run the resulting apk using normal Android SDK tools.

It's easiest to do everything by
  cd android/
  make

You have to also copy the data/* files to
/sdcard/castle_game_engine/androiddemo/, e.g. like this:
  adb shell mkdir /sdcard/castle_game_engine/androiddemo/
  adb push data/ /sdcard/castle_game_engine/androiddemo/
(TODO: The need for this will disappear once we implement getting
data using Android asset manager, then the data will be just packed
inside .apk.)

If you want to take a look at the Android integration source code,
most important part of it is now inside
castle_game_engine/src/window/castlewindow_android.inc .
Using Android API inside castle_game_engine/src/base/android/ .
