First example of using our engine on Android (2D controls and 3D rendering
using full-featured SceneManager (TCastleScene underneath)).
This compiles to a library libandroiddemo.so.

It is easiest to compile and run the application for Android by:

  cd android/
  android update lib-project -p . --target 1 # do this only 1st time to generate android/local.properties
  make

The "make" command does everything necessary to compile the source code
into the final apk, and even installs and runs it
(if you connected your Android device with debugging enabled):

- Compiles the library libandroiddemo.so with FPC.
- Copies libandroiddemo.so to the android/libs/
  (just like ndk-build does for libraries written in C for Android).
- Copies the data/ contents to android/assets/ .
  This way the data will be available inside .apk, which is the best
  way to distribute game data with your program.
- Packages the project inside android/ using "ant" from Android SDK tools.
- Installs and runs the resulting apk using Android SDK tools.

Note that you will need to install Android SDK, Android NDK
and FPC cross-compiler first. See
https://sourceforge.net/p/castle-engine/wiki/Android%20development/
for details how to do it.

If you want to take a look at the internals of the Android integration,
most important part of it is now inside
castle_game_engine/src/window/castlewindow_android.inc .
Using Android API inside castle_game_engine/src/base/android/ .
