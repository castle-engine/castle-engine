First example of using our engine on Android (2D controls and 3D rendering
using full-featured SceneManager (TCastleScene underneath)).

Compile and run the application for Android by
https://github.com/castle-engine/castle-engine/wiki/Build-Tool :

  castle-engine package --target=android --mode=debug
  castle-engine install --target=android
  castle-engine run --target=android

This does everything necessary to compile the source code
into the final apk, and even installs and runs it
(if you connected your Android device with debugging enabled):

- Compiles the library (with native code for Android) with FPC.
- Copies the library to a temporary Android project directory.
- Copies the data/ contents to Android project assets.
  This way the data will be available inside .apk, which is the best
  way to distribute game data with your program.
- Packages the project using Android Gradle build system.
- Installs and runs the resulting apk using Android SDK tools.

Note that you will need to install Android SDK, Android NDK
and FPC cross-compiler first. See
https://github.com/castle-engine/castle-engine/wiki/Android
for details how to do it.

If you want to take a look at the internals of the Android integration,
most important part of it is now inside
castle_game_engine/src/window/castlewindow_android.inc .
Using Android API inside castle_game_engine/src/base/android/ .
