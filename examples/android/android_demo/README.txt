First example of using our engine on Android (2D controls and 3D rendering
using full-featured SceneManager (TCastleScene underneath)).
This compiles to a library libandroiddemo.so.

Compile and run the application for Android by
https://github.com/castle-engine/castle-engine/wiki/Build-Tool :

  castle-engine package --os=android --cpu=arm --mode=debug
  castle-engine install --os=android --cpu=arm

This does everything necessary to compile the source code
into the final apk, and even installs and runs it
(if you connected your Android device with debugging enabled):

- Compiles the library libandroiddemo.so with FPC.
- Copies libandroiddemo.so to the temporary Android project directory.
- Uses ndk-build to add debug symbols.
- Copies the data/ contents to Android project assets.
  This way the data will be available inside .apk, which is the best
  way to distribute game data with your program.
- Packages the project  "ant" from Android SDK tools.
- Installs and runs the resulting apk using Android SDK tools.

Note that you will need to install Android SDK, Android NDK
and FPC cross-compiler first. See
https://github.com/castle-engine/castle-engine/wiki/Android
for details how to do it.

If you want to take a look at the internals of the Android integration,
most important part of it is now inside
castle_game_engine/src/window/castlewindow_android.inc .
Using Android API inside castle_game_engine/src/base/android/ .
