First example of using our engine (2D controls and 3D rendering,
using full-featured SceneManager (TCastleScene underneath)).
This compiles to a library libcge_android_lib.so.

After compiling, copy also the data/* files to /sdcard/kambitest/,
e.g. using
  adb push data/ /sdcard/kambitest

Then use Android's NDK sample android-ndk-r9b/samples/hello-gl2.
Simply
  cp -f libcge_android_lib.so libs/armeabi/libgl2jni.so
(that is, use our library instead of the one compiled from C with NDK's gcc).

And compile/run hello-gl2 on your Android device as usual.

So, right now, this depends on Java wrapper from hello-gl2,
that already initializes OpenGL context for us.
TODO:
A version that allows us to initialize context (using EGL) will follow soon :),
and also our own Java wrapper, possibly even just use Android's
NativeActivity without any custom Java code.

