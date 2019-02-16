# OpenAL (sound engine) for Android

The exact source code of this library is on

  https://github.com/castle-engine/android-openal

Forked from http://repo.or.cz/w/openal-soft/android.git .
Note that all the workarounds and tweaks mentioned on
http://pielot.org/2010/12/openal-on-android/
seem to be already fixed in the latest version of this repo :)

Recompile like this:

  git clone https://github.com/castle-engine/android-openal

  cd android-openal/android/jni/
  ndk-build
  cd ../../../

  cp -f android-openal/android/libs/armeabi-v7a/libopenal.so \
    app/src/main/jni/armeabi-v7a/libopenal.so
