# This file is only used by ndk-build.
# Which in turn is only useful for us to make ndk-gdb working.
# See https://github.com/castle-engine/castle-engine/wiki/Android-FAQ#debugging-running-application-on-an-android-device-using-ndk-gdb
#
# Note that we could work without ndk-build actually,
# and just copy our libraries to jniLibs.
# The only downside would be non-working ndk-gdb.
#
# The APP_ABI is necessary to indicate ABI of our SO files to NDK.
# With older Android NDK versions, this was not necessary.
# But at least from r12b (maybe earlier) not declaring this is an error,
# results in errors like
#   .../ndk/toolchains/x86_64-4.9/prebuilt/linux-x86_64/bin/x86_64-linux-android-strip: Unable to recognise the format of the input file `./libs/x86_64/libandroiddemo.so'
#
# The armeabi-v7a is our proper platform, with hard floats.
# See https://developer.android.com/ndk/guides/application_mk.html
# and http://stackoverflow.com/questions/24948008/linking-so-file-within-android-ndk
# and *do not* confuse this with (removed now) armeabi-v7a-hard ABI:
# https://android.googlesource.com/platform/ndk/+show/353e653824b79c43b948429870d0abeedebde386/docs/HardFloatAbi.md

APP_ABI := armeabi-v7a
