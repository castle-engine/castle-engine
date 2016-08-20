# This is necessary to indicate ABI of our SO files.
#
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
