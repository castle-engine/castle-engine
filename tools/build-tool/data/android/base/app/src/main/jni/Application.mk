# This file is used by ndk-build.
# See https://developer.android.com/ndk/guides/application_mk .
#
# Note: It is possible for CGE to work completely without ndk-build
# (since we use FPC to do the actual building)
# and just copy our libraries to jniLibs.
# The only downside would be non-working ndk-gdb.
# Integrating with ndk-build and copying libraries using
# PREBUILT_SHARED_LIBRARY in Android.mk,
# makes ndk-gdb working.
# See https://github.com/castle-engine/castle-engine/wiki/Android-FAQ#debugging-running-application-on-an-android-device-using-ndk-gdb
#
# Note: Specifying APP_ABI is not necessary when ndk-build is called by Gradle.
# Instead ABI are listed in build.gradle as ndk.abiFilters.
# The docs https://developer.android.com/ndk/guides/application_mk
# say explicitly that APP_ABI in Application.mk is ignored then.
#
# However, for now, CGE build tool calls ndk-build directly,
# see RunNdkBuild in ToolAndroidPackage comments,
# this way ndk-gdb remains useful.
# In this case, it *is* necessary to have proper APP_ABI.
APP_ABI := ${ANDROID_ABI_LIST_MAKEFILE}

# NDK platform version should in practice always equal
# minSdkVersion, or be lower than it.
# But it doesn't really make any sense to set it lower,
# so in practice it should be equal.
# NDK platform version must *never* be higher than minSdkVersion
# ( https://stackoverflow.com/questions/21888052/what-is-the-relation-between-app-platform-androidminsdkversion-and-androidtar ).
APP_PLATFORM := android-16
