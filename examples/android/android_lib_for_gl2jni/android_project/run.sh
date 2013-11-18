#!/bin/bash
set -eu

mkdir -p libs/armeabi/
cp -f ../libcge_android_lib.so libs/armeabi/libgl2jni.so

ant debug
adb install -r bin/GL2JNIActivity-debug.apk
adb shell am start -a android.intent.action.MAIN -n com.android.gl2jni/.GL2JNIActivity
