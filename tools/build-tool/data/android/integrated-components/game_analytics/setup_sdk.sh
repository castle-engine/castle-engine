#!/bin/bash
set -eu

BASE_URL='https://github.com/GameAnalytics/GA-SDK-ANDROID/blob/master/GA/'
URL_SUFFIX='?raw=true'

wget "${BASE_URL}"libs/gameanalytics.jar"${URL_SUFFIX}" \
--output-document libs/gameanalytics.jar

wget "${BASE_URL}"src/main/jniLibs/armeabi/libGameAnalytics.so"${URL_SUFFIX}" \
             --output-document jni/armeabi/libGameAnalytics.so
wget "${BASE_URL}"src/main/jniLibs/armeabi-v7a/libGameAnalytics.so"${URL_SUFFIX}" \
             --output-document jni/armeabi-v7a/libGameAnalytics.so
wget "${BASE_URL}"src/main/jniLibs/x86/libGameAnalytics.so"${URL_SUFFIX}" \
             --output-document jni/x86/libGameAnalytics.so
