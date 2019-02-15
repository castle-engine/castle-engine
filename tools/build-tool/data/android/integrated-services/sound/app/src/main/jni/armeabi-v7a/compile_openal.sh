#!/bin/bash
set -eu

# See http://pielot.org/2010/12/openal-on-android/
# and repo on http://repo.or.cz/w/openal-soft/android.git
# (and note that all the workarounds and tweaks mentioned there seem
# to be already fixed in the latest version of this repo:)

git clone https://github.com/castle-engine/android-openal

cd android-openal/android/jni/
ndk-build
cd ../../../

cp -f android-openal/android/libs/armeabi-v7a/libopenal.so .
