#!/bin/bash
set -eu

# See http://pielot.org/2010/12/openal-on-android/
# and repo on http://repo.or.cz/w/openal-soft/android.git
# (and note that all the workarounds and tweaks mentioned there seem
# to be already fixed in the latest version of this repo:)

git clone http://repo.or.cz/openal-soft/android.git

cd android/android/jni/
ndk-build
cd ../../../

cp -f android/android/libs/armeabi/libopenal.so .
