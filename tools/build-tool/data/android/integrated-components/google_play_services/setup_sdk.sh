#!/bin/bash
set -e

if [ -z "${ANDROID_HOME}" ]; then
  echo '$ANDROID_HOME environment variable is not defined.'
  exit 1
fi

LIB_LOCATION="${ANDROID_HOME}/extras/google/google_play_services/libproject/google-play-services_lib/"

if [ '!' -d "${LIB_LOCATION}" ]; then
  echo "Cannot find google-play-services_lib library (in ${LIB_LOCATION})."
  echo 'Run the Android SDK manager ("android" tool)'
  echo '  and install "Extras -> Google Play Services".'
  exit 1
fi

cp -R "${LIB_LOCATION}" .
