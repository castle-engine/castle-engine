#!/bin/bash
set -e

if [ -z "${ANDROID_HOME}" ]; then
  echo '$ANDROID_HOME environment variable is not defined.'
  exit 1
fi

LIB_LOCATION="${ANDROID_HOME}/extras/google/google_play_services/libproject/google-play-services_lib/"

# google_play_services_froyo is really old... It will require removing
# the @integer/google_play_services_version referefence from AndroidManifest.xml,
# and it will probably have other problems.
#
# if [ '!' -d "${LIB_LOCATION}" ]; then
#   # fallback on alternative location
#   LIB_LOCATION="${ANDROID_HOME}/extras/google/google_play_services_froyo/libproject/google-play-services_lib/"
# fi

if [ '!' -d "${LIB_LOCATION}" ]; then
  echo "Cannot find google-play-services_lib library (in ${LIB_LOCATION})."
  echo 'Run the Android SDK manager ("android" tool)'
  echo '  and install "Extras -> Google Play Services".'
  exit 1
fi

# Leaving previous google-play-services_lib contents could break the build
# if the old files (that should no longer be used) are left. E.g. when testing
# google_play_services_froyo we get ant errors like:
#
#   [aapt] Generating resource IDs...
#   [aapt] /tmp/castle-engine375324/google-play-services_lib/res/values/colors.xml:13: error: Resource entry common_action_bar_splitter is already defined.
#   [aapt] /tmp/castle-engine375324/google-play-services_lib/res/values/base_colors.xml:12: Originally defined here.
#   ....
# BUILD FAILED
# /home/michalis/installed/android/sdk/tools/ant/build.xml:597: The following error occurred while executing this line:
# /home/michalis/installed/android/sdk/tools/ant/build.xml:649: The following error occurred while executing this line:
# /home/michalis/installed/android/sdk/tools/ant/build.xml:694: null returned: 1
#
# Although google_play_services_froyo should not be used anyway (very old),
# it's still a good idea to clear the previous version.
rm -Rf google-play-services_lib

cp -R "${LIB_LOCATION}" .
echo "Found Google Play Services library (${LIB_LOCATION})."

SUPPORT_LIB_LOCATION="${ANDROID_HOME}/extras/android/support/v13/android-support-v13.jar"

if [ '!' -f "${SUPPORT_LIB_LOCATION}" ]; then
  echo "Cannot find android-support-v13.jar library (in ${SUPPORT_LIB_LOCATION})."
  echo 'Run the Android SDK manager ("android" tool) and install "Extras -> Android Support Library" (you may need to select the "Obsolete" checkbox first).'
  echo 'Or from the command-line run'
  echo '  android update sdk --no-ui -a --filter "extra-android-support"'
  # command-line version from http://stackoverflow.com/questions/12382555/android-support-library-manual-download
  exit 1
fi

mkdir -p libs/
cp -R "${SUPPORT_LIB_LOCATION}" libs/
echo "Found Android Support Library (${SUPPORT_LIB_LOCATION})."
echo 'Component google_play_services setup OK.'
