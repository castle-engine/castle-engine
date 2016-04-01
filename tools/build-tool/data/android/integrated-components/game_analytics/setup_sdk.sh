#!/bin/bash
set -eu

BASE_URL='https://github.com/GameAnalytics/GA-SDK-ANDROID/blob/master/GA/'
URL_SUFFIX='?raw=true'

wget "${BASE_URL}"jar/libs/gameanalytics.jar"${URL_SUFFIX}" \
--output-document libs/gameanalytics.jar

# Unused tests of using aar below.
#
# mkdir -p gameanalytics/
# wget "${BASE_URL}"aar/gameanalytics.aar"${URL_SUFFIX}" \
# --output-document gameanalytics/gameanalytics.aar
# cd gameanalytics/
# unzip gameanalytics.aar
# rm -f gameanalytics.aar
