#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# This script uploads to https://castle-engine.itch.io/platformer/ .
# Uses itch.io command-line butler:
# https://itch.io/docs/butler/installing.html
# https://itch.io/docs/itch/installing/linux/ubuntu-and-debian.html

castle-engine compile
VERSION=`castle-engine output version`

# Package for platform expressed as OS/CPU combination (using default --target=custom)
# for CGE build tool
do_package ()
{
  OS="$1"
  CPU="$2"
  shift 2

  OUT_DIR=platformer-"${OS}"-"${CPU}"

  rm -Rf "${OUT_DIR}"
  castle-engine package --package-format=directory --package-name-no-version --mode=release --os="${OS}" --cpu="${CPU}"
  cp platformer-"${OS}".itch.toml "${OUT_DIR}"/.itch.toml
  butler push "${OUT_DIR}" castle-engine/platformer:"${OS}"-"${CPU}" --userversion "${VERSION}"
}

# Package for Android.
# This is a specialized and adjusted version of do_package -- we need to use --targer, and upload apk.
do_package_android ()
{
  TARGET="android"
  OUT_DIR=platformer-"${TARGET}"

  if [ ! -f ../AndroidSigningProperties.txt ]; then
    echo 'No AndroidSigningProperties.txt, we cannot make release APK.'
    exit 1
  fi

  rm -Rf "${OUT_DIR}"
  mkdir "${OUT_DIR}"
  castle-engine package --package-name-no-version --mode=release --target="${TARGET}"
  # Looks like manifest (platformer-android.itch.toml) is pointless in case of Android,
  # also it makes a weird download (with zip with 2 files).
  # So we just upload APK directly.
  # Itch.io automatically marks it as "Android platform" in the file list.
  butler push ../platformer-android-release.apk castle-engine/platformer:"${TARGET}" --userversion "${VERSION}"
}

do_package linux x86_64
do_package win64 x86_64
do_package_android

butler status castle-engine/platformer
