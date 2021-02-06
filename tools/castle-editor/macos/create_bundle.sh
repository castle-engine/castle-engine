#!/bin/bash
set -eux

# Creating "Castle Game Engine.app" directory , which is a macOS "bundle"
# (magic directory that behaves like a clickable application under macOS).
#
# This is a simplified and specialized version of
# https://github.com/castle-engine/cge-scripts/blob/master/create_macosx_bundle.sh .
#
# Before calling this, compile editor binary like "lazbuild code/castle_editor.lpi".
# Note that this script can in principle put any binary (compiled for any OS/CPU)
# into a bundle, but it really only makes sense to package this way macOS binary
# (OS=darwin, CPU=x86_64).

# Configurable variables -----------------------------------------------------

BUNDLE_NAME='Castle Game Engine'
BUNDLE_DIR="${BUNDLE_NAME}.app"
EXE_NAME='castle-editor'
APP_VERSION='7.0-alpha.snapshot'

# Do the job -----------------------------------------------------------------

rm -Rf "${BUNDLE_DIR}"
mkdir -p "${BUNDLE_DIR}/Contents/MacOS"
mkdir -p "${BUNDLE_DIR}/Contents/Resources"

cp "${EXE_NAME}" "${BUNDLE_DIR}/Contents/MacOS"

# TODO: put icon in "${BUNDLE_DIR}/Contents/Resources/${EXE_NAME}.icns"

echo "APPL????" > "${BUNDLE_DIR}/Contents/PkgInfo"

cat > "${BUNDLE_DIR}/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleDevelopmentRegion</key>
  <string>English</string>
  <key>CFBundleExecutable</key>
  <string>${EXE_NAME}</string>
  <key>CFBundleName</key>
  <string>${BUNDLE_NAME}</string>
  <key>CFBundleIdentifier</key>
  <string>io.castle-engine.${EXE_NAME}</string>
  <key>CFBundleInfoDictionaryVersion</key>
  <string>6.0</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>CFBundleSignature</key>
  <string>view</string>
  <key>CFBundleShortVersionString</key>
  <string>${APP_VERSION}</string>
  <key>CFBundleVersion</key>
  <string>${APP_VERSION}</string>
  <key>CSResourcesFileMapped</key>
  <true/>
  <!-- TODO -->
  <!--key>CFBundleIconFile</key-->
  <!--string>${EXE_NAME}</string-->
  <key>CFBundleDocumentTypes</key>
  <array>
    <dict>
      <key>CFBundleTypeRole</key>
      <string>Viewer</string>
      <key>CFBundleTypeExtensions</key>
      <array>
        <string>*</string>
      </array>
      <key>CFBundleTypeOSTypes</key>
      <array>
        <string>fold</string>
        <string>disk</string>
        <string>****</string>
      </array>
    </dict>
  </array>
  <key>NSHighResolutionCapable</key>
  <true/>
</dict>
</plist>
EOF
