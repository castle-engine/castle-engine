#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# This script uploads to https://castle-engine.itch.io/platformer/ .
# Uses itch.io command-line butler:
# https://itch.io/docs/butler/installing.html
# https://itch.io/docs/itch/installing/linux/ubuntu-and-debian.html

castle-engine compile
VERSION=`../platformer --version | sed -e "s/^platformer //" `

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

do_package linux x86_64
do_package win64 x86_64

butler status castle-engine/platformer
