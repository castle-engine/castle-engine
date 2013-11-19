#!/bin/bash
set -eu

cd ../../../
ppcrossarm -Tandroid -CfVFPV3 -dDEBUG ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg examples/android/android_demo/cge_android_lib.lpr
