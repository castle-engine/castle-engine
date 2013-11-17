#!/bin/bash
set -eu

cd ../../../
ppcrossarm -Tandroid -CfVFPV3 -dDEBUG ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg examples/android/android_lib_for_gl2jni/cge_android_lib.lpr
