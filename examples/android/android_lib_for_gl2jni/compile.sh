#!/bin/bash
set -eu

cd ../../../
ppcrossarm -Tandroid -CfVFPV3 -dDEBUG ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg examples/library/android_lib/cge_android_lib.lpr
