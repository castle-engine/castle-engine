#!/bin/bash
set -e

# ----------------------------------------------------------------------------
# Compile CGE library for iOS targets.
# See https://castle-engine.io/ios
#
# Executes the build tool, which in turn will execute FPC.
# The build tool searches for FPC on $PATH.
#
# Note that the compilation artifacts will be in ./castle-engine-output/ , and that's good,
# we want separate compilation artifacts (from other projects) to:
#
# 1. make sure that CastleWindow unit is compiled with LIBRARY backend,
# 2. make sure that every unit is recompiled with -fPIC (necessary for .so on x86_64).
# ----------------------------------------------------------------------------

# Never use cache, as it will likely contain CastleWindow with different backend, and units without -fPIC.
castle-engine cache-clean

# Variant not using CastleEngineManifest.xml
#castle-engine simple-compile --target=ios --ios-simulator --compiler-option=-fPIC --compiler-option=-dCASTLE_WINDOW_LIBRARY --verbose castleengine.lpr
#libtool -static -o libcastleengine.a castle-engine-output/compilation/aarch64-ios/lib_cge_project.a castle-engine-output/compilation/x86_64-iphonesim/lib_cge_project.a

# Variant using CastleEngineManifest.xml
castle-engine compile --target ios --ios-simulator
