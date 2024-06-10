# ----------------------------------------------------------------------------
# PowerShell (for Windows) script to compile build tool,
# using only FPC (no lazbuild, no previous castle-engine necessary).
# ----------------------------------------------------------------------------

# Allow calling this script from tools/build-tool/ subdirectory of CGE.
# It will just cd to the top-level CGE directory (necessary as castle-fpc.cfg
# contains paths relative to it).
if (Test-Path castle_engine.dpr) {
  cd ../..
}

New-Item -Path tools/build-tool/ -Name castle-engine-output -ItemType "directory" -Force
New-Item -Path tools/build-tool/castle-engine-output/ -Name build-tool-compilation -ItemType "directory" -Force

fpc `
  -dRELEASE `
  -dCASTLE_STRICT_CLI `
  @castle-fpc.cfg `
  -FEtools/build-tool/ `
  -FUtools/build-tool/castle-engine-output/build-tool-compilation `
  -Futools/common-code/ `
  -Futools/build-tool/code/ `
  -Futools/build-tool/embedded_images/ `
  tools/build-tool/castle_engine.dpr

# Make final exe name with -, not _ .
# Note: Using PowerShell Move-Item, not Rename-Item, because Rename-Item
# (with with -Force) cannot overwrite destination.
Move-Item -Path "tools/build-tool/castle_engine.exe" -Destination "tools/build-tool/castle-engine.exe" -Force
