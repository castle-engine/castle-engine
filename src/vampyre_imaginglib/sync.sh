#!/bin/bash
set -eu

rm -Rf src/
git clone https://github.com/galfar/imaginglib src/
cd src/

# Remove .git to avoid confusing any git client, the repo of this is CGE repo.
# Workaround "Permission denied" on GIT for Windows, see https://stackoverflow.com/a/59019201
chmod -R a+w .git
rm -Rf .git

# We add Vampyre units through castle_engine_base package (in Lazarus and Delphi),
# this makes things easier for Lazarus and Delphi users,
# see CGE packages/README.md .
# Remove original Vampyre Lazarus packages, to not confuse users.
rm -Rf Packages
rm -Rf Extras/Packages

# Remove this, as it was confusing people, as it had DOM unit sources
# (rather dated copy of FPC DOM unit sources).
# In CGE, we use
# - FPC's DOM (with FPC)
# - or our DOM implementation, on top of Delphi's XML units,
#   in src/compatibility/delphi-only/dom.pas .
rm -Rf Extras/Tools/VampyreDoc/

# Remove Demos, as they contained also dglOpenGL.pas and this was confusing.
# In CGE, we use CastleGL, which is our adjusted fork of dglOpenGL.
rm -Rf Demos

# Remove more, to not confuse CGE users with things we don't use here.
rm -Rf .github/ Scripts/
