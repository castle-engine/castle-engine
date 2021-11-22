#!/bin/bash
set -eu

rm -Rf src/
git clone https://github.com/galfar/imaginglib src/
cd src/

# Remove .git to avoid confusing any git client, the repo of this is CGE repo.
# Workaround "Permission denied" on GIT for Windows, see https://stackoverflow.com/a/59019201
chmod -R a+w .git
rm -Rf .git
