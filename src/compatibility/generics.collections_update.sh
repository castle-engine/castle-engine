#!/bin/bash
set -eu

# Get the Generics.Collections units, for use in FPC < 3.1.1
# (later FPC contains these already).
#
# Note: we don't use git submodules for this
# (see https://git-scm.com/book/en/v2/Git-Tools-Submodules )
# because git doesn't download the submodules automatically.
# It would need "git submodule init", "git submodule update",
# and the primary purpose of this directory
# is to make CGE "work out-of-the-box" for people with older FPC.

rm -Rf generics.collections
git clone https://github.com/dathox/generics.collections
rm -Rf generics.collections/.git/
