#!/bin/bash
set -eu

# -----------------------------------------------------------------------
# Generate CastleInternalJobWeb using the webidl2pas.
#
# WEBIDLs (in parts/) have been downloaded from
# https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/ .
#
# They have been modified (cut down, sometimes *a lot*)
# to remove all the stuff that is not necessary
# for the Castle Game Engine (and thus avoid pulling in a lot of dependencies
# and having huge CastleInternalJobWeb unit).
# -----------------------------------------------------------------------

# Note: The order in which we glue these files does not matter.
# webidl2pas will handle it right anyway.

rm -f castleinternaljobweb.webidl
for F in parts/*.webidl; do
  echo '/*' $F '----------------------------------------------------- */' >> castleinternaljobweb.webidl
  cat "$F" >> castleinternaljobweb.webidl
done

webidl2pas --input=castleinternaljobweb.webidl --outputformat=wasmjob \
  --typealiases=@alias_webidl_types_in_other_units.txt \
  --globals=@global_webidl_vars.txt \
  --unitname=CastleInternalJobWeb \
  --output=../castleinternaljobweb.pas \
  --optionsinheader
