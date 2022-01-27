#!/bin/bash
set -e
# enable /** globbing 
shopt -s globstar

ARG=${1:-no-arg}

echo "Deleting ugly files..."

ROOTDIR="$(dirname "$0")/.."

PAT_UNITS="*.dcu *.ppu *.rsj *.or"
# Careful with these! We have some precompiled C code we want to keep 
PAT_OBJ="*.obj *.o"
PAT_DELPHI="*.identcache *.local *.ddp *.tvsconfig *.otares *.deployproj *.stat"
PAT_BACKUPS="*.bak *.bk? *.~* *.*~"
PAT_DEBUG="*.dbg *.rsm *.jdbg"
PAT_MISC="*.compiled link????.res Thumbs.db"

ALL_BUT_OBJ_PATS="$PAT_UNITS $PAT_DELPHI $PAT_BACKUPS $PAT_DEBUG $PAT_MISC"
ALL_PATS="$ALL_BUT_OBJ_PATS $PAT_OBJ"

delInTree() {     
  for PAT in $2; do 
    find $1 -mindepth 1 -iname "$PAT" -type f | xargs rm -f 
  done  
}

delWholeDir() {
  rm -rf $1 
}


delWholeDir "$ROOTDIR/Bin/Dcu"
delWholeDir "$ROOTDIR/Demos/Bin/Dcu"

if [ $ARG = "--clean-also-bin-dirs" ]; then
  # careful
  delWholeDir "$ROOTDIR/Bin/*" 
  delWholeDir "$ROOTDIR/Demos/Bin/*" 
fi

delInTree "$ROOTDIR/Source" "$ALL_PATS"
delInTree "$ROOTDIR/Demos" "$ALL_PATS" 
delInTree "$ROOTDIR/Packages" "$ALL_PATS" 

delInTree "$ROOTDIR/Extensions" "$ALL_BUT_OBJ_PATS"
delInTree "$ROOTDIR/Extras" "$ALL_BUT_OBJ_PATS"

delWholeDir "$ROOTDIR/**/__history"
delWholeDir "$ROOTDIR/**/backup"
delWholeDir "$ROOTDIR/**/__recovery"

echo "Clean finished"
