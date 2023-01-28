#!/bin/bash
set -euo pipefail

# Extract information about unit dependencies,
# including implementation dependencies.
# Good script to keep track of dependencies between many CGE units.
#
# This can only check units it can compile.
# For now it assumes that it runs on a desktop Unix (like a Linux),
# so it omits stuff specific to Windows, Android, it also omits stuff
# that requires Lazarus LCL.

CHECK_DIR=`pwd`

# Enter CGE src dir.
cd "${CASTLE_ENGINE_PATH}"/src/

# Clean ppu/o files.
make clean -C ../

# Calculate temp file location.
# Work regardless if $HOME/tmp is created.
TMP_PAS_LIST="${CASTLE_ENGINE_PATH}/test-cge-units-dependencies_all_units.txt"
TMP_LOG="${CASTLE_ENGINE_PATH}/cge_check_units_dependencies.log"

# clean previous log
rm -f "${TMP_LOG}"
echo "Logging compilation output to ${TMP_LOG}"

echo "All units list in ${TMP_PAS_LIST}"
# Avoid
# - compatibility, to avoid https://bugs.freepascal.org/view.php?id=32192
# - vampyre_imaginglib:
#   To avoid considering DOM, XMLRead etc. (Vampyre has their copies) inside CGE.
#   Also, some units inside may not compile without LCL.
# - lcl, indy as they depend on LCL, Indy and compilation with just `castle-engine simple-compile ...` may fail
# - deprecated_units, we don't track their dependencies
# - delphi, as they only compile with Delphi, as this check uses FPC later
find . \
  '(' -type d -iname android -prune ')' -or \
  '(' -type d -iname windows -prune ')' -or \
  '(' -type d -iname lcl -prune ')' -or \
  '(' -type d -iname delphi -prune ')' -or \
  '(' -type d -iname indy -prune ')' -or \
  '(' -type d -iname nodes_specification -prune ')' -or \
  '(' -type d -iname compatibility -prune ')' -or \
  '(' -type d -iname deprecated_units -prune ')' -or \
  '(' -type d -iname vampyre_imaginglib -prune ')' -or \
  '(' -type f -iname '*.pas' -print ')' > "${TMP_PAS_LIST}"

SUCCESS='true'

for F in `cat "${TMP_PAS_LIST}"`; do
  echo "Checking dependencies of $F"
  castle-engine simple-compile "$F" >> "${TMP_LOG}"
  # Like `stringoper ChangeFileExt %F .ppu`
  PPU="${F%.*}.ppu"

  # Strip directory name,
  # and add castle-engine-output/compilation/<arch-cpu>/,
  # because that's where "castle-engine simple-compile" places ppu now.
  PPU="`basename \"${PPU}\"`"
  PPU='castle-engine-output/compilation/'`fpc -iTP`-`fpc -iTO`"/${PPU}"

  DEPENDENCIES_TO_CHECK=`ppudump "$PPU" | grep 'Uses unit' | awk '{ print $3 }' | sort -u`
  # echo 'Got dependencies:'
  # echo "$DEPENDENCIES_TO_CHECK"

  set +e
  "${CHECK_DIR}"/check_one_unit_dependencies "${TMP_PAS_LIST}" "$F" "$DEPENDENCIES_TO_CHECK"
  if [ "$?" != 0 ]; then
    echo 'check_units_dependencies.sh: Failure detected, we will continue but exit with non-zero status at the end'
    SUCCESS='false'
  fi
  set +e
done

if [ "${SUCCESS}" = 'false' ]; then
  echo 'Finished, some errors detected, exiting with non-zero status. Consult the above log for details what failed.'
  exit 1
else
  echo 'Finished, everything correct.'
fi
