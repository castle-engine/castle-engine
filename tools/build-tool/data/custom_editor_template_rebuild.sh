#!/bin/bash
set -eu

# Update files in custom_editor_template/ based on current editor source code.
# Uses "sed" (assuming it is GNU sed).

EDITOR_SOURCE_ROOT=../../castle-editor/

cp -f "${EDITOR_SOURCE_ROOT}castle_editor.ico" custom_editor_template/

cp -f "${EDITOR_SOURCE_ROOT}castle_editor.lpr" custom_editor_template/
sed --in-place \
  -e 's|// This line will be automatically uncommented by tools/build-tool/data/custom_editor_template_rebuild.sh|// This line was uncommented by tools/build-tool/data/custom_editor_template_rebuild.sh|' \
  -e 's|//castle_editor_automatic_package|castle_editor_automatic_package|' \
  custom_editor_template/castle_editor.lpr

cp -f "${EDITOR_SOURCE_ROOT}castle_editor.lpi" custom_editor_template/
sed --in-place \
  -e 's|<RequiredPackages Count="4">|<RequiredPackages Count="5">|' \
  -e 's|</RequiredPackages>|<Item5> <PackageName Value="castle_editor_automatic_package"/> </Item5> </RequiredPackages>|' \
  -e 's|<Filename Value="\(code/[_a-zA-Z0-9]\+.pas\)"|<Filename Value="${CASTLE_ENGINE_PATH}tools/castle-editor/\1"|' \
  -e 's|<Filename Value="../common-code/|<Filename Value="${CASTLE_ENGINE_PATH}tools/common-code/|' \
  -e 's|<IncludeFiles Value="../../src/common_includes;$(ProjOutDir)"/>|<IncludeFiles Value="${CASTLE_ENGINE_PATH}src/common_includes;$(ProjOutDir)"/>|' \
  -e 's|<OtherUnitFiles Value="../common-code;code"/>|<OtherUnitFiles Value="${CASTLE_ENGINE_PATH}tools/castle-editor/code;${CASTLE_ENGINE_PATH}tools/common-code"/>|' \
  -e 's|</SearchPaths>|<Libraries Value="${ABSOLUTE_LIBRARY_PATHS}" /> </SearchPaths>|' \
  -e 's|<ConfigFilePath Value="../../castle-fpc-messages.cfg"/>|<ConfigFilePath Value="${CASTLE_ENGINE_PATH}castle-fpc-messages.cfg"/>|' \
  -e 's|<Filename Value="castle-editor"/>|<Filename Value="castle-editor-new"/>|' \
  custom_editor_template/castle_editor.lpi
