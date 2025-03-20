#!/bin/bash
set -euo pipefail

SRC_MANUAL_TYPES="${CASTLE_ENGINE_PATH}/src/scene/load/ifc/castleifc_ifc_standard_types.inc"
OUTPUT_MANUAL_TYPES_TXT="${CASTLE_ENGINE_PATH}/tools/internal/generate_ifc/data/manually_defined_types.txt"

grep '^  TIfc.* =' "${SRC_MANUAL_TYPES}" > "${OUTPUT_MANUAL_TYPES_TXT}"

sed -i -e 's|^  \(TIfc.*\) =.*|\1|g' "${OUTPUT_MANUAL_TYPES_TXT}"
