# src/files/tools: Functionality used by CGE tools

Functionality useful for CGE tools and design-time operations.

This directory contains code shared between:

- CGE build tool, in `tools/build-tool/`

- CGE editor, in `tools/castle-editor/`

- Delphi design-time functionality,
  used from `castle_engine_design.dpk`,
  by unit `CastleInternalDelphiDesign`.

All the units here are _internal_, which means that your own applications should not access these units. However, some of the units here are processed by PasDoc, to document available properties in JSON files like `CastleProjectLocalSettings.json`.
