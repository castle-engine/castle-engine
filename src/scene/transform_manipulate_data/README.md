# Data (3D gizmos) for `TCastleTransformManipulate`

- Designed in [Blender](https://castle-engine.io/blender), `.blend` files inside `source/`.

- Exported to glTF. And wrapped in an X3D files like `xxx_final.x3dv` to define comfortably what is collider, what is rendered.

    The exported files are in `exported/` folder.

    You can open glTF and X3D files here in [Castle Model Viewer](https://castle-engine.io/castle-model-viewer).

- Then converted to Pascal array definitions in `generated-pascal`, to embed them in Pascal units (in a way that works cross-platform and with both FPC and Delphi).

    For this we invoke `make` that in turn uses [file_to_pascal_data](https://github.com/pasdoc/pasdoc/blob/master/source/tools/file_to_pascal_data.dpr). Just download this single file and compile (with FPC or Delphi) and put the `file_to_pascal_data[.exe]` in your `$PATH`. It is part of [PasDoc](https://github.com/pasdoc/pasdoc/) tools.
