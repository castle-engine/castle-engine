# Combine Multiple Models Into One Scene

An example how to load multiple 3D model files into a single X3D graph,
by combining them into a single X3D tree rooted in TX3DRootNode.

This is useful for example when auto-generating a complex 3D scene
by code, from multiple small X3D files.
If you want to render the complex scene fast, it may be beneficial to combine
it into a single TCastleScene, instead of using multiple TCastleScenes.
See https://castle-engine.io/manual_transformation_hierarchy.php for detailed
discussion when this makes sense.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `combine_multiple_x3d_into_one_scene_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `combine_multiple_x3d_into_one_scene_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
