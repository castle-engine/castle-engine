# Command-line tool to detect placeholder names in models

_This is deprecated. The placeholder detection is used by `TLevel.Load`, which will also be deprecated. You can instead design your levels, with placeholders, using CGE editor, and use any TCastleTransform for placeholders. See "3D FPS Game" template for example._

Pass on command-line the URL (usually just a filename) of 3D model file and the name of placeholder detection routine (like "x3dshape" or "blender"). We will output information about detected placeholders in the file.

For example,

```
./placeholder_names model.x3d blender
./placeholder_names ../fps_game/data/example_level/example_level.gltf blender
```

Using `blender` approach assumes that model.x3d was created using standard Blender X3D exporter, and will show Blender object names for every shape in the model.

See `TLevel.Load` for a description where we use placeholders in the engine.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `build_3d_object_by_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
