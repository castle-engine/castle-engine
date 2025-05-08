The data-source/ directory contains the sources for respective files
in the data/ directory.

Place here files that you don't want to distribute
with the regular binary game release,
and you don't access these files in your application at runtime.

In other words, these files are only useful for developing the game assets.
Examples of files to put here:

- Blender .blend files (and the textures in source formats)
- Spine .spine files (with the images/ subdirectory, used by Spine to define each element)
- GIMP .xcf files
- Photoshop .psd files
- Audacity project files

These assets are exported to data/ in a format readable by the game.
Examples of files to put in data/ :

- 3D and 2D models exported to .gltf or .x3d, along with final textures
- Spine animations exported to .json, along with atlas files
- Final images in .png, .jpg and other formats supported by Castle Game Engine
- Final sound files in .wav, .ogg formats

Castle Game Engine doesn't treat the `data-source` directory in any special way.
It is not packaged by the [build tool](https://castle-engine.io/build_tool)
command `castle-engine package`, as it only packages the special `data` directory
by default.

There is an alternative method to avoid packing your "source data files":
you could place them in `data/` but avoid packing using the `<exclude>` mechanism
in [CastleEngineManifest.xml](https://castle-engine.io/project_manifest).
