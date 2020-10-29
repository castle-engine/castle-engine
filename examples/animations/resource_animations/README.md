Load a creature modeled and animated in Blender through `resource.xml` .
See https://castle-engine.io/creating_data_resources.php .

This demonstrates various ways of exporting animated 3D models
to Castle Game Engine, the same ("knight") creature is exported
in different ways:

- Single file for all animations (glTF, X3D, castle-anim-frames)

- Multiple files, different file for each animation (multiple X3D, multiple castle-anim-frames)

We declare them in resource.xml files with different names
("KnightSingleXThreeD", "KnightMultipleXThreeD", and so on)
so the engine (ObjectPascal code) sees them as different creature resources.
In a real game, you would choose just one approach to exporting,
and have a single creature named just "Knight".

## What is the best approach?

Single glTF file ( `data/knight_single_gltf/` ).
This works with latest Blender exporter to glTF out-of-the-box,
and gives the best performance and features.

## Authors and license

* The knight 3D model author:

    The model in knight.blend and with texture in textures/knight.png
    based on http://opengameart.org/content/animated-knight
    by Clement Wu, Nikolaus & Botanic.

    Licensed on
      CC-BY 3.0
      CC-BY-SA 3.0
      GPL 3.0
      GPL 2.0
    or above versions of these.

    It was somewhat retouched by Michalis Kamburelis.

* This rest of the example code and data is by Michalis Kamburelis,
  license LGPL >= 2, like the rest of Castle Game Engine,
  see https://castle-engine.io/license.php .
