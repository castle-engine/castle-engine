This example loads a creature modeled and animated in Blender.
To demonstrate various ways of exporting animated 3D models
to Castle Game Engine, the same ("knight") creature is exported
in 4 different ways:

1. Single X3D file with all the animations,
   in data/knight_single_x3d/
2. Multiple X3D files with one for each animation,
   in data/knight_multiple_x3d/
3. Single castle-anim-frames file with all the animations,
   in data/knight_single_castle_anim_frames/
4. Multiple castle-anim-frames files, one for each animation,
   in data/knight_multiple_castle_anim_frames/

We declare them in resource.xml files with different names
("KnightSingleXThreeD", "KnightMultipleXThreeD", and so on)
so the engine (ObjectPascal code) sees them as 4 different creature resources.
In a real game, you would choose just one approach to exporting,
and have a single creature named just "Knight".

Notes about modelers and exporters:

- The 1st method ("Single X3D file") should be considered the best
  (smallest file size, loading time, memory consumption).
  If your 3D authoring software allows to export multiple animations
  to an X3D file, then use it.

- For Blender, you will probably export your animations to
  "castle-anim-frames" file.
  See http://castle-engine.sourceforge.net/creating_data_blender.php .

  Although it is possible to convert "castle-anim-frames" later to X3D,
  but it may require some manual work (you can help yourself with our tool
  3d_rendering_processing/tools/castle_anim_frames_to_interpolators, but it's
  not completely automatic for non-trivial models).

  So you may as well just use "castle-anim-frames" when working
  with Blender.

The knight 3D model author:
  The model in knight.blend and with texture in textures/knight.png
  based on http://opengameart.org/content/animated-knight
  by Clement Wu, Nikolaus & Botanic,
  licensed on
    CC-BY 3.0
    CC-BY-SA 3.0
    GPL 3.0
    GPL 2.0
  or above versions of these.
  It was somewhat retouched by Michalis Kamburelis.

This rest of the example code and data is by Michalis Kamburelis,
license LGPL >= 2, like the rest of Castle Game Engine,
see COPYING.txt in parent castle_game_engine/ directory.
