This example loads a creature modeled and animated in Blender.

To demonstrate various ways of exporting the 3D models for creatures
for Castle Game Engine, the knight creature is exported in 3 different
ways:

1. Single X3D file with all animations, in data/knight_single_x3d/
2. Multiple X3D files with one for each animation, in data/knight_multiple_x3d/
3. KAnim file (referting to a series of X3D models for each still frame)
   for each animation, in data/knight_kanim/

We declare them in resource.xml files with different names
("KnightSingleX3D", "KnightMultipleX3D", "KnightKAnim")
so the engine (ObjectPascal code) actually sees three different creature kinds.
In a real game, you should probably choose just one approach to exporting,
and have a single creature named just "Knight".

Notes about modelers and exporters:

- The 1st method ("Single X3D file") should be considered the best
  (smallest file size, loading time, memory consumption).

- When using Blender:

  Unfortunately, only the 3rd method ("KAnim file")
  is right now possible to do automatically from Blender, using
  our KAnim exporter on http://castle-engine.sourceforge.net/blender.php .
  Other methods require some manual work to edit the X3D files,
  and/or helping yourself with our tool
  3d_rendering_processing/tools/kanim_to_interpolators (but it's still
  not automatic for non-trivial models).

  We plan to extend Blender exporter to overcome this limitation.
  For now, use KAnim approach, which isn't so terrible actually :)

- Other modellers may allow you to export X3D file with animations
  (I know that at least 3DS Max can export some animations to X3D),
  so method 2. or even 1. may be possible with other modellers.

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
