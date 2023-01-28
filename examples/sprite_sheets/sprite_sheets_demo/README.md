# sprite_sheet_animation

Simple demo using sprite sheets.

This demo uses the most advised way to deal with sptite sheets:

- create/modify them using CGE sprite sheet editor

- save as a `.castle-sprite-sheet` file

- load to TCastleScene.

See [sprite sheets documentation](https://castle-engine.io/sprite_sheets).

# Alternatives

- You can use `TGLVideo2D` class to render a series of images,
  like in the "data/hero_animation/single_frames_60fps/*.png" directory here.
  Load a series of images using the `TVideo.Load` with URL
  `data/hero_animation/single_frames_60fps/hawaii_exo-walking_@counter(1).png` .
  Then create `TGLVideo2D` instance and render like
  `MyVideo.DrawableImageFromTime(...).Draw(...)`.

  But this method is *not* advised.
  Using the sprite sheet results in faster rendering, since it's a single image
  for GPU.

- You can design the 2D animation in Spine (or Dragon Bones) and export it to Spine JSON.
  Or you can design it in Blender, and export to glTF.

  In either case, you get a smooth animation, using bones,
  and can load it using TCastleScene.Load into Castle Game Engine.

  This is *the best approach for 2D animation*,
  that results in a perfectly smooth animation,
  without having a prerecorded number of images.
  But it requires preparing an animation as a "real" 2D or 3D animation format,
  not as a series of images.

- CGE also exposes `TSprite` class that allows to manually load and render
  sprite sheets. It is deprecated now.
