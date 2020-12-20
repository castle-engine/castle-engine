This is a demo of using `TSprite` class from Castle Game Engine
to display an animation.

**This approach to handling sprite sheets (using TSprite) is deprecated now.
We instead advise to [load sprite sheets to TCastleScene](https://github.com/castle-engine/castle-engine/wiki/Sprite-sheets).**

Sprite is an image that contains a series of animation frames,
like "data/hero_animation/hero_sprite_sheet_60fps.png" here.
You load such animation to TSprite, and render it using TSprite.Draw,
e.g. in TCastleWindow.OnRender event.

There are many tools to create such sprite sheet image.
For this demo, I exported an animation from Spine to a series of PNG images
(visible in "data/hero_animation/single_frames_60fps/*.png"),
and then I combined these into one big image using the tool
"examples/sprite_sheets/combine_images_into_sprite_sheet" from Castle Game Engine.

# Alternatives

- Express spite sheet animations in Starling or Cocos2d format,
  and then load, display and animate them using TCastleScene.
  See https://github.com/castle-engine/castle-engine/wiki/Sprite-sheets .
  See https://github.com/castle-engine/castle-engine/wiki/2D-Games
  for a discussion and comparison of these 2 approaches.

- You can also use `TGLVideo2D` class to render a series of images,
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
