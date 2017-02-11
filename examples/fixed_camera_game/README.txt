"The Rift"

------------------------------------------------------------------------------
What is it?

"The Rift" is a demo how to make a game with fixed camera/background
mixed with animated 3D, using the Castle Game Engine.
This is not really a playable game (although it may inspire you to make one!).

"Fixed camera" means that the location view (like the room etc.)
is still, but the 3D characters (player, NPCs, items) are moving freely.
Example games using this view: "Alone In The Dark", "Syberia" and "Still Life".
There are some advantages of this view:

- Your background is simply a 2D art. If you're a skilled 2D artist,
  you can create really beautiful backgrounds, depicting fantastic
  things with incredible detail. You can help yourself with any 2D graphic
  effects, and even with photographs. You can render the image with
  any advanced global-illumination method (ray-tracers etc.).

  This allows you to show things that would be impossible (or incredibly hard)
  to accurately model in 3D (for normal 3D game, like FPS).

  (Actually, the 2D background needs to have a matching skeleton 3D model.
  This is needed to to know the depths, necessary to correctly mix 3D characters
  and dynamic shadows with the background.
  But this 3D model is never directly visible to the user,
  so it can be really crude/simplistic, so it should be quite easy to create it.

- Since you know where the camera is, you can stage many cinematic effects.
  The game may feel more like a movie with suggestive camera angles.

For these reasons, this view is very nice for adventure / horror games,
where the emphasis is on giving the player nice view of the location.
It's probably not good for shooters and such (aiming may be awkward,
as the camera is still --- it doesn't match your hero orientation).

------------------------------------------------------------------------------
How does it work (and some unrealized plans for the future):

I wanted to make a nice small playable game from this, although admittedly
I didn't get there. There is an intro at the beginning (suggesting a storyline
that isn't really implemented), using some photos from http://www.flickr.com/
(licensed on suitable Creative Commons licenses).
Some 2D graphics and 3D modelling was made by me (Michalis).

Some of the content is configurable by simply editing the data files,
but some of the stuff is hardcoded in the source code.
I deliberately wanted to see first "what works" before spending too much
time on making everything configurable by content creators.

Some design decisions:

- Each location has a prepared 2D image, that's an obvious start.
  This image can be prepared in any way --- you can render it from 3d model
  (and you can render it using any rendered, e.g. blender's advanced renderer),
  you can draw it, process it with GIMP/whatever however you like.
  This is the main idea behind such games: 2d graphic artist can really
  show off.

- "3d being" is something that has to be prepared and rendered as 3d.
  These are things that move freely on the scene, so they simply have
  to be prepared as 3d. These are: player, NPCs, other parts of the location
  that have to be 3d (because e.g. they move --- like a shutter
  moved by the wind). Some NPCs and "other parts" may be made as
  2d graphic animations if their move is constrained enough,
  but usually we'll not be able to make it.

  Most (all, for now ?) 3d beings should be usable to cast shadows,
  so they must be a manifolds, or almost manifolds, see
  [http://castle-engine.sourceforge.net/castle-development.php#section_creatures]
  for an overview what this means and how to check it.

- We need to have a "3d skeleton" for each location. This will probably
  be one of the painful things to make, hopefully it'll turn out
  to be easy. I'm calling this "3d skeleton" because it will *not*
  be rendered for user anywhere. However, it will be rendered into
  the depth buffer. This skeleton may be sometimes painful to make,
  but it's simply absolutely necessary for

  a) Rendering models of 3d beings appropriately tested with depth buffer.
  b) Collision detection with player.
  c) Casting shadows (the location casts shadows over the 3d beings).

  I may also allow authors to just prepare "depth buffer" images
  instead of skeleton_3d. You should also be able to render
  such "depth buffer" image from the game, to later retouch it.
  But note that such "prepared depth buffer image" will be only
  useful for the a) part --- b) and c) still need normal geometry
  to operate.

- Each location also needs lights (their positions/directions) for shadows
  to be calculated.

  The engine should allow these lights to change ?
  Yes, but beware: this means that static scene image may not include
  shadows cast by these dynamic lights.

- Also we need a darker image of the location, that will be used
  in places where shadows from 3d beings fall on the location.

- Should location image itself already include self-shadows caused by
  this location ? Generally yes, since the artist may want to draw
  these shadows to give them more "artistic feeling" in some cases.

  But we're also able to automatically compute such self-shadows.
  In particular, this is needed if lights may be dynamic.

- Summing up what we already have:

  location has:
    image (image filename, will be scaled if needed to screen size)
    shadow_image (image filename, will be scaled if needed to screen size)
    skeleton_3d (3D model filename)
    lights (3D file containing only light nodes; I'll prepare in-game
      editor for this, you can also use 3d modelling program like
      blender to make this, since it's just a VRML/X3D file).

    image_includes_shadow = FALSE | TRUE

      If imageIncludesShadow then the artist has already drawn the shadows
      inside referenced "image". This means that the light may
      not really change dynamically, unless we hope that "user will not
      notice these artifacts".

      If not imageIncludesShadow then we'll display the scene shadowed
      by it's own skeleton_3d.)

      Maybe lights should have separate imageIncludesShadow setting ?

  3d being has: well, 3d model filename.

  Shadow volumes strategy:
    draw location skeleton_3d -> depth buffer
      (must be first, before rendering 3d beings)
    draw location.image -> color buffer
    draw all 3d beings with lighting on -> both depth and color buffers

    fill stencil buffer by drawing shadow volumes from all 3d beings.
    Now we know where 3d beings cast shadows.

    if image_includes_shadow = TRUE then
      draw shadow_image
      where stencil buffer indicates shadow lies. Be careful --- render
      this image taking into account it's associated depth buffer
      (in case of equality --- allow to pass).

    fill stencil buffer by drawing skeleton_3d.
    Now we know where *either 3d beings or the scene* cast shadows.

    redraw all 3d beings with lighting off, only where stencil buffer
    indicates shadow lies. Test with depth buffer, but in case of equality ---
    allow to pass (since we want to draw over our own model).

    if image_includes_shadow = FALSE then you want to draw shadow_image
    now.

Some debug features that you may want to try to see how it all works inside:

- Read TCastleOnScreenMenu.DesignerMode help. Activated by --debug-menu-designer
  command-line option, and then press F12 when you see the main game menu.

- "Inspect creatures" : keys to walk: the same as view3dscene in
  Walk mode, see there for docs. Also:
  Escape exits.
  H (like "home", but home is already used) --- go back to initial camera view.
  Changing creature state (testing this was the primary reason for
  "Inspect creatures" mode): s(tand), b(ored), w(alk).

Locations features:
(TODO: move it to the game ?)

- home: note that the scene casts proper shadow on the player.
  In this scene this can be mostly visible when you're beside the chair,
  but it's generally done: every piece of the scene can cast shadows.

- Note that you may want to fill open windows/doors etc. with invisible
  polygon, so that mouse picking (to indicate walk target position)
  on them will work. This is decision for location creator, whether
  this works reasonable for user in given location.

Modelling creatures:
- see [http://castle-engine.sourceforge.net/castle-development.php]

- Note that humanoid walk animation is obviously poor (it seems like
  his torso moves too smooth). This is the fault of my poor animation
  skills, the code is Ok --- real creatures, done by real artists,
  will look better.

- Models should preferably be exported to the latest version of X3D,
  preferably using our custom Blender exporter on
  [http://castle-engine.sourceforge.net/blender.php].

  However, this demo was done when we used old VRML 2.0.
  As such, we made location and humanoid animations using our custom
  VRML 2.0 exporter.
  Humanoid creature as an example has a text data with simple script
  to export (just calls vrml97ExportFrame a couple of times).
  Exporting is simply calling this script (Alt + P in Blender
  when mouse over this script).

  The script automatically saves appropriate frames to appropriate
  VRML files. Generated VRML files have Blender modifiers (including
  Armature modifier, critical for animation) applied, so all is Ok.

Michalis Kamburelis
