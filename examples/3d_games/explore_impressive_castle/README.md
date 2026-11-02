# Explore Impressive 3D Castle

- Walk in 3D exploring an impressive 3D castle.

    - Castle graphics custom made by real graphic artists (thank you to [Adrianna Matejek](https://github.com/Sherchloex) and [Alya Vaneskehyan](https://github.com/AlyaVanes)) for our engine. Put together using [Blender](https://castle-engine.io/blender) and [Castle Game Engine Editor](https://castle-engine.io/editor).

- Shoot at enemies.

  - Right now, only one knight guards the entrance.

  - The enemy logic is using [TCastleMoveAttack component](https://castle-engine.io/apidoc/html/CastleLivingBehaviors.TCastleMoveAttack.html).

- Full game controller support (joystick, gamepad) using trivial [TCastleWalkNavigation.UseGameController](https://castle-engine.io/apidoc/html/CastleCameras.TCastleWalkNavigation.html#UseGameController-Integer-).

    - See [documentation about implementing controller support](https://castle-engine.io/controllers) in our engine.

- An extra viewport in right-top corner of the screen shows a mini-map (view from the top). See [Multiple viewports to display one world](https://castle-engine.io/multiple_viewports_to_display_one_world) for a documentation how to set this up.

## Screenshots

![Screenshot working in editor](screenshot_editor.png)
![Screenshot 2](screenshot_2.png)
![Screenshot](screenshot.png)

## Performance of this demo

This demo is admittedly more performance-demanding than most other examples of _Castle Game Engine_.

### What features make this demo performance-demanding?

- It uses a lot of light sources, and they are all dynamic.

- We also use physical based lighting model, which is more realistic but a bit slower.

### How to make your own games fast

Every game has different system requirements, which are determined by the graphic features you use in the particular game (not just by the engine capabilities). Our _Castle Game Engine_ strives to support even very ancient systems, however you need to take some limits into account if you want to target low-end machines. If this is the case, be sure to read:

- [Our guidlines how to optimize your game](https://castle-engine.io/manual_optimization.php).

- If you really want to support extremely low-end machines, then be careful when using [some of the features that make rendering prettier](https://castle-engine.io/how_to_make_rendering_prettier). Some of them (though not all) have a performance cost. Again, be sure to test (don't guess :) ), every case is different.

### Particular advises to make a demo like this (3D level and many lights) run faster

1. For light sources, you can use less light sources and/or bake light sources. The information [how to use lights baking in Blender is here](https://castle-engine.io/blender#_baking_lighting).

    Right now we don't support doing such _baking_ straight from Castle Game Engine editor (unlike e.g. _Unreal Engine_ which has this built-in) so you need to use [Blender](https://castle-engine.io/blender) (or other 3D authoring software) for this and export to Castle Game Engine a model with already baked lighting.

    If this sounds complicated, then simply limit your lighting usage, or even use vertex colors.

2. Consider switching from physical-based lighting model to Phong lighting model. How to do this depends on your models, but usually (if you use [glTF format](https://castle-engine.io/gltf), which we recommend) you can load models with `TCastleSceneLoadOptions.GltfPhongMaterials` set to `true`.

    Do this early -- e.g. in the `AppplicationInitialize` in the unit `GameInitialize`.

3. As a brute-force solution (that makes point 2. above completely unnecessary and makes 1. less necessary) you can also do

    ```
    TGLFeatures.RequestCapabilities := rcForceFixedFunction;
    ```

    This is somewhat brute-force solution, it makes a few things uglier, but it makes sense if you really want to target low-old GPUs. You can add this line to the `AppplicationInitialize` *or* you can choose in CGE editor menu item _"Run -> Run Parameters -> Force Ancient Rendering"_ and run the game. You will see the game looks worse -- but is also much faster.

## Inputs

Attack (with currently equipped weapon) by clicking with left mouse button.

Move and rotate:

- use AWSD or arrow keys,
- look around using _"Mouse Look"_ (move the mouse when in game),
- hold Shift to run.

Misc:

- screenshot: F5.
- fake win: P,
- fake death: O.

Game controller inputs are documented at [TCastleWalkNavigation.UseGameController](https://castle-engine.io/apidoc/html/CastleCameras.TCastleWalkNavigation.html#UseGameController-Integer-).

A lot more keys and mouse shortcuts are instantly working
(and they all are configurable, too). See the [TCastleWalkNavigation](https://castle-engine.io/apidoc/html/CastleCameras.TCastleWalkNavigation.html) features.

## Building

Using [Castle Game Engine](https://castle-engine.io/).

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `fps_game.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `fps_game.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
