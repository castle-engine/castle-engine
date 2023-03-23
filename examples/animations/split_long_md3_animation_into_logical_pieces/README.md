# Demo of using one long MD3 animation split into a few logical pieces

In case of some 3D authoring software and formats, it may happen that we must have one, long animation that actually contains a few "logical" animations glued together.

This in particular happens when using MD3, because MD3 format just doesn't allow to define multiple animations. They are in some cases defined in separate `animation.cfg` file (simple text file placed alongside the .md3 file) but CGE by itself does not read this `animation.cfg` file (at least now).

This example shows a creature from [Tremulous](https://tremulous.net/) that in MD3 has just one animation. When loading such model as `TCastleScene`, it has just one animation called `animation` (this is `TNodeInterpolator.DefaultAnimationName`). Playing it as a whole (`MyScene.PlayAnimation('animation')`) is possible... but not very useful. You actually want to play subranges of this animation.

So the example code also loads and parses accompanying `animation.cfg` file, and it displays all animations, and allows to play them. Internally the playback just means to use `TCastleSceneCore.ForceAnimationPose` each frame.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `split_long_md3_animation_into_logical_pieces_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
