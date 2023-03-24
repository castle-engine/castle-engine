# Demo of using MD3 with long animation split into a few logical pieces (subanimations)

In case of some 3D authoring software and formats, it happens that we have one, long animation that contains a few logical animations ("subanimations") glued together.

This in particular happens when using MD3, because MD3 format just doesn't allow to define multiple animations. They are in some cases defined in separate `animation.cfg` file (simple text file placed alongside the .md3 file). CGE by itself does not read such `animation.cfg` file (at least now), but this example contains a simple reader for such files.

This example shows a creature from [Tremulous](https://tremulous.net/) that in MD3 has just one animation. When loading such model as `TCastleScene`, it has just one animation called `animation`. Playing it as a whole using `MyScene.PlayAnimation('animation')` is possible... but not very useful. You actually want to play subranges of this animation.

To this end, this example defines `TSceneSubAnimations` class, descendant of `TCastleScene`. This class:

- automatically parses `animation.cfg` placed alongside md3 file when you use `TSceneSubAnimations.Load`

- exposes information about subanimations in `TSceneSubAnimations.SubAnimations`

- allows to play the subanimation using `TSceneSubAnimations.PlaySubAnimation`. Internally the playback uses `TCastleSceneCore.ForceAnimationPose` to update each frame.

The example application uses this class, displays all animations, and allows to play them by clicking on appropriate buttons.

![Screenshot](screenshot.png)

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `split_long_md3_animation_into_logical_pieces_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
