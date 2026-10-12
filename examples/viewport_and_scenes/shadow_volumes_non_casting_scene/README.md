# Shadow volumes: preparing a large scene that doesn't cast shadows

Loads a large "level" into a viewport that uses [shadow volumes](https://castle-engine.io/shadow_volumes). The level doesn't cast shadows (`CastShadows` = `false`). The example measures how long `MainViewport.PrepareResources` takes for the level.

- The design (`data/gameviewmain.castle-user-interface`) has a point light with `Shadows` = `true`, a box that casts a shadow, and an empty `SceneLevel` with `CastShadows` = `false`.

- The code (`code/gameviewmain.pas`) loads into `SceneLevel` a floor of 100 x 100 separate tiles (20 000 triangles) with gaps between them. Like typical level geometry, it is not 2-manifold: no edge is shared by two triangles, so every edge is a _border edge_.

- The label shows how long preparing the level took, and for how many of its shapes the shadow volume edges were calculated.

Keys:

- _C_ toggles the level's `CastShadows` and loads it again.
- _+_ / _-_ change the number of tiles.
- _R_ loads the level again.

Preparing a scene for shadow volumes (`prShadowVolume`) calculates the _manifold and border edges_ of every shape. Each new edge is compared with all the border edges found so far, so this is quadratic in the number of border edges. For a mesh like this level it takes seconds:

| Level | `CastShadows` = `false` | `CastShadows` = `true` |
| --- | --- | --- |
| 100 x 100 tiles (20 000 triangles) | 0.03 s | 1.6 s |
| 150 x 150 tiles (45 000 triangles) | 0.02 s | 13 s |

A scene that doesn't cast shadows doesn't need these edges, so `TCastleSceneCore.PrepareResources` only calculates them when `CastShadows` is `true`. Before that, the times in the `false` column were the same as in the `true` column (2.2 s and 14.4 s).

The times were measured on Windows (x86_64), FPC 3.2.2, release build. Your times will differ, but the difference between the two columns should be similar.

![Screenshot](screenshot.png)

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `shadow_volumes_non_casting_scene_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `shadow_volumes_non_casting_scene_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
