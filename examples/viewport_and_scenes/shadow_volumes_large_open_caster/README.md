# Shadow volumes: loading a large mesh that is not 2-manifold

Loads a large mesh that is not 2-manifold into a viewport that uses [shadow volumes](https://castle-engine.io/shadow_volumes), with `CastShadows` = `true` (the default for all scenes). The example measures how long `MainViewport.PrepareResources` takes for it.

- The design (`data/gameviewmain.castle-user-interface`) has a point light with `Shadows` = `true`, a ground plane, a box casting a shadow and an empty `SceneWall`.

- The code (`code/gameviewmain.pas`) loads into `SceneWall` a wall of 100 x 100 separate tiles (20 000 triangles) with gaps between them. Like typical level geometry, it is not 2-manifold: each tile's diagonal is a _manifold edge_ (between 2 triangles), all other edges are _border edges_ (1 triangle).

- The label shows how long preparing the wall took and its numbers of manifold and border edges.

Keys:

- _+_ / _-_ change the number of tiles.
- _R_ loads the wall again.

Preparing a scene for shadow volumes calculates the manifold and border edges of every shape. The engine needs them even for a shape that doesn't end up casting a shadow. A shape with border edges doesn't render shadow volumes (unless `RenderOptions.WholeSceneManifold`), and its border edges are how the engine knows that. So this cost is paid for any large level geometry loaded with the default `CastShadows` = `true`.

Edges are paired up by looking up each new edge's counterpart in a hash map, which takes linear time. Before, each new edge was compared with all border edges found so far, which was quadratic in their number:

| Wall | Before | After |
| --- | --- | --- |
| 100 x 100 tiles (20 000 triangles, 40 000 border edges) | 1.81 s | 0.05 s |
| 150 x 150 tiles (45 000 triangles, 90 000 border edges) | 10.23 s | 0.08 s |
| 300 x 300 tiles (180 000 triangles, 360 000 border edges) | (minutes) | 0.34 s |

The times were measured on Windows (x86_64), FPC 3.2.2, release build. They include the rest of `PrepareResources` (like preparing the mesh for rendering). Your times will differ, but the difference should be similar.

![Screenshot](screenshot.png)

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `shadow_volumes_large_open_caster_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `shadow_volumes_large_open_caster_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
