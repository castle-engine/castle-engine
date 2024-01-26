# Detect Scene Hits

Detect what is the `TCastleScene` under mouse / touch position.

Uses `TCastleViewport.TransformUnderMouse` to detect clicks on a particular scene.

Note that you could also look at `TCastleViewport.TriangleHit` for details of the triangle under mouse. You can also look at each scene properties:

- `TCastleScene.PointingDeviceOverItem: PTriangle;`
- `TCastleScene.PointingDeviceOverPoint: TVector3;`
- `TCastleScene.PointingDeviceActive: Boolean;`

You can also take a look at `TCastleViewport.MouseRayHit`, which contains all information. The `TCastleViewport.TransformUnderMouse` and `TCastleViewport.TriangleHit` and in fact just comfortable shortcuts that access `TCastleViewport.MouseRayHit` underneath.

Alternative way to capture clicks is to use `TouchSensor` X3D node (not shown in this example).

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `detect_scene_hit_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `detect_scene_hit_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
