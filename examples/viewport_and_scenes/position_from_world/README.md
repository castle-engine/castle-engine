# PositionFromWorld demo

Demo of using TCastleViewport.PositionFromWorld to map 3D position (inside the world defined by TCastleViewport.Items) into 2D position (treating the viewport as 2D user interface element). This allows to e.g.

1. Place a UI element exactly where some 3D thing is. This example shows how to position a TCastleLabel so that is always remains at the TCastleSphere middle, regardless of how you move and zoom the camera.

2. Transform 3D elements to make them look in 2D in some desired way. For example, you can adjust 3D scale, to force some 3D object to have certain size on the screen, regardless of how far is it from the camera.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `position_from_world_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `position_from_world_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
