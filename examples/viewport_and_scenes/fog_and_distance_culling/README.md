# Fog and Distance Culling

Demo of `TCastleScene.DistanceCulling`, especially useful when you have fog. We can avoid rendering objects that are too far to be visible (covered by fog). It's a useful optimization when you have a dense fog, and a background that has the same color as the fog.

This loads `data/fog_culling_final.x3dv` X3D file.

Press [Ctrl + F] to toggle DistanceCulling (and fog display) on/off.

Press [Ctrl + C] to toggle per-shape frustum culling on/off, ths also makes a difference in this demo.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `fog_and_distance_culling_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `fog_and_distance_culling_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
