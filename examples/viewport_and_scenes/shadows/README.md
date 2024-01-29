# Shadows

Simple demo of shadows:

- Every light type (point, spot, directional) has a boolean `Shadows` property,

- Each `TCastleTransforem` has `CastShadows` property,

- For shadow volumes, all objects (primitives and meshes loaded by `TCastleScene`) are potential shadow casters, as long as they are 2-manifold.

![Screenshot](screenshot.png)

![Screenshot from editor](screenshot_editor.png)

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `shadows_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `shadows_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
