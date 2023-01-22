# View Tiled map as TCastleScene in TCastleViewport

Load and view any Tiled map using `TCastleScene` in `TCastleViewport`.

You can

- open any Tiled map (`*.tmx` file),

- pan the map (drag with left mouse button pressed, this is working thanks to standard `TCastle2DNavigation`),

- zoom (use mouse wheel, this is working thanks to standard `TCastle2DNavigation`).

Our `data` subdirectory contains a number of Tiled maps. You can also go ahead and download [Tiled, free map editor](https://www.mapeditor.org/) to edit these maps or create new ones.

Note about using `TCastleScene` for Tiled:

- It works most sensible in orthographic view with camera direction along Z. Otherwise the distance in Z between each layer may be visible.

- The best `BlendingSort` is `bsNone`, not `bs3D` or `bs2D`.

    IOW the tiles are already provided in the correct rendering order, blending sorting should not mess with it. Testcase: `data/maps/isometric_grass_and_water.tmx` .

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `map_viewer_in_viewport_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
