# Viewer for Tiled maps

Use `TCastleTiledMapControl` control to load any Tiled map.

**Deprecated**: We recommend using `TCastleTiledMap` (see https://castle-engine.io/tiled_maps ) in a viewport now. See `examples/tiled/map_viewer_in_viewport` project.

You can

- open any Tiled map (`*.tmx` file),

- pan the map (drag with left mouse button pressed),

- zoom (use mouse wheel).

Our `data` subdirectory contains a number of Tiled maps. You can also go ahead and download [Tiled, free map editor](https://www.mapeditor.org/) to edit these maps or create new ones.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `map_viewer_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
