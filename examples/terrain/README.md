# Terrain generation and visualization

Terrain generation and visualization.

To use terrain in your own games:

- open CGE editor
- add `TCastleTerrain` to display a terrain
- add `TCastleTerrainNoise` to generate terrain heights
- set `TCastleTerrain.Data` to connect it with `TCastleTerrainNoise`.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `terrain_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
