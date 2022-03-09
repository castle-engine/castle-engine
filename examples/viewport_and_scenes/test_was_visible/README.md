# Test WasVisible method

This is a demo of `TCastleScene.WasVisible` and `TAbstractShapeNode.WasVisible` methods, that determine whether scene / shape were (potentially, at least partially) visible in the last rendering event. This allows to use the results of frustum culling and occlusion culling (if you activate occlusion culling, see https://castle-engine.io/occlusion_query ) for any custom need, e.g. to load some content dynamically.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `test_was_visible_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
