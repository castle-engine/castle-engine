# Test WasVisible method

This is a demo of `TCastleScene.WasVisible` and `TAbstractShapeNode.WasVisible` methods, that determine whether scene / shape were (potentially, at least partially) visible in the last rendering event. This allows to use the results of frustum culling and occlusion culling (if you activate occlusion culling, see https://castle-engine.io/occlusion_culling ) for any custom need, e.g. to load some content dynamically.

Note: This is a rare example in 3D in which the order of transforms on `Viewport.Items` matters. Keep the `SceneTestBlocks` *after* the `Plane1`. Only then the shapes in `SceneTestBlocks` can be eliminated by occlusion query when `Plane1` obscures them. Usually, this is resolved within a single scene using `OcclusionSort`, but in this case (when one scene occludes another) for now you have to manually order them right. If you change the order, and render `SceneTestBlocks`, then occlusion query will never eliminate any shapes.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `test_was_visible_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
