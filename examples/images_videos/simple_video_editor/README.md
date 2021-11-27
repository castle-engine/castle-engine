# Simple Video Editor

Simple example of loading a video file and playing it in Castle Game Engine
window, with some simple editing features.
Also handles normal image files, since we can load all image
formats to TVideo by just treating them as movies with 1 frame.

Beware! CGE `TVideo` is *not* suitable to load larger videos. It will take
a long time, and consume a lot of memory. It is only suitable for a short videos
used typically for visual effects.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
