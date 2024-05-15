# Simple Video Editor

Simple example of loading a video file and playing it in Castle Game Engine
window, with some simple editing features.
Can load:

- image sequence.

    By default loads image sequence in `data/flame_seamless/0001.png`, `data/flame_seamless/0002.png`...
    It can be referred to using CGE URL like this: `castle-data/flame_seamless/@counter(4).png` .

- some video formats, if FfMpeg is installed and available on command-line.

    Beware! CGE `TVideo` is *not* suitable to load larger videos. It will take
    a long time, and consume a lot of memory. It is only suitable for a short videos
    used typically for visual effects.

- can also load single image files.

    We can load all image formats to TVideo by just treating them as movies with 1 frame.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
