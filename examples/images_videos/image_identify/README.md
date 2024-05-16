# Command-line tool to write basic information about the image

Given a list of image URLs or filenames (as parameters), load them, and write basic information about them.

- Image size,
- type,
- alpha channel (detailed analysis of alpha: yes/no or full range).

Can be used as a test of our CastleImages loading capabilities. Similar to ImageMagick `identify`.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
