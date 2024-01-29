# Tiling and moving background in Viewport

Example how to draw a tiling (repeated texture into infinity) and moving background
in a Viewport. It is efficient -- draws only 1 quad, depends on GPU to repeat the texture.
Defines a reusable class TTilingBackground.
Use arrows to modify the origin (shift the image).

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
