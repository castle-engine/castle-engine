# Test TCastleImage.Draw3x3

Test `TCastleImage.Draw3x3` method, to draw one image on another using 9-slices
algorithm.

Note that you should prefer to use TDrawableImage.Draw3x3 in most games
(it's done on GPU and is much faster), see e.g. draw_images_on_gpu.lpr
for an example how to draw on TDrawableImage.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
