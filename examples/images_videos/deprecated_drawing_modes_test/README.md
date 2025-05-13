# Test TCastleImage.DrawFrom

Test drawing one TCastleImage on another (using TCastleImage.DrawFrom)
with various blending modes.

**This approach to draw image-on-image is deprecated.** It is faster, and more feature-packed, to draw on images using `TDrawableImage` API. For demos, see:

- ../draw_images_on_gpu/
- ../image_generate_and_use/

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
