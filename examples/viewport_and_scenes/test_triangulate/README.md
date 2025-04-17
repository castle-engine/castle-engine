# Deconstruct scene into triangles

Deconstruct scene into triangles using `TCastleShape.Triangulate`.

It's not really a feature you will often need with CGE. But if you need to process model's triangles for any purpose (non-standard rendering, non-standard collisions etc.) then the triangles information is available.

Note that using `TCastleShape.GeometryArrays` is more efficient (it doesn't require to deconstruct everything into simple triangles, and things can be indexed) for rendering using modern GPU APIs. See `examples/research_special_rendering_methods/new_renderer_skeleton/new_renderer_skeleton.lpr` for more comments.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `build_3d_object_by_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
