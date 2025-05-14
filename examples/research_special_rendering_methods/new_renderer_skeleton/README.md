# Skeleton code to start a new renderer (Vulkan, BGFX...)

We plan to add more "renderers" to the engine, to render using:

- [BGFX](https://castle-engine.io/roadmap#bgfx) (and thus, underneath, Vulkan, Metal and more)
- direct [Vulkan](https://castle-engine.io/roadmap#vulkan)
- and possibly more, depending on our resources and needs.

This application is a starting point to experiment with adding a new renderer to the engine.

See the news post: https://castle-engine.io/wp/2017/11/11/skeleton-code-to-start-a-new-renderer-metal-vulkan-direct3d-opengles-improvements/

We plan to introduce an abstraction layer to define "a renderer backend" in the engine. Until then, this is a good starting point to experiment with a new renderer.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `new_renderer_skeleton.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `new_renderer_skeleton.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
