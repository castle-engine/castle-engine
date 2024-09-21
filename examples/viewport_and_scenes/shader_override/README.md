# Override Shaders Demo

Set GLSL shader of a particular 3D object.
We do this by setting up nodes:

- TShaderPartNode (2x, once for vertex shader, once for fragment shader)
- TComposedShaderNode

See https://castle-engine.io/x3d_implementation_shaders.php about these nodes.

This approach completely overrides all Castle Game Engine rendering features,
you have to supply the shader code to do everything.

In most cases, you should rather prefer the approach using "composable shader effects"
demonstrated in ../shader_effects/ demo.
It uses the TEffectNode and TEffectPartNode
instead of TComposedShaderNode and TShaderPartNode.
See https://castle-engine.io/compositing_shaders.php .

Press S to toggle usage of the custom shader.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `shader_override_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `shader_override_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
