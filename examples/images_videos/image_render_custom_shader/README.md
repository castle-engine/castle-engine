# Render image with TCastleImageControl using custom GLSL shader

Simple rendering using TCastleImageControl with a custom GLSL shader.

Note that an alternative way of using custom shaders is to use a scene (like TCastleScene and TCastleViewport with a rectangle, see https://castle-engine.io/x3d_implementation_geometry2d.php ). And then you can assign custom shaders using nodes, see the examples ../../viewport_and_scenes/shader_effects , ../../viewport_and_scenes/shader_override .

This method is limited to rendering 2D images. It uses TCastleImageControl.CustomShader, that under the hood uses TDrawableImage.CustomShader. It requires manually creating TGLSLProgram instance and linking it.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
