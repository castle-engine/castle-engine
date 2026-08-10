# Using OpenGL(ES) transform feedback

Demo of using `TGLSLProgram.SetTransformFeedbackVaryings`.

This allows to store the data computed by the vertex shader in VBO, and use it for subsequent rendering. Thus GPU can be used to *update* a piece of data, each frame, and the data can remain on GPU.

See https://open.gl/feedback , https://www.khronos.org/opengl/wiki/Transform_Feedback .

This can, and is, used to implement particle systems: see https://castle-engine.io/additional_components.php , https://github.com/Kagamma/cge-3d-particle-emitter .

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `cars_demo_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
