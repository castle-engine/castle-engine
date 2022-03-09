GLSL (OpenGL Shading Language) code for rendering scenes in Castle Game Engine.
Everything rendered by TCastleScene within TCastleViewport is done by these shaders.

Extensions:
- *.vs - vertex shader
- *.fs - fragment shader
- *.gs - geometry shader
- *.glsl - shader code that may be placed in various shader stages
  E.g. lighting calculation may be done in either vertex shader
  (for Gouraud shading) or in fragment shader (for Phong shading).

See https://castle-engine.io/compositing_shaders.php
for our "compositing shaders" approach, which is used to "compose"
the final shader from multiple pieces.
This approach is used both internally (to construct the final shader from
various pieces defined in this directory) and externally (to allow user
to adjust shaders using "Effect" nodes).
The shader generation code is contained in CGE
src/x3d/opengl/castlerendererinternalshader.pas unit.

See https://castle-engine.io/x3d_implementation_shaders.php
for various information about shader code in X3D.
