# GLSL (OpenGL Shading Language) code for rendering scenes in Castle Game Engine

Everything rendered by `TCastleScene` within `TCastleViewport` is done by the shaders in this directory.

Extensions:
- `*.vs` - vertex shader
- `*.fs` - fragment shader
- `*.gs` - geometry shader
- `*.glsl` - shader code that may be placed in various shader stages. E.g. lighting calculation may be done in either vertex shader (for Gouraud shading) or in fragment shader (for Phong shading).

See https://castle-engine.io/shaders
about our shaders approach, which is used to "compose"
the final shader from multiple pieces.
This approach is used both internally (to construct the final shader from
various pieces defined in this directory) and externally (to allow user
to adjust shaders using `Effect` nodes).
The shader generation code is contained in CGE
`src/scene/castlerendererinternalshader.pas` unit.

See https://castle-engine.io/x3d_implementation_shaders.php
for various information about shader code in X3D.

## Meaning of various special comments in shader code

Various special comments looking like `/* FOO-BAR */` are used to mark places in shader code that are automatically processed.

### VARYING-PASSTHROUGH-GEOMETRY-SHADERS

The `/* VARYING-PASSTHROUGH-GEOMETRY-SHADERS */` comment indicates that the next line contains "varying" shader variable (output from vertex shader, input to fragment shader) that should be passed through geometry shaders without modification in simple cases.

It is only allowed in a vertex shader.

It is ignored if geometry shader is not present.

This means that

- `geometryVertexSet` will automatically pass this variable from vertex shader to fragment shader.

- `geometryVertexAdd` will automatically add this variable, scaled, to the output of the geometry shader.

- `geometryVertexZero` will automatically set this variable to zero in the output of the geometry shader.

Moreover, in fragment shader, we will automatically rename this variable to have the `_geoshader` suffix, since that's the name of the geometry shader output. We do this by adding `#define <name> <name>_geoshader` at the very beginning of the fragment shader.

Implementing this feature means that the next line must have a syntax

```
varying <type> <name>;
```

where `<type>` is `vecX`, `matX`, `float`. No extra comments are allowed between `/* VARYING-PASSTHROUGH-GEOMETRY-SHADERS */` and `;` ending the declaration (for now, our implementation would not be able to handle them). This is a bit more constrained than the general GLSL syntax.

The shader compilation will _fail_ if we cannot interpret the `varying` declaration.

Note that this marker should not be used with conditional compilation, inside GLSL `#if`. Our mechanism does not understand it (yet?), and would generate invalid code in such case. If you need this, then you will need to implement the logic of `/* VARYING-PASSTHROUGH-GEOMETRY-SHADERS */` a bit more manually, using `PLUG_geometry_vertex_set` and friends to define your variable only if it is present.
