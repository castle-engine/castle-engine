# Test Rendering With Old OpenGL

This example uses `TGLFeatures.RequestCapabilities := rcForceFixedFunction`, to force treating the current OpenGL version as if it was an ancient OpenGL version, without some modern features (like VBO and shaders). Once OpenGL context is initialized, it is guaranteed it will have `GLFeatures.EnableFixedFunction = true`.

Then it exercises a few things in CGE that do rendering to make sure they all support `GLFeatures.EnableFixedFunction = true` case nicely, and thus will work even on ancient systems with old OpenGL. Note that CGE also uses this mode for GPUs we detect as "too buggy" with regards to their VBO or shaders support. So it is important that this case works reliably, even though it should not be used on new systems with modern GPU.

Note that fixed-function is only used for OpenGL rendering, on desktops. For OpenGLES, we require OpenGLES >= 2 where support for VBO and shaders is just mandatory (and this is present on all modern mobile devices).

The rendering methods tested:

- `TCastleScene`

- `TDrawableImage`

- `DrawRectangle` (doing `DrawPrimitive2D` under the hood)

- `TCastleFont.Print` (uses `TDrawableImage` under the hood)

- `TCastleRenderUnlitMesh` (internal CGE utility in `CastleInternalGLRenderer` right now)

Everything else in CGE is in some way rendered on top of above methods.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `test_rendering_old_opengl_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
