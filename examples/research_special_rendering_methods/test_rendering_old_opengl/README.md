# Test Rendering With Various OpenGL(ES) Context Capabilities

This example sets `TGLFeatures.RequestCapabilities` and then tests a few standard engine rendering techniques, to make sure everything works at all OpenGL "capabilities" variants that we support.

## OpenGL(ES) Capabilities

- By default we set `TGLFeatures.RequestCapabilities := rcForceFixedFunction`, to force treating the current OpenGL version as if it was an ancient OpenGL version, without a lot of modern features (like VBO and shaders). Once such OpenGL context is initialized, it is guaranteed to have `GLFeatures.EnableFixedFunction = true`.

    Note that you should never use `TGLFeatures.RequestCapabilities := rcForceFixedFunction` in real applications. It is only for testing purposes. In real applications, always use (which happens by default) modern OpenGL functionality, as supported by almost all GPUs you can find nowadays. It is both more functional, and may even be faster than fixed-function.

    We do it here just a test that if CGE happens to run on a really ancient OpenGL (may be found in virtual machines), or we detect buggy OpenGL shaders support, then things still work reliably. Of course in such case there are limits, e.g. PBR or shadow maps will not work without shaders. Instead of PBR we will use Phong lighting model, and shadow maps will not be used.

    Note that fixed-function is only used for OpenGL rendering, on desktops. For OpenGLES, we require OpenGLES >= 2 where support for VBO and shaders is just mandatory (and this is present on all modern mobile devices).

- Run with command-line parameter `-render=force-modern` to set `TGLFeatures.RequestCapabilities := rcForceModern`.

    This is the other end of support spectrum. We make sure to initialize OpenGL 3.2 "core" profile, without any compatibility features enabled. It is guaranteed we will have `GLFeatures.EnableFixedFunction = false`, we will use shaders and VBO and VAO for everything.

    Moreover OpenGL "debug output" is enabled. The log will contain extra information from OpenGL -- potential problems, notifications.

- Run with command-line parameter `-render=automatic` to set `TGLFeatures.RequestCapabilities := rcAutomatic`.

    This is the default CGE behavior.

    We ask for context with latest OpenGL version, but also with compatibility API entry points. If everything goes well and we get a context with at least OpenGL 2.0, then we use shaders and modern features for everything (`GLFeatures.EnableFixedFunction` will be `false`). If we get ancient or buggy OpenGL version, we use `GLFeatures.EnableFixedFunction = true`. ( In the future it will be improved to ask for "core" profile by default, and only fallback on compatibility if necessary. )

## Rendering

This application exercises a few things in CGE that do rendering to make sure they all support various capabilities.

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
