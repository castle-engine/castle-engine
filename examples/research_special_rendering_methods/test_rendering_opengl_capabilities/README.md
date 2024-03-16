# Test Rendering With Various OpenGL(ES) Capabilities

This example tests a few standard engine rendering techniques, to make sure everything works at all OpenGL "capabilities" that we support.

The intended usage is to pass on command-line standard option `--capabilities=...`, handled by `CastleWindow` unit (`TCastleApplication.ParseStandardParameters`), to request specific capability. Observe the result.

## OpenGL(ES) Capabilities

- _Automatic_:

    This is the default CGE behavior. You can run with command-line parameter `--capabilities=automatic` to request it explicitly, but it is default now anyway.

    Corresponds to `TGLFeatures.RequestCapabilities := rcAutomatic`.

    We ask for context with latest OpenGL version, but also with compatibility API entry points. If everything goes well and we get a context with at least OpenGL 2.0, then we use shaders and modern features for everything (`GLFeatures.EnableFixedFunction` will be `false`). If we get ancient or buggy OpenGL version, we use `GLFeatures.EnableFixedFunction = true`.

    TODO: In the future we will ask for "core" profile by default, and only fallback on compatibility if necessary.

- _Force Modern OpenGL(ES) usage_:

    Run with command-line option `--capabilities=force-modern`. This sets `TGLFeatures.RequestCapabilities := rcForceModern`.

    In this mode we make sure to initialize OpenGL 3.3 "core" profile (see TGLFeatures.ModernVersionMajor, TGLFeatures.ModernVersionMinor), _without any compatibility features enabled_. It is guaranteed we will have `GLFeatures.EnableFixedFunction = false`. We will use shaders, VBO, VAO, texture swizzle and more modern features for everything.

    Moreover, in this application, OpenGL "debug" is enabled. The log will contain extra information from OpenGL. And eventual OpenGL problems will be reported as exceptions right at the command that caused the problem.

- _Force Ancient Force-function Usage_:

    Run with command-line option `--capabilities=force-fixed-function`. This sets `TGLFeatures.RequestCapabilities := rcForceFixedFunction`.

    In effect we force treating the current OpenGL version as if it was an ancient OpenGL version, without any modern features (like VBO and shaders). Once such OpenGL context is initialized, it is guaranteed to have `GLFeatures.EnableFixedFunction = true`.

    Note that this is only for testing purposes. In real usage, always use (which happens by default) modern OpenGL functionality, as supported by almost all GPUs you can find nowadays. Modern API is both more functional and faster than fixed-function.

    Testing this is useful to make sure we run OK on a really ancient OpenGL (may be found in virtual machines), or we detect buggy OpenGL shaders support, then things still work reliably. Of course in such case there are limits, e.g. PBR or shadow maps will not work without shaders. Instead of PBR we will use Phong lighting model, and shadow maps will not be used.

    Note that fixed-function is only used for OpenGL rendering, on desktops. For OpenGLES, we require OpenGLES >= 2 where support for VBO and shaders is just mandatory (and this is present on all modern mobile devices).

## Rendering

This application exercises a few things in CGE that do rendering to make sure they all support various capabilities.

The rendering methods tested:

- `TCastleScene` (including testing shadow maps and shadow volumes).

- `TDrawableImage`

- `DrawRectangle` (doing `DrawPrimitive2D` under the hood)

- `TCastleFont.Print` (uses `TDrawableImage` under the hood)

- `TCastleRenderUnlitMesh`

- Screen (post-processing) effects (by `TCastleScreenEffect.AddScreenEffect`).

Everything else in CGE is in some way rendered on top of above methods.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `test_rendering_opengl_capabilities_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `test_rendering_opengl_capabilities_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
