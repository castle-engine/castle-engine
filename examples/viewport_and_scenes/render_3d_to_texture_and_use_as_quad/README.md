# Render 3D scene to a texture, and use this texture to display another 3D scene

- Use `TGLRenderToTexture` to render a 3D scene (`SourceScene`) to a texture.
- Then grab this texture to a normal memory (as TRGBImage).
- Use it as a texture for a quad inside an interactive 3D scene (RuntimeScene).

Usage: Run this, and press an arrow key or drag with mouse -- to confirm that what you see is just a quad with a texture:) Change the line `SourceScene.Load` to set the source 3D scene visible on the texture.

You could also save the texture grabbed this way to a file. Or really do anything with it --- you can process the TRGBImage instance in any way. See example `../render_3d_to_image`.

TODO:

- This could be done much more efficiently, without copying the image from GPU to RAM (normal memory). TGLRenderToTexture can render to an OpenGL texture resource (`TGLTextureId`), but right now TPixelTextureNode / TImageTextureNode cannot be initialized from such a TGLTextureId.

    To put it in different words, our usage of TGLRenderToTexture in this demo is unoptimal. We use `TGLRenderToTexture` to render off-screen, and then we grab the texture from FBO using the unoptimal `SaveScreen_NoFlush`. Using the `TGLRenderToTexture` to render straight to `TGLRenderToTexture.Texture` would be much better.

    Note: The faster approach would also require extra work on Android, or other platforms where you can lose OpenGLES context essentially at any moment. You would need to rerender such textures in this case, when the OpenGLES context returns.

    Note: You can already use TRenderTextureNode to make this efficiently (without copying the texture data to normal memory), but TRenderTextureNode would be more tricky, it would require setting special camera views for RenderTexture. Right now, RenderTexture is most suited to grab to texture a view from the same scene, not to render one scene to be displayed as sprite on another scene.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `multiple_viewports_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
