The demos in this directory explore special rendering methods.

These demos generally use low-level graphic calls (directly to OpenGL(ES)). We don't advise adopting these techniques unless you're already familiar with both OpenGL(ES) and CGE.

These demos explore features that may result in new CGE features in the future. Consider this a "research" area. If some rendering method here will prove worthy (fast, reliable on all platforms and GPUs, impressive), it may be integrated into the core engine one day.

If you're looking for *easy and practical* methods to achieve various graphic effects *now*, consider the algorithms already integrated with our engine core:

- Shadow maps: https://castle-engine.io/x3d_extensions_shadow_maps.php
- Shadow volumes:  https://castle-engine.io/x3d_extensions.php#section_ext_shadows
- Mirrors on flat objects: https://castle-engine.io/x3d_extensions_mirror_plane.php
- Cube map reflections: https://castle-engine.io/x3d_implementation_cubemaptexturing.php
- Reflections using a RenderedTexture: https://castle-engine.io/x3d_implementation_texturing_extensions.php#section_ext_rendered_texture
- Screen-space reflections: https://castle-engine.io/wp/2020/11/07/screen-space-reflections-effect-enhancements-to-glsl-api-for-effects/
