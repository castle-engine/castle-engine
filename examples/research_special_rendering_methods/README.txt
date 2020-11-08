The demos in this directory explore some special rendering methods
to achieve dynamic shadows and mirrors. While they are quite impressive,
they require some low-level OpenGL trickery. Some of them are also quite
expensive, even on modern GPUs.

Consider this a "research" area. If some rendering method here will prove
worthy (fast, practical, impressive), it may be integrated into
the core engine one day (and then using it in all your applications
will be trivial).

If you're looking for *easy and practical* methods to achieve
shadows and mirrors *now*, consider the algorithms already integrated
with our engine core:

- Shadow maps:
  https://castle-engine.io/x3d_extensions_shadow_maps.php
- Shadow volumes:
  https://castle-engine.io/x3d_extensions.php#section_ext_shadows
- Mirrors on flat objects:
  https://castle-engine.io/x3d_extensions_mirror_plane.php
- Cube map reflections:
  https://castle-engine.io/x3d_implementation_cubemaptexturing.php
- Reflections using a RenderedTexture:
  https://castle-engine.io/x3d_implementation_texturing_extensions.php#section_ext_rendered_texture
- Screen-space reflections:
  https://castle-engine.io/wp/2020/11/07/screen-space-reflections-effect-enhancements-to-glsl-api-for-effects/
