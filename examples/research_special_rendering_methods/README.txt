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
  http://castle-engine.sourceforge.net/x3d_extensions_shadow_maps.php
- Shadow volumes:
  http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadows
- Cube map reflections:
  http://castle-engine.sourceforge.net/x3d_implementation_cubemaptexturing.php
- Reflections using a RenderedTexture:
  http://castle-engine.sourceforge.net/x3d_implementation_texturing_extensions.php#section_ext_rendered_texture
