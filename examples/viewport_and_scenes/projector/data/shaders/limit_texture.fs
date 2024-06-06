/* Limit the texture display to the place where its coordinates are in 0..1 range.

   Pretend the color is 1,1,1,1 (opaque white) for the rest of the texture,
   so mixing it with existing color calculated by lighting will do nothing.

   See Pascal code of AddEffectToLimitTexture for more comments why this is
   necessary.

   For information about PLUG_ in shaders, see
   https://castle-engine.io/compositing_shaders.php
   and in particular PLUG_texture_color is documented at
   https://castle-engine.io/compositing_shaders_doc/html/section.fragment_plugs.html
*/

void PLUG_texture_color(inout vec4 texture_color,
  const in sampler2D texture,
  const in vec4 tex_coord)
{
  vec2 tex_coord_2d = tex_coord.xy / tex_coord.w;
  if (tex_coord_2d.x < 0.0 ||
      tex_coord_2d.x > 1.0 ||
      tex_coord_2d.y < 0.0 ||
      tex_coord_2d.y > 1.0) {
    texture_color = vec4(1.0, 1.0, 1.0, 1.0);
  }
}
