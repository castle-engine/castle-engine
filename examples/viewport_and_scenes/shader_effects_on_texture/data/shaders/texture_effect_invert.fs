/* Process texture color to produce a "color inversion" effect.
   See https://castle-engine.io/compositing_shaders_doc/html/
   about shader effects in Castle Game Engine.
   See https://castle-engine.io/compositing_shaders_doc/html/section.fragment_plugs.html
   for docs of PLUG_texture_color function, used here.
*/

void PLUG_texture_color(
  inout vec4 texture_color,
  const in sampler2D texture,
  const in vec4 tex_coord)
{
  texture_color.rgb = vec3(1.0) - texture_color.rgb;
}
