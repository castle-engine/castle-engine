/* Process texture color to produce a sepia effect.
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
  texture_color.rgb = vec3(
    texture_color.r * 0.393 + texture_color.g * 0.769 + texture_color.b * 0.189,
    texture_color.r * 0.349 + texture_color.g * 0.686 + texture_color.b * 0.168,
    texture_color.r * 0.272 + texture_color.g * 0.534 + texture_color.b * 0.131
  );
}
