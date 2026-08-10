uniform sampler2D testTexture;

varying vec4 my_vertex_object;

// Modify the fragment color by given texture.
// Use coordinates in object-space as texture coordinates
// (just for test).
void PLUG_fragment_modify(inout vec4 fragment_color)
{
  vec2 tex_coord = my_vertex_object.xy;
  fragment_color.rgb = texture2D(testTexture, tex_coord).rgb;
}
