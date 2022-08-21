/* OpenGL shader effect (used to enhance the Castle Game Engine shaders,
   see https://castle-engine.io/compositing_shaders.php ),
   applied over terrain.

   This adjusts terrain color, mixing textures, based on current height. */

uniform sampler2D tex_1;
uniform sampler2D tex_2;
uniform sampler2D tex_3;
uniform sampler2D tex_4;

uniform vec3 color_1;
uniform vec3 color_2;
uniform vec3 color_3;
uniform vec3 color_4;

uniform float uv_scale_1;
uniform float uv_scale_2;
uniform float uv_scale_3;
uniform float uv_scale_4;

uniform float height_1;
uniform float height_2;

uniform float layers_influence;
uniform float steep_emphasize;

varying vec3 terrain_position;
varying vec3 terrain_normal;

// avoid redeclaring when no "separate compilation units" available (OpenGLES)
#ifndef GL_ES
vec4 castle_texture_color_to_linear(const in vec4 srgbIn);
#endif

void PLUG_main_texture_apply(inout vec4 fragment_color, const in vec3 normal)
{
  float h = terrain_position.y;

  /* We flip terrain_position.z, to map texture more naturally, when viewed from above.
     This is consistent with calculating TexCoord for TCastleTerrainData.Height.
     We just flip the sign, because the terrain textures always have repeat = true,
     so there's no need to shift the texture in any way.
  */
  vec2 uv = vec2(terrain_position.x, -terrain_position.z);

  /* What does this mean?
     normal_slope (normal.y)
     = 0 means a vertical face
     = 1 means a horizontal face
  */
  float normal_slope = normalize(terrain_normal).y;

  vec3 c1 = color_1 * castle_texture_color_to_linear(texture2D(tex_1, uv * uv_scale_1)).rgb;
  vec3 c2 = color_2 * castle_texture_color_to_linear(texture2D(tex_2, uv * uv_scale_2)).rgb;
  vec3 c3 = color_3 * castle_texture_color_to_linear(texture2D(tex_3, uv * uv_scale_3)).rgb;
  vec3 c4 = color_4 * castle_texture_color_to_linear(texture2D(tex_4, uv * uv_scale_4)).rgb;

  float height_mix = smoothstep(height_1, height_2, h);
  vec3 flat_color = mix(c1, c3, height_mix);
  vec3 steep_color = mix(c2, c4, height_mix);
  vec3 modified_color = mix(steep_color, flat_color, pow(normal_slope, steep_emphasize));

  fragment_color.rgb = mix(fragment_color.rgb, modified_color, layers_influence);
}
