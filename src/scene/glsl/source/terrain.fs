/* OpenGL shader effect (used to enhance the Castle Game Engine shaders,
   see https://castle-engine.io/compositing_shaders.php ),
   applied over terrain.

   This adjusts terrain color, mixing textures, based on current height. */

uniform sampler2D tex_1;
uniform sampler2D tex_2;
uniform sampler2D tex_3;

uniform vec3 color_1;
uniform vec3 color_2;
uniform vec3 color_3;

uniform float uv_scale_1;
uniform float uv_scale_2;
uniform float uv_scale_3;

uniform float normal_dark;
uniform float normal_darkening;
uniform float texture_mix;

uniform float h0; // below is tex/color_1
uniform float h1; // below is tex/color_2 mixed with tex/color_1
uniform float h2; // below is tex/color_2
uniform float h3; // below is tex/color_3 mixed with tex/color_2
                  // above is tex/color_3

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
  float normal_slope = normalize(terrain_normal).y;

  vec3 base_color;
  float hhalf = (h1 + h2) * 0.5;
  if (h < hhalf) {
    float mixfactor = smoothstep(h0, h1, h);
    base_color = mix(
      color_1 * castle_texture_color_to_linear(texture2D(tex_1, uv * uv_scale_1)).rgb,
      color_2 * castle_texture_color_to_linear(texture2D(tex_2, uv * uv_scale_2)).rgb,
      mixfactor);
  } else {
    float mixfactor = smoothstep(h2, h3, h);
    base_color = mix(
      color_2 * castle_texture_color_to_linear(texture2D(tex_2, uv * uv_scale_2)).rgb,
      color_3 * castle_texture_color_to_linear(texture2D(tex_3, uv * uv_scale_3)).rgb,
      mixfactor);
  }

  fragment_color.rgb = mix(fragment_color.rgb, base_color, texture_mix);

  fragment_color.rgb *= mix(normal_darkening, 1.0, smoothstep(normal_dark, 1.0, normal_slope));
}
