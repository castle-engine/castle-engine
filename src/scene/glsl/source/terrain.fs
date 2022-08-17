/* OpenGL shader effect (used to enhance the Castle Game Engine shaders,
   see https://castle-engine.io/compositing_shaders.php ),
   applied over terrain.

   This adjusts terrain color, mixing textures, based on current height. */

uniform sampler2D tex_1;
uniform sampler2D tex_2;
uniform sampler2D tex_3;

uniform float uv_scale_1;
uniform float uv_scale_2;
uniform float uv_scale_3;

uniform float normal_dark;
uniform float normal_darkening;
uniform float texture_mix;

uniform float h0; // below is tex_1
uniform float h1; // below is tex_2 mixed with tex_1
uniform float h2; // below is tex_2
uniform float h3; // below is tex_3 mixed with tex_2
                  // above is tex_3

varying vec3 terrain_position;
varying vec3 terrain_normal;

void PLUG_main_texture_apply(inout vec4 fragment_color, const in vec3 normal)
{
  vec4 tex;
  float h = terrain_position.y;
  /* We flip terrain_position.z, to map texture more naturally, when viewed from above.
     This is consistent with calculating TexCoord for TCastleTerrainData.Height.
     We just flip the sign, because the terrain textures always have repeat = true,
     so there's no need to shift the texture in any way.
  */
  vec2 uv = vec2(terrain_position.x, -terrain_position.z);
  float normal_slope = normalize(terrain_normal).y;

  /*
  if (h <= h0) {
    tex = texture2D(tex_1, uv);
  } else
  if (h <= h1) {
    float mixfactor = smoothstep(h0, h1, h);
      //clamp((h - h0) / (h1 - h0), 0.0, 1.0);
    tex = mix(texture2D(tex_1, uv), texture2D(tex_2, uv), mixfactor);
  } else
  if (h <= h2) {
    tex = texture2D(tex_2, uv);
  } else
  if (h <= h3) {
    float mixfactor = smoothstep(h2, h3, h);
      //clamp((h - h2) / (h3 - h2), 0.0, 1.0);
    tex = mix(texture2D(tex_2, uv), texture2D(tex_3, uv), mixfactor);
  } else
  {
    tex = texture2D(tex_3, uv);
  }
  */

  /* This achieves the same effect as above (because smoothstep
     does clamp() inside), but better:
     - one 1 "if", instead of 4 "if"s
     - no weird artifacts when h is precisely at h0, h1, h2 or h3.

       (previous code was sometimes showing a weird color at these borders,
       possibly GPU was forcing all neighboring pixels to have the same
       "if" outcome -- observed with
         Renderer: GeForce GTS 450/PCIe/SSE2
         Version: 4.5.0 NVIDIA 375.82
         on Linux/x86_64.
       ).
  */

  float hhalf = (h1 + h2) * 0.5;
  if (h < hhalf) {
    float mixfactor = smoothstep(h0, h1, h);
    tex = mix(
      texture2D(tex_1, uv * uv_scale_1),
      texture2D(tex_2, uv * uv_scale_2), mixfactor);
  } else {
    float mixfactor = smoothstep(h2, h3, h);
    tex = mix(
      texture2D(tex_2, uv * uv_scale_2),
      texture2D(tex_3, uv * uv_scale_3), mixfactor);
  }

  fragment_color.rgb = mix(fragment_color.rgb, tex.rgb, texture_mix);

  fragment_color.rgb *= mix(normal_darkening, 1.0, smoothstep(normal_dark, 1.0, normal_slope));
}
