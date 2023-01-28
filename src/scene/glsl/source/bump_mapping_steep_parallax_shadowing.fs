/* Steep parallax with self-shading bump mapping shader effect.
   This is added right after bump_mapping.fs and bump_mapping_parallax.fs.
   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.
*/

// avoid redeclaring this when no "separate compilation units" (OpenGLES)
#ifndef GL_ES
uniform float castle_parallax_bm_scale;
uniform sampler2D castle_normal_map;
float castle_bm_height;
vec2 castle_parallax_tex_coord;
#endif

varying vec3 castle_light_direction_tangent_space;

/* This has to be done after PLUG texture_coord_shift (done from PLUG main_texture_apply),
   as we depend that global castle_bm_height/castle_parallax_tex_coord
   are already set correctly. */
void PLUG_steep_parallax_shadow_apply(inout vec4 fragment_color)
{
  vec3 light_dir = normalize(castle_light_direction_tangent_space);

  /* We basically do the same thing as when we calculate tex_coord
     with steep parallax mapping.
     Only now we increment height, and we use light_dir instead of
     v_to_eye. */
  float num_steps = mix(30.0, 10.0, light_dir.z);

  float step = 1.0 / num_steps;

  vec2 delta = light_dir.xy * castle_parallax_bm_scale / (light_dir.z * num_steps);

  /* Do the 1st step always, otherwise initial height = shadow_map_height
     and we would be considered in our own shadow. */
  float height = castle_bm_height + step;
  vec2 shadow_texture_coord = castle_parallax_tex_coord + delta;
  float shadow_map_height = texture2D(castle_normal_map, shadow_texture_coord).a;

  while (shadow_map_height < height && height < 1.0)
  {
    height += step;
    shadow_texture_coord += delta;
    shadow_map_height = texture2D(castle_normal_map, shadow_texture_coord).a;
  }

  if (shadow_map_height >= height)
  {
    /* TODO: setting appropriate light contribution to 0 would be more correct.
       But for now, this self-shadowing is hacky, always from light source 0,
       and after the light calculation is actually done. */
    fragment_color.rgb /= 2.0;
  }
}
