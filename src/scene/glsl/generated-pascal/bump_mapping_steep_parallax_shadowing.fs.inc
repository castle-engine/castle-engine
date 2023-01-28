{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/bump_mapping_steep_parallax_shadowing.fs" }
'/* Steep parallax with self-shading bump mapping shader effect.' + LineEnding +
'   This is added right after bump_mapping.fs and bump_mapping_parallax.fs.' + LineEnding +
'   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'// avoid redeclaring this when no "separate compilation units" (OpenGLES)' + LineEnding +
'#ifndef GL_ES' + LineEnding +
'uniform float castle_parallax_bm_scale;' + LineEnding +
'uniform sampler2D castle_normal_map;' + LineEnding +
'float castle_bm_height;' + LineEnding +
'vec2 castle_parallax_tex_coord;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'varying vec3 castle_light_direction_tangent_space;' + LineEnding +
'' + LineEnding +
'/* This has to be done after PLUG texture_coord_shift (done from PLUG main_texture_apply),' + LineEnding +
'   as we depend that global castle_bm_height/castle_parallax_tex_coord' + LineEnding +
'   are already set correctly. */' + LineEnding +
'void PLUG_steep_parallax_shadow_apply(inout vec4 fragment_color)' + LineEnding +
'{' + LineEnding +
'  vec3 light_dir = normalize(castle_light_direction_tangent_space);' + LineEnding +
'' + LineEnding +
'  /* We basically do the same thing as when we calculate tex_coord' + LineEnding +
'     with steep parallax mapping.' + LineEnding +
'     Only now we increment height, and we use light_dir instead of' + LineEnding +
'     v_to_eye. */' + LineEnding +
'  float num_steps = mix(30.0, 10.0, light_dir.z);' + LineEnding +
'' + LineEnding +
'  float step = 1.0 / num_steps;' + LineEnding +
'' + LineEnding +
'  vec2 delta = light_dir.xy * castle_parallax_bm_scale / (light_dir.z * num_steps);' + LineEnding +
'' + LineEnding +
'  /* Do the 1st step always, otherwise initial height = shadow_map_height' + LineEnding +
'     and we would be considered in our own shadow. */' + LineEnding +
'  float height = castle_bm_height + step;' + LineEnding +
'  vec2 shadow_texture_coord = castle_parallax_tex_coord + delta;' + LineEnding +
'  float shadow_map_height = texture2D(castle_normal_map, shadow_texture_coord).a;' + LineEnding +
'' + LineEnding +
'  while (shadow_map_height < height && height < 1.0)' + LineEnding +
'  {' + LineEnding +
'    height += step;' + LineEnding +
'    shadow_texture_coord += delta;' + LineEnding +
'    shadow_map_height = texture2D(castle_normal_map, shadow_texture_coord).a;' + LineEnding +
'  }' + LineEnding +
'' + LineEnding +
'  if (shadow_map_height >= height)' + LineEnding +
'  {' + LineEnding +
'    /* TODO: setting appropriate light contribution to 0 would be more correct.' + LineEnding +
'       But for now, this self-shadowing is hacky, always from light source 0,' + LineEnding +
'       and after the light calculation is actually done. */' + LineEnding +
'    fragment_color.rgb /= 2.0;' + LineEnding +
'  }' + LineEnding +
'}' + LineEnding +
''
