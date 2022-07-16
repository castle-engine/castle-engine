{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/bump_mapping_parallax.fs" }
'/* Parallax bump mapping shader effect.' + LineEnding +
'   This is added right after bump_mapping.fs.' + LineEnding +
'   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'uniform float castle_parallax_bm_scale;' + LineEnding +
'' + LineEnding +
'// declare castle_normal_map, avoiding redeclaring it for GL_ES' + LineEnding +
'#ifdef GL_ES' + LineEnding +
'  #ifndef castle_normal_map_defined' + LineEnding +
'  #define castle_normal_map_defined' + LineEnding +
'  uniform sampler2D castle_normal_map;' + LineEnding +
'  #endif' + LineEnding +
'#else' + LineEnding +
'  uniform sampler2D castle_normal_map;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'varying vec3 castle_vertex_to_eye_in_tangent_space;' + LineEnding +
'' + LineEnding +
'#ifdef CASTLE_BUMP_MAPPING_PARALLAX_STEEP' + LineEnding +
'float castle_bm_height;' + LineEnding +
'vec2 castle_parallax_tex_coord;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void PLUG_texture_coord_shift(inout vec2 tex_coord)' + LineEnding +
'{' + LineEnding +
'  // We have to normalize castle_vertex_to_eye_in_tangent_space again, just like normal vectors.' + LineEnding +
'  vec3 v_to_eye = normalize(castle_vertex_to_eye_in_tangent_space);' + LineEnding +
'' + LineEnding +
'#ifndef CASTLE_BUMP_MAPPING_PARALLAX_STEEP' + LineEnding +
'' + LineEnding +
'  // Classic (not steep) parallax bump mapping' + LineEnding +
'  float height = (texture2D(castle_normal_map, tex_coord).a - 1.0/2.0) * castle_parallax_bm_scale;' + LineEnding +
'  tex_coord += height * v_to_eye.xy /* / v_to_eye.z*/;' + LineEnding +
'' + LineEnding +
'#else' + LineEnding +
'' + LineEnding +
'  // Steep parallax bump mapping' + LineEnding +
'  /* At smaller view angles, much more iterations needed, otherwise ugly' + LineEnding +
'     aliasing artifacts quickly appear. */' + LineEnding +
'  float num_steps = mix(30.0, 10.0, v_to_eye.z);' + LineEnding +
'#ifdef CASTLE_BUGGY_BUMP_MAPPING_NUM_STEPS' + LineEnding +
'  num_steps = clamp(num_steps, 10.0, 30.0);' + LineEnding +
'#endif' + LineEnding +
'  float step = 1.0 / num_steps;' + LineEnding +
'' + LineEnding +
'  /* Should we remove "v_to_eye.z" below, i.e. should we apply' + LineEnding +
'     "offset limiting" ? In works about steep parallax mapping,' + LineEnding +
'     v_to_eye.z is present, and in sample steep parallax mapping' + LineEnding +
'     shader they suggest that it doesn''t really matter.' + LineEnding +
'     My tests confirm this, so I leave v_to_eye.z component. */' + LineEnding +
'' + LineEnding +
'  vec2 delta = -v_to_eye.xy * castle_parallax_bm_scale / (v_to_eye.z * num_steps);' + LineEnding +
'  float height = 1.0;' + LineEnding +
'  castle_bm_height = texture2D(castle_normal_map, tex_coord).a;' + LineEnding +
'' + LineEnding +
'  /* TODO: NVidia GeForce FX 5200 fails here with' + LineEnding +
'' + LineEnding +
'        error C5011: profile does not support "while" statements' + LineEnding +
'        and "while" could not be unrolled.' + LineEnding +
'' + LineEnding +
'     I could workaround this problem (by using' + LineEnding +
'       for (int i = 0; i < steep_steps_max; i++)' + LineEnding +
'     loop and' + LineEnding +
'       if (! (castle_bm_height < height)) break;' + LineEnding +
'     , this is possible to unroll). But it turns out that this still' + LineEnding +
'     (even with steep_steps_max = 1) works much too slow on this hardware...' + LineEnding +
'  */' + LineEnding +
'' + LineEnding +
'  while (castle_bm_height < height)' + LineEnding +
'  {' + LineEnding +
'    height -= step;' + LineEnding +
'    tex_coord += delta;' + LineEnding +
'    castle_bm_height = texture2D(castle_normal_map, tex_coord).a;' + LineEnding +
'  }' + LineEnding +
'' + LineEnding +
'  // Save value for bump_mapping_steep_parallax_shadowing.fs' + LineEnding +
'  castle_parallax_tex_coord = tex_coord;' + LineEnding +
'' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
''
