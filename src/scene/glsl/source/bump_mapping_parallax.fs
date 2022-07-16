/* Parallax bump mapping shader effect.
   This is added right after bump_mapping.fs.
   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.
*/

uniform float castle_parallax_bm_scale;

// declare castle_normal_map, avoiding redeclaring it for GL_ES
#ifdef GL_ES
  #ifndef castle_normal_map_defined
  #define castle_normal_map_defined
  uniform sampler2D castle_normal_map;
  #endif
#else
  uniform sampler2D castle_normal_map;
#endif

varying vec3 castle_vertex_to_eye_in_tangent_space;

#ifdef CASTLE_BUMP_MAPPING_PARALLAX_STEEP
float castle_bm_height;
vec2 castle_parallax_tex_coord;
#endif

void PLUG_texture_coord_shift(inout vec2 tex_coord)
{
  // We have to normalize castle_vertex_to_eye_in_tangent_space again, just like normal vectors.
  vec3 v_to_eye = normalize(castle_vertex_to_eye_in_tangent_space);

#ifndef CASTLE_BUMP_MAPPING_PARALLAX_STEEP

  // Classic (not steep) parallax bump mapping
  float height = (texture2D(castle_normal_map, tex_coord).a - 1.0/2.0) * castle_parallax_bm_scale;
  tex_coord += height * v_to_eye.xy /* / v_to_eye.z*/;

#else

  // Steep parallax bump mapping
  /* At smaller view angles, much more iterations needed, otherwise ugly
     aliasing artifacts quickly appear. */
  float num_steps = mix(30.0, 10.0, v_to_eye.z);
#ifdef CASTLE_BUGGY_BUMP_MAPPING_NUM_STEPS
  num_steps = clamp(num_steps, 10.0, 30.0);
#endif
  float step = 1.0 / num_steps;

  /* Should we remove "v_to_eye.z" below, i.e. should we apply
     "offset limiting" ? In works about steep parallax mapping,
     v_to_eye.z is present, and in sample steep parallax mapping
     shader they suggest that it doesn't really matter.
     My tests confirm this, so I leave v_to_eye.z component. */

  vec2 delta = -v_to_eye.xy * castle_parallax_bm_scale / (v_to_eye.z * num_steps);
  float height = 1.0;
  castle_bm_height = texture2D(castle_normal_map, tex_coord).a;

  /* TODO: NVidia GeForce FX 5200 fails here with

        error C5011: profile does not support "while" statements
        and "while" could not be unrolled.

     I could workaround this problem (by using
       for (int i = 0; i < steep_steps_max; i++)
     loop and
       if (! (castle_bm_height < height)) break;
     , this is possible to unroll). But it turns out that this still
     (even with steep_steps_max = 1) works much too slow on this hardware...
  */

  while (castle_bm_height < height)
  {
    height -= step;
    tex_coord += delta;
    castle_bm_height = texture2D(castle_normal_map, tex_coord).a;
  }

  // Save value for bump_mapping_steep_parallax_shadowing.fs
  castle_parallax_tex_coord = tex_coord;

#endif
}
