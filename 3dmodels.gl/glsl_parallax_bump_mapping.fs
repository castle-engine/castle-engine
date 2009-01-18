/*
  Copyright 2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* GLSL fragment shader to do bump mapping.
   Version with parallax mapping.

   This is converted to glsl_parallax_bump_mapping.fs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

uniform sampler2D tex_normal_map;
uniform sampler2D tex_original;
uniform sampler2D tex_height_map;
uniform vec4 light_ambient_color;
uniform vec4 light_diffuse_color;

uniform float scale;
uniform float bias;

varying vec3 light_dir_tangent;

varying vec3 point_to_eye_in_tangent_space;

/* This will be defined (or not) right before compilation of this file,
   see VRMLOpenGLRenderer.pas.
#define STEEP
*/

#ifdef STEEP
  /* You can define STEEP_SHADOW only if STEEP is also defined.
     (Although it is possible to implement self-shadowing with still shifting
     tex coord with classic (no steep) parallax mapping, it's not implemented.)
  */
  #define STEEP_SHADOW
#endif

/* Number of steps for steep mapping.
   I saw implementations that mix between 50 and 10 num_steps.
   Mixing between 10 and 30 seems to give quite good results, and is really
   fast on ATI on MacBookPro.
*/

const float steep_steps_min = 10.0;
const float steep_steps_max = 30.0;

const float steep_shadow_steps_min = 10.0;
const float steep_shadow_steps_max = 30.0;

void main(void)
{
  vec3 p_to_eye = normalize(point_to_eye_in_tangent_space);
  vec2 texture_coord = gl_TexCoord[0].st;

#ifndef STEEP
  /* I take "r" (red) component of tex_height_map.
     When loading texture for tex_height_map, I made sure it's grayscale
     so I'm sure that all rgb components are the same. */
  float height = texture2D(tex_height_map, gl_TexCoord[0].st).r * scale - bias;
  texture_coord += height * p_to_eye.xy /* / p_to_eye.z*/;
#else
  /* At smaller view angles, much more iterations needed, otherwise ugly
     aliasing arifacts quickly appear. */
  float num_steps = mix(steep_steps_max, steep_steps_min, p_to_eye.z);

  float step = 1.0 / num_steps;

  /* Should we remove "p_to_eye.z" below, i.e. should we apply
     "offset limiting" ? In works about steep parallax mapping,
     p_to_eye.z is present, and in sample steep parallax mapping
     shader they suggest that it doesn't really matter.
     My tests confirm this, so I leave p_to_eye.z component. */

  vec2 delta = -p_to_eye.xy * scale / (p_to_eye.z * num_steps);

  float height = 1.0;

  float map_height = texture2D(tex_height_map, texture_coord).r;

  /* It's known problem that NVidia GeForce FX 5200 fails here with

       error C5011: profile does not support "while" statements
       and "while" could not be unrolled.

     I could workaround this problem (by using
       for (int i = 0; i < steep_steps_max; i++)
     loop and
       if (! (map_height < height)) break;
     , this is possible to unroll). But it turns out that this still
     (even with steep_steps_max = 1) works much too slow on this hardware...
     so I simply fallback to non-steep version of parallax mapping
     if this doesn't compile. */

  while (map_height < height)
  {
    height -= step;
    texture_coord += delta;
    map_height = texture2D(tex_height_map, texture_coord).r;
  }
#endif

  /* gl_FragColor = all ambient lighting. */
  gl_FragColor =
    gl_FrontLightModelProduct.sceneColor +
    light_ambient_color * gl_FrontMaterial.ambient;

  /* Both light_dir and normal are in tangent space. */
  vec3 light_dir = normalize(light_dir_tangent);

  /* I read normal from texture, this is the very idea of bump mapping.
     Unpack normals, they are in texture in [0..1] range and I want in [-1..1]. */
  vec3 normal = vec3(
    texture2D(tex_normal_map, texture_coord)) * 2.0 - vec3(1, 1, 1);

  /* TODO: two-sided lighting.
     See glsl_bump_mapping.fs for comments why it's not done now.

  if (!gl_FrontFacing)
    normal.z = -normal.z;

  Hmm, idea: using p_to_eye, I can handle two-sided lighting in parallax
  version by

  if (p_to_eye.z < 0.0)
    normal.z = -normal.z;

  This is for sure Ok with no-steep version. Check what should be done
  for steep version ? Besides, this doesn't fix the non-parallax version
  in glsl_bump_mapping.fs (p_to_eye is not calculated there,
  and it would silly (too much waste of time for something so seldom used ?)
  to calculate it only for the purpose of two-sided lighting ?).
  */

  /* gl_FragColor += diffuse lighting */
  float diffuse_factor = dot(normal, light_dir);
#ifndef STEEP_SHADOW
  gl_FragColor += light_diffuse_color * gl_FrontMaterial.diffuse *
      max(diffuse_factor, 0.0);

  gl_FragColor *= texture2D(tex_original, texture_coord);
#else
  if (diffuse_factor > 0.0)
  {
    /* We basically do the same thing as when we calculate texture_coord
       with steep parallax mapping.
       Only now we increment height, and we use light_dir instead of
       p_to_eye. */
    float num_steps = mix(steep_shadow_steps_max, steep_shadow_steps_min, light_dir.z);

    float step = 1.0 / num_steps;

    vec2 delta = light_dir.xy * scale / (light_dir.z * num_steps);

    /* Do the 1st step always, otherwise initial height = shadow_map_height
       and we would be considered in our own shadow. */
    float height = map_height + step;
    vec2 shadow_texture_coord = texture_coord + delta;
    float shadow_map_height = texture2D(tex_height_map, shadow_texture_coord).r;

    while (shadow_map_height < height && height < 1.0)
    {
      height += step;
      shadow_texture_coord += delta;
      shadow_map_height = texture2D(tex_height_map, shadow_texture_coord).r;
    }

    if (shadow_map_height < height)
    {
      gl_FragColor += light_diffuse_color * gl_FrontMaterial.diffuse *
        diffuse_factor;
    }
  }
#endif

  gl_FragColor *= texture2D(tex_original, texture_coord);
}
