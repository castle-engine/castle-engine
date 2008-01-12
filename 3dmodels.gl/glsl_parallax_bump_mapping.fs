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

#define STEEP
#ifdef STEEP
  /* You can define STEEP_SHADOW only if STEEP is defined. */
  #define STEEP_SHADOW
#endif

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
  float num_steps = 10.0;
  /* At smaller view angles, much more iterations needed, otherwise ugly
     aliasing arifacts quickly appear. I saw implementations that mix between
     50 and 10 num_stepa --- really a lot of iterations. */
  num_steps = mix(num_steps * 3.0, num_steps, p_to_eye.z);

  float step = 1.0 / num_steps;

  /* Should we remove "p_to_eye.z" below, i.e. should we apply
     "offset limiting" ? In works about steep parallax mapping,
     p_to_eye.z is present, and in sample steep parallax mapping
     shader they suggest that it doesn't really matter.
     My tests confirm this, so I leave p_to_eye.z component. */

  vec2 delta = -p_to_eye.xy * scale / (p_to_eye.z * num_steps);

  float height = 1.0;

  float map_height = texture2D(tex_height_map, texture_coord).r;
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

  /* I want to do two-sided lighting, so I want to have normal
     pointing from this side of the face that is currently displayed.
     Current normal is for front face, so negate it if backfacing.
     Since this is in tangent space, "negate" means only negate it's z
     component.

     Alt version of this, not using "if" just in case for future:
       normal.z -= normal.z * 2.0 * (1.0 - float(gl_FrontFacing));
  */
/*  if (!gl_FrontFacing)
    normal.z = -normal.z;
*/
  /* gl_FragColor += diffuse lighting */
#ifndef STEEP_SHADOW
  gl_FragColor += light_diffuse_color * gl_FrontMaterial.diffuse *
      max(dot(normal, light_dir), 0.0);

  gl_FragColor *= texture2D(tex_original, texture_coord);
#else
  float diffuse_factor = dot(normal, light_dir);
  if (diffuse_factor > 0)
  {
    /* We basically do the same thing as when we calculate texture_coord
       with steep parallax mapping.
       Only now we increment height, and we use light_dir instead of
       p_to_eye. */
    float num_steps = 10.0;
    num_steps = mix(num_steps * 3.0, num_steps, light_dir.z);

    float step = 1.0 / num_steps;

    vec2 delta = light_dir.xy * scale / (light_dir.z * num_steps);

    /* Do the 1st step always, otherwise initial height = shadow_map_height
       and we would be considered in our own shadow. */
    float height = map_height + step;
    vec2 shadow_texture_coord = texture_coord + delta;
    float shadow_map_height = texture2D(tex_height_map, shadow_texture_coord).r;

    while (shadow_map_height < height && height < 1)
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

/* This is more close to what bmDot3Normalized method did
   (but it's less correct and generally worse. I present it here
   only to visually compare bmDot3Normalized and bmGLSL,
   to see that they are able to calculate the same.)
*/
/*
  gl_FragColor =
    //gl_FrontLightModelProduct.sceneColor +
    gl_FrontLightProduct[0].ambient +
    gl_FrontLightProduct[0].diffuse;

  gl_FragColor *= texture2D(tex_original, texture_coord);

  gl_FragColor *= max(dot(normal, light_dir), 0.0);
*/
}
