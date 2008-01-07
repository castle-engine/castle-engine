/* GLSL fragment shader to do bump mapping.

   This is converted to glsl_bump_mapping.fs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

uniform sampler2D tex_normal_map;
uniform sampler2D tex_original;
uniform vec4 light_ambient_color;
uniform vec4 light_diffuse_color;

varying vec3 light_dir_tangent;

void main(void)
{
  /* gl_FragColor = all ambient lighting. */
  gl_FragColor =
    gl_FrontLightModelProduct.sceneColor +
    light_ambient_color * gl_FrontMaterial.ambient;

  /* Both light_dir and normal are in tangent space. */
  vec3 light_dir = normalize(light_dir_tangent);

  /* I read normal from texture, this is the very idea of bump mapping.
     Unpack normals, they are in texture in [0..1] range and I want in [-1..1]. */
  vec3 normal = vec3(
    texture2D(tex_normal_map, gl_TexCoord[0].st)) * 2.0 - vec3(1, 1, 1);

  /* I want to do two-sided lighting, so I want to have normal
     pointing from this side of the face that is currently displayed.
     Current normal is for front face, so negate it if backfacing.
     Since this is in tangent space, "negate" means only negate it's z
     component.

     Alt version of this, not using "if" just in case for future:
       normal.z -= normal.z * 2.0 * (1.0 - float(gl_FrontFacing));
  */
  if (!gl_FrontFacing)
    normal.z = -normal.z;

  /* gl_FragColor += diffuse lighting */
  gl_FragColor += light_diffuse_color * gl_FrontMaterial.diffuse *
      max(dot(normal, light_dir), 0.0);

  gl_FragColor *= texture2D(tex_original, gl_TexCoord[0].st);

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

  gl_FragColor *= texture2D(tex_original, gl_TexCoord[0].st);

  gl_FragColor *= max(dot(normal, light_dir), 0.0);
*/
}
