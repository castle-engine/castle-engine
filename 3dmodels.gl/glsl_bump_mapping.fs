/* GLSL fragment shader to do bump mapping.

   This is converted to glsl_bump_mapping.fs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

uniform sampler2D tex_normal_map;
uniform sampler2D tex_original;

varying vec3 light_dir_tangent;

void main(void)
{
  /* Both light_dir and normal are in tangent space. */
  vec3 light_dir = normalize(light_dir_tangent);

  /* I read normal from texture, this is the very idea of bump mapping.
     Unpack normals, they are in texture in [0..1] range and I want in [-1..1]. */
  vec3 normal = texture2D(tex_normal_map, gl_TexCoord[0].st) * 2 - vec3(1, 1, 1);

  /* gl_FragColor = lighting computed mostly just like using OpenGL
     fixed-function pipeline, but per-fragment. */
  gl_FragColor =
    gl_FrontLightModelProduct.sceneColor +
    gl_FrontLightProduct[0].ambient +
    gl_FrontLightProduct[0].diffuse * max(dot(normal, light_dir), 0.0);

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
