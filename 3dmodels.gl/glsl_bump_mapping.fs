/* GLSL fragment shader to do bump mapping.

   This is converted to glsl_bump_mapping.fs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

uniform sampler2D tex_normal_map;
uniform sampler2D tex_original;

varying vec3 interpolated_normal;
varying vec3 vertex_world;

void main(void)
{
  /* TODO: take this from VRMLOpenGLRenderer.Attributes */
  vec3 light_position = gl_LightSource[0].position.xyz;
  vec3 light_dir = normalize(light_position - vertex_world);
  vec3 normal = normalize(interpolated_normal);

  /* gl_FragColor = lighting computed mostly just like using OpenGL
     fixed-function pipeline, but per-fragment. */

  gl_FragColor =
    gl_FrontLightModelProduct.sceneColor +
    gl_FrontLightProduct[0].ambient +
    gl_FrontLightProduct[0].diffuse * max(dot(normal, light_dir), 0.0);

  gl_FragColor *= texture2D(tex_original, gl_TexCoord[0].st);
}
