/* Generic GLSL fragment shader template. It will be used
   by vrmlshadergenerator.pas to construct final shader.

   This is converted to template.fs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

varying vec4 vertex_eye;
varying vec3 normal_eye;

void main(void)
{
  gl_FragColor = gl_FrontLightModelProduct.sceneColor;

/* TODO: We assume we have constant num of lights
   TODO: two-side lighting? */

  for (int i = 0; i < 1; i++)
  {
    /* add ambient term */
    gl_FragColor += gl_FrontLightProduct[i].ambient;

    /* add diffuse term */
    /* TODO: assume directional light.
       TODO: in what coords is gl_LightSource[i].position? */
    vec3 light_dir = gl_LightSource[i].position.xyz;
    gl_FragColor += gl_FrontLightProduct[i].diffuse
      * max(dot(normal_eye, light_dir), 0.0);

    /* add specular term */
    vec3 reflect = normalize(-reflect(light_dir, normal_eye));
    /* vertex to camera direction = camera pos - vertex pos.
       We work in eye space here, so camera pos = always zero. */
    vec3 vertex_to_camera_dir = normalize(-vec3(vertex_eye));
    gl_FragColor += gl_FrontLightProduct[i].specular
      * pow(max(dot(reflect, vertex_to_camera_dir), 0.0),
            gl_FrontMaterial.shininess);
  }

// gl_FragColor *= texture2D(tex_original, gl_TexCoord[0].st);
}
