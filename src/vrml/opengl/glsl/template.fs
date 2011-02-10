/* Generic GLSL fragment shader template. It will be used
   by vrmlshadergenerator.pas to construct final shader.

   This is converted to template.fs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

varying vec4 vertex_eye;
varying vec3 normal_eye;

/* *** FRAGMENT-SHADER-DECLARE *** */

void add_light_contribution(inout vec4 color,
  const in gl_LightProducts light_products,
  const in gl_LightSourceParameters light_source)
{
  /* add ambient term */
  color += light_products.ambient;

  /* add diffuse term */
  /* TODO: assume directional light.
     TODO: in what coords is light_source.position? */
  vec3 light_dir = light_source.position.xyz;
  color += light_products.diffuse
    * max(dot(normal_eye, light_dir), 0.0);

  /* add specular term */
  vec3 reflect = normalize(-reflect(light_dir, normal_eye));
  /* vertex to camera direction = camera pos - vertex pos.
     We work in eye space here, so camera pos = always zero. */
  vec3 vertex_to_camera_dir = normalize(-vec3(vertex_eye));
  color += light_products.specular
    * pow(max(dot(reflect, vertex_to_camera_dir), 0.0),
          gl_FrontMaterial.shininess);
}

void main(void)
{
  gl_FragColor = gl_FrontLightModelProduct.sceneColor;

/* TODO: We assume we have constant num of lights
   TODO: two-side lighting? */

  for (int i = 0; i < 1; i++)
    add_light_contribution(gl_FragColor,
      gl_FrontLightProduct[i], gl_LightSource[i]);

  /* *** TEXTURE-APPLY *** */
}
