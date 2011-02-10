/* Generic GLSL fragment shader template. It will be used
   by vrmlshadergenerator.pas to construct final shader.

   This is converted to template.fs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

varying vec4 vertex_eye;
varying vec3 normal_eye;

/* PLUG: fragment-declare declaration */

void add_light_contribution(inout vec4 color,
  const in vec3 normal_eye,
  const in gl_LightProducts light_products,
  const in gl_LightSourceParameters light_source,
  const in gl_MaterialParameters material)
{
  vec3 light_dir;

  /* add ambient term */
  color += light_products.ambient;

  /* add diffuse term */
  if (light_source.position.w != 0.0)
  {
    /* we assume in this case light_source.position.w == 1,
       so there's no need to divide by it. This is true for our VRML/X3D
       lights. */
    /* positional light */
    light_dir = normalize(light_source.position.xyz - vec3(vertex_eye));

    /* non-spot lights have always cutoff = 180, with cos = -1,
       so the check below will always be false. No need to explicitly
       compare with -1, nice. */
    if (dot(normalize(light_source.spotDirection), -light_dir) <
        light_source.spotCosCutoff)
      return;
  } else
  {
    /* directional light */
    light_dir = normalize(light_source.position.xyz);
  }

  color += light_products.diffuse
    * max(dot(normal_eye, light_dir), 0.0);

  /* add specular term */
  vec3 reflect = normalize(-reflect(light_dir, normal_eye));
  /* vertex to camera direction = camera pos - vertex pos.
     We work in eye space here, so camera pos = always zero. */
  vec3 vertex_to_camera_dir = normalize(-vec3(vertex_eye));
  color += light_products.specular
    * pow(max(dot(reflect, vertex_to_camera_dir), 0.0), material.shininess);
}

void main(void)
{
  gl_FragColor = gl_FrontLightModelProduct.sceneColor;

  if (gl_FrontFacing)
  {
    for (int i = 0; i < LIGHTS_ENABLED; i++)
      add_light_contribution(gl_FragColor, normal_eye,
        gl_FrontLightProduct[i], gl_LightSource[i], gl_FrontMaterial);
    /* Otherwise, alpha is usually large after previous add_light_contribution,
       and it's always opaque.
       Using diffuse.a is actually exactly what fixed-function pipeline does
       too, according to http://www.sjbaker.org/steve/omniv/opengl_lighting.html
    */
    gl_FragColor.a = gl_FrontMaterial.diffuse.a;
  } else
  {
    for (int i = 0; i < LIGHTS_ENABLED; i++)
      add_light_contribution(gl_FragColor, -normal_eye,
        gl_BackLightProduct[i], gl_LightSource[i], gl_BackMaterial);
    gl_FragColor.a = gl_BackMaterial.diffuse.a;
  }

  /* PLUG: texture-apply void %s(inout vec4 fragment_color) */

  /* PLUG: fragment-end void %s(const in vec4 fragment_color) */
}
