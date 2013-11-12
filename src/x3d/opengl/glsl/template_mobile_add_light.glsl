/* Shader code used for adding light source contribution, for OpenGL ES. */

/* Light source position in eye coordinates. */
const vec3 castle_LightSource<Light>Position = vec3(0.0, 0.0, 0.0);
uniform float castle_LightSource<Light>SpotExponent;
/* Multiplied colors of light source and material. */
uniform vec4 castle_SideLightProduct<Light>Ambient;
uniform vec4 castle_SideLightProduct<Light>Diffuse;
uniform vec4 castle_SideLightProduct<Light>Specular;

void PLUG_add_light_contribution(inout vec4 color,
  const in vec4 vertex_eye,
  const in vec3 normal_eye)
{
  vec3 light_dir = normalize(castle_LightSource<Light>Position - vec3(vertex_eye));
  vec3 reflect_dir = reflect(-light_dir, normal_eye);
  vec3 view_dir = normalize(-vec3(vertex_eye));
  float diffuse = max(dot(light_dir, normal_eye), 0.0);
  float spec = 0.0;
  if (diffuse > 0.0) {
      spec = max(dot(reflect_dir, view_dir), 0.0);
      spec = pow(spec, castle_LightSource<Light>SpotExponent);
  }
  color += castle_SideLightProduct<Light>Ambient +
    castle_SideLightProduct<Light>Diffuse * diffuse +
    castle_SideLightProduct<Light>Specular * spec;
}

/* Undefine lights symbols, since for OpenGL ES all the shader parts
   are concatenated into a single string. */
#undef LIGHT_TYPE_POSITIONAL
#undef LIGHT_TYPE_SPOT
#undef LIGHT_HAS_AMBIENT
#undef LIGHT_HAS_SPECULAR
#undef LIGHT_HAS_ATTENUATION
#undef LIGHT_HAS_RADIUS
