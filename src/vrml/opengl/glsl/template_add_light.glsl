/* Shader code used for adding light source contribution. */

void PLUG_add_light_contribution_side(inout vec4 color,
  const in vec4 vertex_eye,
  const in vec3 normal_eye,
  const in gl_MaterialParameters material)
{
  vec3 light_dir;

/* Calculate light_dir */
#ifdef LIGHT_TYPE_KNOWN

  light_dir = normalize(gl_LightSource[light_number].position.xyz
#ifdef LIGHT_TYPE_POSITIONAL
    /* positional light */
    /* we assume in this case gl_LightSource[light_number].position.w == 1,
       so there's no need to divide by it. This is true for our VRML/X3D
       lights. */
    - vec3(vertex_eye)
#endif
  );

#ifdef LIGHT_TYPE_SPOT
  /* Check gl_LightSource[light_number].position first, as we want to add nothing
     (not even ambient term) when were outside of spot light cone. */

  float spot_cos = dot(normalize(gl_LightSource[light_number].spotDirection), -light_dir);
  /* non-spot lights have always cutoff = 180, with cos = -1,
     so the check below will always be false. No need to explicitly
     compare with -1, nice. */
  if (spot_cos < gl_LightSource[light_number].spotCosCutoff)
    return;
#endif

#else

  /* When light type is not known (this happens for lights set outside
     of TVRMLGLRenderer), we use less efficient code, that actually detects
     light type. */
  if (gl_LightSource[light_number].position.w != 0.0)
  {
    light_dir = normalize(gl_LightSource[light_number].position.xyz - vec3(vertex_eye));
    if (dot(normalize(gl_LightSource[light_number].spotDirection), -light_dir) <
        gl_LightSource[light_number].spotCosCutoff)
      return;
  } else
  {
    light_dir = normalize(gl_LightSource[light_number].position.xyz);
  }

#endif

  float scale = 1.0;
  /* PLUG: light_scale (scale, normal_eye, light_dir, gl_LightSource[light_number], gl_SideLightProduct[light_number], material) */

#ifdef LIGHT_TYPE_SPOT
  scale *= pow(spot_cos, gl_LightSource[light_number].spotExponent);
#endif

#ifdef LIGHT_HAS_ATTENUATION_ONLY_CONSTANT
  scale /= gl_LightSource[light_number].constantAttenuation;
#endif

#ifdef LIGHT_HAS_ATTENUATION_FULL
  /* We could calculate distance_to_light during calculation of light_dir.
     This would allow us to work faster (no need for calculating sqrt two
     times). However, I fear that calculating l = length() and then doing
     /= l would be in practice slower than calling built-in normalize().
     It would be best to calculate distance_to_light this way only when
     LIGHT_HAS_ATTENUATION_FULL is defined. */
  float distance_to_light = distance(gl_LightSource[light_number].position.xyz,
    vec3(vertex_eye));
  scale /= gl_LightSource[light_number].constantAttenuation +
           gl_LightSource[light_number].linearAttenuation * distance_to_light +
           gl_LightSource[light_number].quadraticAttenuation * distance_to_light * distance_to_light;
#endif

  /* add ambient term */
  vec4 light_color =
#ifdef LIGHT_HAS_AMBIENT
  gl_SideLightProduct[light_number].ambient;
#else
  vec4(0.0);
#endif

  /* add diffuse term */
  vec4 diffuse = gl_SideLightProduct[light_number].diffuse;
  /* PLUG: material_light_diffuse (diffuse, vertex_eye, normal_eye, gl_LightSource[light_number], material) */
  float diffuse_factor = max(dot(normal_eye, light_dir), 0.0);
  light_color += diffuse * diffuse_factor;

#ifdef LIGHT_HAS_SPECULAR
  /* add specular term */
  if (diffuse_factor != 0.0)
    light_color += gl_SideLightProduct[light_number].specular *
      pow(max(dot(vec3(gl_LightSource[light_number].halfVector), normal_eye),
        0.0), material.shininess);
#endif

  color += light_color * scale;

#undef LIGHT_TYPE_POSITIONAL
#undef LIGHT_TYPE_SPOT
#undef LIGHT_TYPE_KNOWN
#undef LIGHT_HAS_AMBIENT
#undef LIGHT_HAS_SPECULAR

}
