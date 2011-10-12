/* Shader code used for adding light source contribution. */

#ifdef LIGHT_HAS_RADIUS
uniform float castle_light_light_number_radius;
#endif

#ifdef LIGHT_HAS_BEAM_WIDTH
uniform float castle_light_light_number_beam_width;
#endif

void PLUG_add_light_contribution_side(inout vec4 color,
  const in vec4 vertex_eye,
  const in vec3 normal_eye,
  const in gl_MaterialParameters material)
{
  vec3 light_dir;

/* Calculate light_dir */
#ifdef LIGHT_TYPE_POSITIONAL
  /* positional light. We assume in this case
     gl_LightSource[light_number].position.w == 1, so there's no need
     to divide by it. This is true for our VRML/X3D lights. */
  light_dir = gl_LightSource[light_number].position.xyz - vec3(vertex_eye);
  float distance_to_light = length(light_dir);
  light_dir /= distance_to_light;
#else
  light_dir = normalize(gl_LightSource[light_number].position.xyz);
#endif

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

  float scale = 1.0;
  /* PLUG: light_scale (scale, normal_eye, light_dir, gl_LightSource[light_number], gl_SideLightProduct[light_number], material) */

#ifdef LIGHT_TYPE_SPOT
#ifdef LIGHT_HAS_BEAM_WIDTH
  /* calculate spot following VRML 2.0/X3D idea of beamWidth */
  float cutOffAngle = radians(gl_LightSource[light_number].spotCutoff);
  scale *= clamp(
    (                     acos(spot_cos) - cutOffAngle) /
    (castle_light_light_number_beam_width - cutOffAngle),
    0.0, 1.0);
#endif

#ifdef LIGHT_HAS_SPOT_EXPONENT
  /* calculate spot like fixed-function pipeline, using exponent */
  scale *= pow(spot_cos, gl_LightSource[light_number].spotExponent);
#endif
#endif

#ifdef LIGHT_HAS_ATTENUATION
  scale /= gl_LightSource[light_number].constantAttenuation +
           gl_LightSource[light_number].linearAttenuation * distance_to_light +
           gl_LightSource[light_number].quadraticAttenuation * distance_to_light * distance_to_light;
#endif

#ifdef LIGHT_HAS_RADIUS
  if (distance_to_light >= castle_light_light_number_radius)
    scale = 0.0;
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
#undef LIGHT_HAS_AMBIENT
#undef LIGHT_HAS_SPECULAR
#undef LIGHT_HAS_ATTENUATION
#undef LIGHT_HAS_RADIUS

}
