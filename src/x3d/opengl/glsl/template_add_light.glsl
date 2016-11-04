/* Shader code used for adding light source contribution. */

/* TODO: use this, i.e. define PLUG_geometry_vertex_*
   for every light source to pass these */
#ifdef HAS_GEOMETRY_SHADER
  #define castle_LightSource<Light>Radius castle_LightSource<Light>Radius_geoshader
  #define castle_LightSource<Light>BeamWidth castle_LightSource<Light>BeamWidth_geoshader
#endif

#ifdef LIGHT_TYPE_SPOT
/* We use our own field for spotCosCutoff, as using
   gl_LightSource[<Light>].spotCosCutoff
   is buggy on Radeon on Linux / Mesa:

      Version string: 3.0 Mesa 11.2.0
      Vendor: X.Org
      Vendor type: Unknown
      Renderer: Gallium 0.4 on AMD RV710 (DRM 2.43.0, LLVM 3.8.0)
*/
uniform float castle_LightSource<Light>SpotCosCutoff;
#endif

#ifdef LIGHT_HAS_RADIUS
uniform float castle_LightSource<Light>Radius;
#endif

#ifdef LIGHT_HAS_BEAM_WIDTH
uniform float castle_LightSource<Light>BeamWidth;
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
     gl_LightSource[<Light>].position.w == 1, so there's no need
     to divide by it. This is true for our VRML/X3D lights. */
  light_dir = gl_LightSource[<Light>].position.xyz - vec3(vertex_eye);
  float distance_to_light = length(light_dir);
  light_dir /= distance_to_light;
#else
  light_dir = normalize(gl_LightSource[<Light>].position.xyz);
#endif

#ifdef LIGHT_TYPE_SPOT
  /* Check gl_LightSource[<Light>].position first, as we want to add nothing
     (not even ambient term) when were outside of spot light cone. */

  float spot_cos = dot(normalize(gl_LightSource[<Light>].spotDirection), -light_dir);
  /* non-spot lights have always cutoff = 180, with cos = -1,
     so the check below will always be false. No need to explicitly
     compare with -1, nice. */
  if (spot_cos < castle_LightSource<Light>SpotCosCutoff)
    return;
#endif

  float scale = 1.0;
  /* PLUG: light_scale (scale, normal_eye, light_dir, gl_LightSource[<Light>], gl_SideLightProduct[<Light>], material) */

#ifdef LIGHT_TYPE_SPOT
#ifdef LIGHT_HAS_BEAM_WIDTH
  /* calculate spot following VRML 2.0/X3D idea of beamWidth */
  float cutOffAngle = radians(gl_LightSource[<Light>].spotCutoff);
  scale *= clamp(
    (                    acos(spot_cos) - cutOffAngle) /
    (castle_LightSource<Light>BeamWidth - cutOffAngle),
    0.0, 1.0);
#endif

#ifdef LIGHT_HAS_SPOT_EXPONENT
  /* calculate spot like fixed-function pipeline, using exponent */
  scale *= pow(spot_cos, gl_LightSource[<Light>].spotExponent);
#endif
#endif

#ifdef LIGHT_HAS_ATTENUATION
  scale /= max(1.0,
           gl_LightSource[<Light>].constantAttenuation +
           gl_LightSource[<Light>].linearAttenuation * distance_to_light +
           gl_LightSource[<Light>].quadraticAttenuation * distance_to_light * distance_to_light);
#endif

#ifdef LIGHT_HAS_RADIUS
  if (distance_to_light >= castle_LightSource<Light>Radius)
    scale = 0.0;
#endif

  /* add ambient term */
  vec4 light_color =
#ifdef LIGHT_HAS_AMBIENT
  gl_SideLightProduct[<Light>].ambient;
#else
  vec4(0.0);
#endif

  /* add diffuse term */
  vec4 diffuse = gl_SideLightProduct[<Light>].diffuse;
  /* PLUG: material_light_diffuse (diffuse, vertex_eye, normal_eye, gl_LightSource[<Light>], material) */
  float diffuse_factor = max(dot(normal_eye, light_dir), 0.0);
  light_color += diffuse * diffuse_factor;

#ifdef LIGHT_HAS_SPECULAR
  /* add specular term */
  if (diffuse_factor != 0.0)
    light_color += gl_SideLightProduct[<Light>].specular *
      pow(max(dot(vec3(gl_LightSource[<Light>].halfVector), normal_eye),
        0.0), material.shininess);
#endif

  color += light_color * scale;
}

/* Not really necessary, but undefine just in case, just like OpenGL ES shader. */
#undef LIGHT_TYPE_POSITIONAL
#undef LIGHT_TYPE_SPOT
#undef LIGHT_HAS_AMBIENT
#undef LIGHT_HAS_SPECULAR
#undef LIGHT_HAS_ATTENUATION
#undef LIGHT_HAS_RADIUS
#undef LIGHT_HAS_BEAM_WIDTH
#undef LIGHT_HAS_SPOT_EXPONENT
