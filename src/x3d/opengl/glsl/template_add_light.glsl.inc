{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_add_light.glsl" }
'/* Shader code used for adding light source contribution. */' + LineEnding +
'' + LineEnding +
'/* TODO: use this, i.e. define PLUG_geometry_vertex_*' + LineEnding +
'   for every light source to pass these */' + LineEnding +
'#ifdef HAS_GEOMETRY_SHADER' + LineEnding +
'  #define castle_LightSource<Light>Radius castle_LightSource<Light>Radius_geoshader' + LineEnding +
'  #define castle_LightSource<Light>BeamWidth castle_LightSource<Light>BeamWidth_geoshader' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_TYPE_SPOT' + LineEnding +
'/* We use our own field for spotCosCutoff, as using' + LineEnding +
'   gl_LightSource[<Light>].spotCosCutoff' + LineEnding +
'   is buggy on Radeon on Linux / Mesa:' + LineEnding +
'' + LineEnding +
'      Version string: 3.0 Mesa 11.2.0' + LineEnding +
'      Vendor: X.Org' + LineEnding +
'      Vendor type: Unknown' + LineEnding +
'      Renderer: Gallium 0.4 on AMD RV710 (DRM 2.43.0, LLVM 3.8.0)' + LineEnding +
'*/' + LineEnding +
'uniform float castle_LightSource<Light>SpotCosCutoff;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_HAS_RADIUS' + LineEnding +
'uniform float castle_LightSource<Light>Radius;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_HAS_BEAM_WIDTH' + LineEnding +
'uniform float castle_LightSource<Light>BeamWidth;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void PLUG_add_light_contribution_side(inout vec4 color,' + LineEnding +
'  const in vec4 vertex_eye,' + LineEnding +
'  const in vec3 normal_eye,' + LineEnding +
'  const in gl_MaterialParameters material)' + LineEnding +
'{' + LineEnding +
'  vec3 light_dir;' + LineEnding +
'' + LineEnding +
'/* Calculate light_dir */' + LineEnding +
'#ifdef LIGHT_TYPE_POSITIONAL' + LineEnding +
'  /* positional light. We assume in this case' + LineEnding +
'     gl_LightSource[<Light>].position.w == 1, so there''s no need' + LineEnding +
'     to divide by it. This is true for our VRML/X3D lights. */' + LineEnding +
'  light_dir = gl_LightSource[<Light>].position.xyz - vec3(vertex_eye);' + LineEnding +
'  float distance_to_light = length(light_dir);' + LineEnding +
'  light_dir /= distance_to_light;' + LineEnding +
'#else' + LineEnding +
'  light_dir = normalize(gl_LightSource[<Light>].position.xyz);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_TYPE_SPOT' + LineEnding +
'  /* Check gl_LightSource[<Light>].position first, as we want to add nothing' + LineEnding +
'     (not even ambient term) when were outside of spot light cone. */' + LineEnding +
'' + LineEnding +
'  float spot_cos = dot(normalize(gl_LightSource[<Light>].spotDirection), -light_dir);' + LineEnding +
'  /* non-spot lights have always cutoff = 180, with cos = -1,' + LineEnding +
'     so the check below will always be false. No need to explicitly' + LineEnding +
'     compare with -1, nice. */' + LineEnding +
'  if (spot_cos < castle_LightSource<Light>SpotCosCutoff)' + LineEnding +
'    return;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  float scale = 1.0;' + LineEnding +
'  /* PLUG: light_scale (scale, normal_eye, light_dir, gl_LightSource[<Light>], gl_SideLightProduct[<Light>], material) */' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_TYPE_SPOT' + LineEnding +
'#ifdef LIGHT_HAS_BEAM_WIDTH' + LineEnding +
'  /* calculate spot following VRML 2.0/X3D idea of beamWidth */' + LineEnding +
'  float cutOffAngle = radians(gl_LightSource[<Light>].spotCutoff);' + LineEnding +
'  scale *= clamp(' + LineEnding +
'    (                    acos(spot_cos) - cutOffAngle) /' + LineEnding +
'    (castle_LightSource<Light>BeamWidth - cutOffAngle),' + LineEnding +
'    0.0, 1.0);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_HAS_SPOT_EXPONENT' + LineEnding +
'  /* calculate spot like fixed-function pipeline, using exponent */' + LineEnding +
'  scale *= pow(spot_cos, gl_LightSource[<Light>].spotExponent);' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_HAS_ATTENUATION' + LineEnding +
'  scale /= max(1.0,' + LineEnding +
'           gl_LightSource[<Light>].constantAttenuation +' + LineEnding +
'           gl_LightSource[<Light>].linearAttenuation * distance_to_light +' + LineEnding +
'           gl_LightSource[<Light>].quadraticAttenuation * distance_to_light * distance_to_light);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_HAS_RADIUS' + LineEnding +
'  if (distance_to_light >= castle_LightSource<Light>Radius)' + LineEnding +
'    scale = 0.0;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  /* add ambient term */' + LineEnding +
'  vec4 light_color =' + LineEnding +
'#ifdef LIGHT_HAS_AMBIENT' + LineEnding +
'  gl_SideLightProduct[<Light>].ambient;' + LineEnding +
'#else' + LineEnding +
'  vec4(0.0);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  /* add diffuse term */' + LineEnding +
'  vec4 diffuse = gl_SideLightProduct[<Light>].diffuse;' + LineEnding +
'  /* PLUG: material_light_diffuse (diffuse, vertex_eye, normal_eye, gl_LightSource[<Light>], material) */' + LineEnding +
'  float diffuse_factor = max(dot(normal_eye, light_dir), 0.0);' + LineEnding +
'  light_color += diffuse * diffuse_factor;' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_HAS_SPECULAR' + LineEnding +
'  /* add specular term */' + LineEnding +
'  if (diffuse_factor != 0.0)' + LineEnding +
'    light_color += gl_SideLightProduct[<Light>].specular *' + LineEnding +
'      pow(max(dot(vec3(gl_LightSource[<Light>].halfVector), normal_eye),' + LineEnding +
'        0.0), material.shininess);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  color += light_color * scale;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Not really necessary, but undefine just in case, just like OpenGL ES shader. */' + LineEnding +
'#undef LIGHT_TYPE_POSITIONAL' + LineEnding +
'#undef LIGHT_TYPE_SPOT' + LineEnding +
'#undef LIGHT_HAS_AMBIENT' + LineEnding +
'#undef LIGHT_HAS_SPECULAR' + LineEnding +
'#undef LIGHT_HAS_ATTENUATION' + LineEnding +
'#undef LIGHT_HAS_RADIUS' + LineEnding +
'#undef LIGHT_HAS_BEAM_WIDTH' + LineEnding +
'#undef LIGHT_HAS_SPOT_EXPONENT' + LineEnding +
''
