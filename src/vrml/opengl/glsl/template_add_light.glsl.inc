{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_add_light.glsl" }
'/* Shader code used for adding light source contribution. */' + LineEnding + 
'' + LineEnding + 
'#ifdef LIGHT_HAS_RADIUS' + LineEnding + 
'uniform float kambi_light_light_number_radius;' + LineEnding + 
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
'#ifdef LIGHT_TYPE_KNOWN' + LineEnding + 
'' + LineEnding + 
'#ifdef LIGHT_TYPE_POSITIONAL' + LineEnding + 
'  /* positional light. We assume in this case' + LineEnding + 
'     gl_LightSource[light_number].position.w == 1, so there''s no need' + LineEnding + 
'     to divide by it. This is true for our VRML/X3D lights. */' + LineEnding + 
'  light_dir = gl_LightSource[light_number].position.xyz - vec3(vertex_eye);' + LineEnding + 
'  float distance_to_light = length(light_dir);' + LineEnding + 
'  light_dir /= distance_to_light;' + LineEnding + 
'#else' + LineEnding + 
'  light_dir = normalize(gl_LightSource[light_number].position.xyz);' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'#ifdef LIGHT_TYPE_SPOT' + LineEnding + 
'  /* Check gl_LightSource[light_number].position first, as we want to add nothing' + LineEnding + 
'     (not even ambient term) when were outside of spot light cone. */' + LineEnding + 
'' + LineEnding + 
'  float spot_cos = dot(normalize(gl_LightSource[light_number].spotDirection), -light_dir);' + LineEnding + 
'  /* non-spot lights have always cutoff = 180, with cos = -1,' + LineEnding + 
'     so the check below will always be false. No need to explicitly' + LineEnding + 
'     compare with -1, nice. */' + LineEnding + 
'  if (spot_cos < gl_LightSource[light_number].spotCosCutoff)' + LineEnding + 
'    return;' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'#else' + LineEnding + 
'' + LineEnding + 
'  /* When light type is not known (this happens for lights set outside' + LineEnding + 
'     of TVRMLGLRenderer), we use less efficient code, that actually detects' + LineEnding + 
'     light type. */' + LineEnding + 
'  if (gl_LightSource[light_number].position.w != 0.0)' + LineEnding + 
'  {' + LineEnding + 
'    light_dir = normalize(gl_LightSource[light_number].position.xyz - vec3(vertex_eye));' + LineEnding + 
'    if (dot(normalize(gl_LightSource[light_number].spotDirection), -light_dir) <' + LineEnding + 
'        gl_LightSource[light_number].spotCosCutoff)' + LineEnding + 
'      return;' + LineEnding + 
'  } else' + LineEnding + 
'  {' + LineEnding + 
'    light_dir = normalize(gl_LightSource[light_number].position.xyz);' + LineEnding + 
'  }' + LineEnding + 
'' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'  float scale = 1.0;' + LineEnding + 
'  /* PLUG: light_scale (scale, normal_eye, light_dir, gl_LightSource[light_number], gl_SideLightProduct[light_number], material) */' + LineEnding + 
'' + LineEnding + 
'#ifdef LIGHT_TYPE_SPOT' + LineEnding + 
'  scale *= pow(spot_cos, gl_LightSource[light_number].spotExponent);' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'#ifdef LIGHT_HAS_ATTENUATION' + LineEnding + 
'  scale /= gl_LightSource[light_number].constantAttenuation +' + LineEnding + 
'           gl_LightSource[light_number].linearAttenuation * distance_to_light +' + LineEnding + 
'           gl_LightSource[light_number].quadraticAttenuation * distance_to_light * distance_to_light;' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'#ifdef LIGHT_HAS_RADIUS' + LineEnding + 
'  if (distance_to_light >= kambi_light_light_number_radius)' + LineEnding + 
'    scale = 0.0;' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'  /* add ambient term */' + LineEnding + 
'  vec4 light_color =' + LineEnding + 
'#ifdef LIGHT_HAS_AMBIENT' + LineEnding + 
'  gl_SideLightProduct[light_number].ambient;' + LineEnding + 
'#else' + LineEnding + 
'  vec4(0.0);' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'  /* add diffuse term */' + LineEnding + 
'  vec4 diffuse = gl_SideLightProduct[light_number].diffuse;' + LineEnding + 
'  /* PLUG: material_light_diffuse (diffuse, vertex_eye, normal_eye, gl_LightSource[light_number], material) */' + LineEnding + 
'  float diffuse_factor = max(dot(normal_eye, light_dir), 0.0);' + LineEnding + 
'  light_color += diffuse * diffuse_factor;' + LineEnding + 
'' + LineEnding + 
'#ifdef LIGHT_HAS_SPECULAR' + LineEnding + 
'  /* add specular term */' + LineEnding + 
'  if (diffuse_factor != 0.0)' + LineEnding + 
'    light_color += gl_SideLightProduct[light_number].specular *' + LineEnding + 
'      pow(max(dot(vec3(gl_LightSource[light_number].halfVector), normal_eye),' + LineEnding + 
'        0.0), material.shininess);' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'  color += light_color * scale;' + LineEnding + 
'' + LineEnding + 
'#undef LIGHT_TYPE_POSITIONAL' + LineEnding + 
'#undef LIGHT_TYPE_SPOT' + LineEnding + 
'#undef LIGHT_TYPE_KNOWN' + LineEnding + 
'#undef LIGHT_HAS_AMBIENT' + LineEnding + 
'#undef LIGHT_HAS_SPECULAR' + LineEnding + 
'#undef LIGHT_HAS_ATTENUATION' + LineEnding + 
'#undef LIGHT_HAS_RADIUS' + LineEnding + 
'' + LineEnding + 
'}' + LineEnding + 
''
