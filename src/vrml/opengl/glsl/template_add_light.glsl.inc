{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_add_light.glsl" }
'/* Shader code used for adding light source contribution. */' + LineEnding + 
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
'  light_dir = normalize(gl_LightSource[light_number].position.xyz' + LineEnding + 
'#ifdef LIGHT_TYPE_POSITIONAL' + LineEnding + 
'    /* positional light */' + LineEnding + 
'    /* we assume in this case gl_LightSource[light_number].position.w == 1,' + LineEnding + 
'       so there''s no need to divide by it. This is true for our VRML/X3D' + LineEnding + 
'       lights. */' + LineEnding + 
'    - vec3(vertex_eye)' + LineEnding + 
'#endif' + LineEnding + 
'  );' + LineEnding + 
'' + LineEnding + 
'#ifdef LIGHT_TYPE_SPOT' + LineEnding + 
'  /* Check gl_LightSource[light_number].position first, as we want to add nothing' + LineEnding + 
'     (not even ambient term) when were outside of spot light cone. */' + LineEnding + 
'' + LineEnding + 
'  /* non-spot lights have always cutoff = 180, with cos = -1,' + LineEnding + 
'     so the check below will always be false. No need to explicitly' + LineEnding + 
'     compare with -1, nice. */' + LineEnding + 
'  if (dot(normalize(gl_LightSource[light_number].spotDirection), -light_dir) <' + LineEnding + 
'      gl_LightSource[light_number].spotCosCutoff)' + LineEnding + 
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
'  /* add ambient term */' + LineEnding + 
'  vec4 light_color =' + LineEnding + 
'#ifdef LIGHT_HAS_AMBIENT' + LineEnding + 
'  gl_SideLightProduct[light_number].ambient;' + LineEnding + 
'#else' + LineEnding + 
'  vec4(0.0);' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'  /* add diffuse term */' + LineEnding + 
'  light_color += gl_SideLightProduct[light_number].diffuse' + LineEnding + 
'    * max(dot(normal_eye, light_dir), 0.0);' + LineEnding + 
'' + LineEnding + 
'  /* add specular term */' + LineEnding + 
'#ifdef LIGHT_HAS_SPECULAR' + LineEnding + 
'  vec3 reflect = normalize(-reflect(light_dir, normal_eye));' + LineEnding + 
'  /* vertex to camera direction = camera pos - vertex pos.' + LineEnding + 
'     We work in eye space here, so camera pos = always zero. */' + LineEnding + 
'  vec3 vertex_to_camera_dir = normalize(-vec3(vertex_eye));' + LineEnding + 
'  light_color += gl_SideLightProduct[light_number].specular' + LineEnding + 
'    * pow(max(dot(reflect, vertex_to_camera_dir), 0.0), material.shininess);' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'  color += light_color * scale;' + LineEnding + 
'' + LineEnding + 
'#undef LIGHT_TYPE_POSITIONAL' + LineEnding + 
'#undef LIGHT_TYPE_SPOT' + LineEnding + 
'#undef LIGHT_TYPE_KNOWN' + LineEnding + 
'#undef LIGHT_HAS_AMBIENT' + LineEnding + 
'#undef LIGHT_HAS_SPECULAR' + LineEnding + 
'' + LineEnding + 
'}' + LineEnding + 
''
