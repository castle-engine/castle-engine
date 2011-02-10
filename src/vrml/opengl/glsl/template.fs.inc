{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template.fs" }
'/* Generic GLSL fragment shader template. It will be used' + LineEnding + 
'   by vrmlshadergenerator.pas to construct final shader.' + LineEnding + 
'' + LineEnding + 
'   This is converted to template.fs.inc, and is them compiled' + LineEnding + 
'   in program''s binary.' + LineEnding + 
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'varying vec4 vertex_eye;' + LineEnding + 
'varying vec3 normal_eye;' + LineEnding + 
'' + LineEnding + 
'/* *** FRAGMENT-DECLARE *** */' + LineEnding + 
'' + LineEnding + 
'void add_light_contribution(inout vec4 color,' + LineEnding + 
'  const in vec3 normal_eye,' + LineEnding + 
'  const in gl_LightProducts light_products,' + LineEnding + 
'  const in gl_LightSourceParameters light_source,' + LineEnding + 
'  const in gl_MaterialParameters material)' + LineEnding + 
'{' + LineEnding + 
'  vec3 light_dir;' + LineEnding + 
'' + LineEnding + 
'  /* add ambient term */' + LineEnding + 
'  color += light_products.ambient;' + LineEnding + 
'' + LineEnding + 
'  /* add diffuse term */' + LineEnding + 
'  if (light_source.position.w != 0.0)' + LineEnding + 
'  {' + LineEnding + 
'    /* we assume in this case light_source.position.w == 1,' + LineEnding + 
'       so there''s no need to divide by it. This is true for our VRML/X3D' + LineEnding + 
'       lights. */' + LineEnding + 
'    /* positional light */' + LineEnding + 
'    light_dir = normalize(light_source.position.xyz - vec3(vertex_eye));' + LineEnding + 
'' + LineEnding + 
'    /* non-spot lights have always cutoff = 180, with cos = -1,' + LineEnding + 
'       so the check below will always be false. No need to explicitly' + LineEnding + 
'       compare with -1, nice. */' + LineEnding + 
'    if (dot(normalize(light_source.spotDirection), -light_dir) <' + LineEnding + 
'        light_source.spotCosCutoff)' + LineEnding + 
'      return;' + LineEnding + 
'  } else' + LineEnding + 
'  {' + LineEnding + 
'    /* directional light */' + LineEnding + 
'    light_dir = normalize(light_source.position.xyz);' + LineEnding + 
'  }' + LineEnding + 
'' + LineEnding + 
'  color += light_products.diffuse' + LineEnding + 
'    * max(dot(normal_eye, light_dir), 0.0);' + LineEnding + 
'' + LineEnding + 
'  /* add specular term */' + LineEnding + 
'  vec3 reflect = normalize(-reflect(light_dir, normal_eye));' + LineEnding + 
'  /* vertex to camera direction = camera pos - vertex pos.' + LineEnding + 
'     We work in eye space here, so camera pos = always zero. */' + LineEnding + 
'  vec3 vertex_to_camera_dir = normalize(-vec3(vertex_eye));' + LineEnding + 
'  color += light_products.specular' + LineEnding + 
'    * pow(max(dot(reflect, vertex_to_camera_dir), 0.0), material.shininess);' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  gl_FragColor = gl_FrontLightModelProduct.sceneColor;' + LineEnding + 
'' + LineEnding + 
'  if (gl_FrontFacing)' + LineEnding + 
'  {' + LineEnding + 
'    for (int i = 0; i < LIGHTS_ENABLED; i++)' + LineEnding + 
'      add_light_contribution(gl_FragColor, normal_eye,' + LineEnding + 
'        gl_FrontLightProduct[i], gl_LightSource[i], gl_FrontMaterial);' + LineEnding + 
'    /* Otherwise, alpha is usually large after previous add_light_contribution,' + LineEnding + 
'       and it''s always opaque */' + LineEnding + 
'    gl_FragColor.a = gl_FrontMaterial.diffuse.a;' + LineEnding + 
'  } else' + LineEnding + 
'  {' + LineEnding + 
'    for (int i = 0; i < LIGHTS_ENABLED; i++)' + LineEnding + 
'      add_light_contribution(gl_FragColor, -normal_eye,' + LineEnding + 
'        gl_BackLightProduct[i], gl_LightSource[i], gl_BackMaterial);' + LineEnding + 
'    gl_FragColor.a = gl_BackMaterial.diffuse.a;' + LineEnding + 
'  }' + LineEnding + 
'' + LineEnding + 
'  /* *** TEXTURE-APPLY *** */' + LineEnding + 
'}' + LineEnding + 
''
