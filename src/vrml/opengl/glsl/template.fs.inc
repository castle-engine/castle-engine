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
'/* *** FRAGMENT-SHADER-DECLARE *** */' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  gl_FragColor = gl_FrontLightModelProduct.sceneColor;' + LineEnding + 
'' + LineEnding + 
'/* TODO: We assume we have constant num of lights' + LineEnding + 
'   TODO: two-side lighting? */' + LineEnding + 
'' + LineEnding + 
'  for (int i = 0; i < 1; i++)' + LineEnding + 
'  {' + LineEnding + 
'    /* add ambient term */' + LineEnding + 
'    gl_FragColor += gl_FrontLightProduct[i].ambient;' + LineEnding + 
'' + LineEnding + 
'    /* add diffuse term */' + LineEnding + 
'    /* TODO: assume directional light.' + LineEnding + 
'       TODO: in what coords is gl_LightSource[i].position? */' + LineEnding + 
'    vec3 light_dir = gl_LightSource[i].position.xyz;' + LineEnding + 
'    gl_FragColor += gl_FrontLightProduct[i].diffuse' + LineEnding + 
'      * max(dot(normal_eye, light_dir), 0.0);' + LineEnding + 
'' + LineEnding + 
'    /* add specular term */' + LineEnding + 
'    vec3 reflect = normalize(-reflect(light_dir, normal_eye));' + LineEnding + 
'    /* vertex to camera direction = camera pos - vertex pos.' + LineEnding + 
'       We work in eye space here, so camera pos = always zero. */' + LineEnding + 
'    vec3 vertex_to_camera_dir = normalize(-vec3(vertex_eye));' + LineEnding + 
'    gl_FragColor += gl_FrontLightProduct[i].specular' + LineEnding + 
'      * pow(max(dot(reflect, vertex_to_camera_dir), 0.0),' + LineEnding + 
'            gl_FrontMaterial.shininess);' + LineEnding + 
'  }' + LineEnding + 
'' + LineEnding + 
'  /* *** TEXTURE-APPLY *** */' + LineEnding + 
'}' + LineEnding + 
''
