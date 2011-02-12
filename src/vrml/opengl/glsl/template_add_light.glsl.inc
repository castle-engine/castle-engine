{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_add_light.glsl" }
'/* This shader code will be used for adding light source contribution. */' + LineEnding + 
'' + LineEnding + 
'vec3 light_dir;' + LineEnding + 
'' + LineEnding + 
'/* Check light_source.position first, as we want to add nothing' + LineEnding + 
'   (not even ambient term) when were outside of spot light cone. */' + LineEnding + 
'if (light_source.position.w != 0.0)' + LineEnding + 
'{' + LineEnding + 
'  /* we assume in this case light_source.position.w == 1,' + LineEnding + 
'     so there''s no need to divide by it. This is true for our VRML/X3D' + LineEnding + 
'     lights. */' + LineEnding + 
'  /* positional light */' + LineEnding + 
'  light_dir = normalize(light_source.position.xyz - vec3(vertex_eye));' + LineEnding + 
'' + LineEnding + 
'  /* non-spot lights have always cutoff = 180, with cos = -1,' + LineEnding + 
'     so the check below will always be false. No need to explicitly' + LineEnding + 
'     compare with -1, nice. */' + LineEnding + 
'  if (dot(normalize(light_source.spotDirection), -light_dir) <' + LineEnding + 
'      light_source.spotCosCutoff)' + LineEnding + 
'    return;' + LineEnding + 
'} else' + LineEnding + 
'{' + LineEnding + 
'  /* directional light */' + LineEnding + 
'  light_dir = normalize(light_source.position.xyz);' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'float scale = 1.0;' + LineEnding + 
'/* PLUG: light-scale (scale) (inout float scale) */' + LineEnding + 
'' + LineEnding + 
'/* add ambient term */' + LineEnding + 
'vec4 light_color = light_products.ambient;' + LineEnding + 
'' + LineEnding + 
'/* add diffuse term */' + LineEnding + 
'light_color += light_products.diffuse' + LineEnding + 
'  * max(dot(normal_eye, light_dir), 0.0);' + LineEnding + 
'' + LineEnding + 
'/* add specular term */' + LineEnding + 
'vec3 reflect = normalize(-reflect(light_dir, normal_eye));' + LineEnding + 
'/* vertex to camera direction = camera pos - vertex pos.' + LineEnding + 
'   We work in eye space here, so camera pos = always zero. */' + LineEnding + 
'vec3 vertex_to_camera_dir = normalize(-vec3(vertex_eye));' + LineEnding + 
'light_color += light_products.specular' + LineEnding + 
'  * pow(max(dot(reflect, vertex_to_camera_dir), 0.0), material.shininess);' + LineEnding + 
'' + LineEnding + 
'color += light_color;' + LineEnding + 
''
