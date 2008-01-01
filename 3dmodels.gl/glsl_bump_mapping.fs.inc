{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "glsl_bump_mapping.fs" }
'/* GLSL fragment shader to do bump mapping.' + LineEnding + 
'' + LineEnding + 
'   This is converted to glsl_bump_mapping.fs.inc, and is them compiled' + LineEnding + 
'   in program''s binary.' + LineEnding + 
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'uniform sampler2D tex_normal_map;' + LineEnding + 
'uniform sampler2D tex_original;' + LineEnding + 
'' + LineEnding + 
'varying vec3 interpolated_normal;' + LineEnding + 
'varying vec3 vertex_world;' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  /* TODO: take this from VRMLOpenGLRenderer.Attributes */' + LineEnding + 
'  vec3 light_position = gl_LightSource[0].position.xyz;' + LineEnding + 
'  vec3 light_dir = normalize(light_position - vertex_world);' + LineEnding + 
'  vec3 normal = normalize(interpolated_normal);' + LineEnding + 
'' + LineEnding + 
'  /* gl_FragColor = lighting computed mostly just like using OpenGL' + LineEnding + 
'     fixed-function pipeline, but per-fragment. */' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor =' + LineEnding + 
'    gl_FrontLightModelProduct.sceneColor +' + LineEnding + 
'    gl_FrontLightProduct[0].ambient +' + LineEnding + 
'    gl_FrontLightProduct[0].diffuse * max(dot(normal, light_dir), 0.0);' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor *= texture2D(tex_original, gl_TexCoord[0].st);' + LineEnding + 
'}' + LineEnding + 
''
