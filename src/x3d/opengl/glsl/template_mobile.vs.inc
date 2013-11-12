{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_mobile.vs" }
'/* Generic GLSL vertex shader, used on OpenGL ES. */' + LineEnding +
'' + LineEnding +
'uniform mat4 castle_ModelViewMatrix;' + LineEnding +
'uniform mat4 castle_ProjectionMatrix;' + LineEnding +
'uniform mat3 castle_NormalMatrix;' + LineEnding +
'attribute vec4 castle_Vertex;' + LineEnding +
'attribute vec3 castle_Normal;' + LineEnding +
'' + LineEnding +
'/* PLUG-DECLARATIONS */' + LineEnding +
'' + LineEnding +
'varying vec4 castle_vertex_eye;' + LineEnding +
'varying vec3 castle_normal_eye;' + LineEnding +
'varying vec4 castle_Color;' + LineEnding +
'' + LineEnding +
'/* Light source position in eye coordinates. */' + LineEnding +
'const vec3 castle_LightSource0Position = vec3(0.0, 0.0, 0.0);' + LineEnding +
'const float castle_LightSource0spotExponent = 16.0;' + LineEnding +
'/* Multiplied colors of light source and material. */' + LineEnding +
'const vec4 castle_SideLightProduct0Ambient  = vec4(0.1, 0.1, 0.1, 1.0);' + LineEnding +
'const vec4 castle_SideLightProduct0Diffuse  = vec4(1.0, 1.0, 1.0, 1.0);' + LineEnding +
'const vec4 castle_SideLightProduct0Specular = vec4(1.0, 1.0, 1.0, 1.0);' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  vec4 vertex_object = castle_Vertex;' + LineEnding +
'  vec3 normal_object = castle_Normal;' + LineEnding +
'  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */' + LineEnding +
'  /* PLUG: vertex_object_space (vertex_object, normal_object) */' + LineEnding +
'' + LineEnding +
'  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;' + LineEnding +
'  castle_normal_eye = normalize(castle_NormalMatrix * normal_object);' + LineEnding +
'' + LineEnding +
'  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */' + LineEnding +
'' + LineEnding +
'  /* Simple lighting calculation for a single positional light source. */' + LineEnding +
'  vec3 light_dir = normalize(castle_LightSource0Position - vec3(castle_vertex_eye));' + LineEnding +
'  vec3 reflect_dir = reflect(-light_dir, castle_normal_eye);' + LineEnding +
'  vec3 view_dir = normalize(-vec3(castle_vertex_eye));' + LineEnding +
'  float diffuse = max(dot(light_dir, castle_normal_eye), 0.0);' + LineEnding +
'  float spec = 0.0;' + LineEnding +
'  if (diffuse > 0.0) {' + LineEnding +
'      spec = max(dot(reflect_dir, view_dir), 0.0);' + LineEnding +
'      spec = pow(spec, castle_LightSource0spotExponent);' + LineEnding +
'  }' + LineEnding +
'  castle_Color = castle_SideLightProduct0Ambient +' + LineEnding +
'    castle_SideLightProduct0Diffuse * diffuse +' + LineEnding +
'    castle_SideLightProduct0Specular * spec;' + LineEnding +
'' + LineEnding +
'  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;' + LineEnding +
'}' + LineEnding +
''
