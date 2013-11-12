{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_mobile_add_light.glsl" }
'/* Shader code used for adding light source contribution, for OpenGL ES. */' + LineEnding +
'' + LineEnding +
'/* Light source position in eye coordinates. */' + LineEnding +
'const vec3 castle_LightSource<Light>Position = vec3(0.0, 0.0, 0.0);' + LineEnding +
'uniform float castle_LightSource<Light>SpotExponent;' + LineEnding +
'/* Multiplied colors of light source and material. */' + LineEnding +
'uniform vec4 castle_SideLightProduct<Light>Ambient;' + LineEnding +
'uniform vec4 castle_SideLightProduct<Light>Diffuse;' + LineEnding +
'uniform vec4 castle_SideLightProduct<Light>Specular;' + LineEnding +
'' + LineEnding +
'void PLUG_add_light_contribution(inout vec4 color,' + LineEnding +
'  const in vec4 vertex_eye,' + LineEnding +
'  const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'  vec3 light_dir = normalize(castle_LightSource<Light>Position - vec3(vertex_eye));' + LineEnding +
'  vec3 reflect_dir = reflect(-light_dir, normal_eye);' + LineEnding +
'  vec3 view_dir = normalize(-vec3(vertex_eye));' + LineEnding +
'  float diffuse = max(dot(light_dir, normal_eye), 0.0);' + LineEnding +
'  float spec = 0.0;' + LineEnding +
'  if (diffuse > 0.0) {' + LineEnding +
'      spec = max(dot(reflect_dir, view_dir), 0.0);' + LineEnding +
'      spec = pow(spec, castle_LightSource<Light>SpotExponent);' + LineEnding +
'  }' + LineEnding +
'  color += castle_SideLightProduct<Light>Ambient +' + LineEnding +
'    castle_SideLightProduct<Light>Diffuse * diffuse +' + LineEnding +
'    castle_SideLightProduct<Light>Specular * spec;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Undefine lights symbols, since for OpenGL ES all the shader parts' + LineEnding +
'   are concatenated into a single string. */' + LineEnding +
'#undef LIGHT_TYPE_POSITIONAL' + LineEnding +
'#undef LIGHT_TYPE_SPOT' + LineEnding +
'#undef LIGHT_HAS_AMBIENT' + LineEnding +
'#undef LIGHT_HAS_SPECULAR' + LineEnding +
'#undef LIGHT_HAS_ATTENUATION' + LineEnding +
'#undef LIGHT_HAS_RADIUS' + LineEnding +
''
