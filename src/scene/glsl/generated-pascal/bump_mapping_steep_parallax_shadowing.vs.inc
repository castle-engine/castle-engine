{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/bump_mapping_steep_parallax_shadowing.vs" }
'/* Steep parallax with self-shading bump mapping shader effect.' + LineEnding +
'   This is added right after bump_mapping.vs and bump_mapping_parallax.vs.' + LineEnding +
'   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'varying vec3 castle_light_direction_tangent_space;' + LineEnding +
'' + LineEnding +
'/* Note that there''s no need to protect castle_LightSource0Position' + LineEnding +
'   from redeclaring, which is usually a problem when' + LineEnding +
'   no "separate compilation units" are available (on OpenGLES).' + LineEnding +
'   In this particular case, using bump mapping forces Phong shading,' + LineEnding +
'   and then lights are calculated in the fragment shader.' + LineEnding +
'   The declaration below is in vertex shader, so it never conflicts. */' + LineEnding +
'#ifdef GL_ES' + LineEnding +
'uniform mediump vec3 castle_LightSource0Position;' + LineEnding +
'#else' + LineEnding +
'uniform vec3 castle_LightSource0Position;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void PLUG_bump_mapping_parallax_tangent_space_calculations(' + LineEnding +
'  const in vec4 vertex_eye, const in mat3 eye_to_tangent_space)' + LineEnding +
'{' + LineEnding +
'  vec3 light_dir = castle_LightSource0Position;' + LineEnding +
'#ifdef CASTLE_LIGHT0_POSITIONAL' + LineEnding +
'  light_dir -= vec3(vertex_eye);' + LineEnding +
'#endif' + LineEnding +
'  light_dir = normalize(light_dir);' + LineEnding +
'  castle_light_direction_tangent_space = eye_to_tangent_space * light_dir;' + LineEnding +
'}' + LineEnding +
''
