{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/bump_mapping.fs" }
'/* Bump mapping shader effect.' + LineEnding +
'   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'varying mat3 castle_tangent_to_eye_space;' + LineEnding +
'' + LineEnding +
'// declare castle_normal_map, avoiding redeclaring it for GL_ES' + LineEnding +
'#ifdef GL_ES' + LineEnding +
'  #ifndef castle_normal_map_defined' + LineEnding +
'  #define castle_normal_map_defined' + LineEnding +
'  uniform sampler2D castle_normal_map;' + LineEnding +
'  uniform float castle_normalScale;' + LineEnding +
'  #endif' + LineEnding +
'#else' + LineEnding +
'  uniform sampler2D castle_normal_map;' + LineEnding +
'  uniform float castle_normalScale;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'// avoid redeclaring for GL_ES' + LineEnding +
'#ifndef GL_ES' + LineEnding +
'varying vec4 castle_TexCoord<NormalMapTextureCoordinatesId>;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void PLUG_fragment_eye_space(const vec4 vertex, inout vec3 normal_eye_fragment)' + LineEnding +
'{' + LineEnding +
'  // Read normal from the texture (the core idea of bump mapping).' + LineEnding +
'  // Unpack normals, they are in texture in [0..1] range and we want in [-1..1].' + LineEnding +
'  vec3 normal_tangent = texture2D(castle_normal_map,' + LineEnding +
'    castle_TexCoord<NormalMapTextureCoordinatesId>.st).xyz * 2.0 - vec3(1.0);' + LineEnding +
'' + LineEnding +
'  normal_tangent *= vec3(castle_normalScale, castle_normalScale, 1.0);' + LineEnding +
'' + LineEnding +
'  /* We have to take two-sided lighting into account here, in tangent space.' + LineEnding +
'     Simply negating whole normal in eye space (like we do without bump mapping)' + LineEnding +
'     would not work good,' + LineEnding +
'     check e.g. insides of demo_models/bump_mapping/room_for_parallax_final.wrl. */' + LineEnding +
'  if (gl_FrontFacing)' + LineEnding +
'    /* Avoid AMD bug http://forums.amd.com/devforum/messageview.cfm?catid=392&threadid=148827&enterthread=y' + LineEnding +
'       It causes both (gl_FrontFacing) and (!gl_FrontFacing) to be true...' + LineEnding +
'       To minimize the number of problems, never use "if (!gl_FrontFacing)",' + LineEnding +
'       only "if (gl_FrontFacing)".' + LineEnding +
'       See template_phong.fs for more comments.' + LineEnding +
'    */ ; else' + LineEnding +
'    normal_tangent.z = -normal_tangent.z;' + LineEnding +
'' + LineEnding +
'  normal_eye_fragment = normalize(castle_tangent_to_eye_space * normal_tangent);' + LineEnding +
'}' + LineEnding +
''
