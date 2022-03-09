{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/shadow_map_generate.vs" }
'/* Vertex shader when generating shadow maps. */' + LineEnding +
'' + LineEnding +
'uniform mat4 castle_ModelViewMatrix;' + LineEnding +
'uniform mat4 castle_ProjectionMatrix;' + LineEnding +
'attribute vec4 castle_Vertex;' + LineEnding +
'' + LineEnding +
'#ifdef ALPHA_TEST' + LineEnding +
'attribute vec4 castle_MultiTexCoord0;' + LineEnding +
'varying vec2 castle_TexCoord0_XY;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_Position = castle_ProjectionMatrix * (castle_ModelViewMatrix * castle_Vertex);' + LineEnding +
'' + LineEnding +
'  #ifdef ALPHA_TEST' + LineEnding +
'  castle_TexCoord0_XY = castle_MultiTexCoord0.xy;' + LineEnding +
'  #endif' + LineEnding +
'}' + LineEnding +
''
