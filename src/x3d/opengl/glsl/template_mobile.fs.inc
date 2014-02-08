{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_mobile.fs" }
'/* Generic GLSL fragment shader, used on OpenGL ES. */' + LineEnding +
'' + LineEnding +
'precision mediump float;' + LineEnding +
'' + LineEnding +
'/* PLUG-DECLARATIONS */' + LineEnding +
'' + LineEnding +
'#ifdef HAS_GEOMETRY_SHADER' + LineEnding +
'  #define castle_vertex_eye castle_vertex_eye_geoshader' + LineEnding +
'  #define castle_normal_eye castle_normal_eye_geoshader' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'varying vec4 castle_vertex_eye;' + LineEnding +
'varying vec3 castle_normal_eye;' + LineEnding +
'varying vec4 castle_Color;' + LineEnding +
'' + LineEnding +
'/* Wrapper for calling PLUG texture_coord_shift */' + LineEnding +
'vec2 texture_coord_shifted(in vec2 tex_coord)' + LineEnding +
'{' + LineEnding +
'  /* PLUG: texture_coord_shift (tex_coord) */' + LineEnding +
'  return tex_coord;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  vec4 fragment_color = castle_Color;' + LineEnding +
'' + LineEnding +
'/* Fragment shader on mobile doesn''t get a normal vector now, for speed. */' + LineEnding +
'#define normal_eye_fragment vec3(0.0)' + LineEnding +
'' + LineEnding +
'  /* PLUG: texture_apply (fragment_color, normal_eye_fragment) */' + LineEnding +
'  /* PLUG: steep_parallax_shadow_apply (fragment_color) */' + LineEnding +
'  /* PLUG: fog_apply (fragment_color, normal_eye_fragment) */' + LineEnding +
'' + LineEnding +
'#undef normal_eye_fragment' + LineEnding +
'' + LineEnding +
'  gl_FragColor = fragment_color;' + LineEnding +
'' + LineEnding +
'  /* PLUG: fragment_end (gl_FragColor) */' + LineEnding +
'}' + LineEnding +
''
