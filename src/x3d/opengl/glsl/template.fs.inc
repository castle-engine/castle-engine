{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template.fs" }
'/* Generic GLSL fragment shader.' + LineEnding +
'   Used by ../castlerendererinternalshader.pas to construct the final shader.' + LineEnding +
'' + LineEnding +
'   This is converted to template.fs.inc, and is then compiled' + LineEnding +
'   in program''s binary.' + LineEnding +
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding +
'*/' + LineEnding +
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
'  vec3 normal_eye_fragment = normalize(castle_normal_eye);' + LineEnding +
'' + LineEnding +
'#ifndef CASTLE_BUGGY_FRONT_FACING' + LineEnding +
'  if (gl_FrontFacing)' + LineEnding +
'    /* Avoid AMD bug http://forums.amd.com/devforum/messageview.cfm?catid=392&threadid=148827&enterthread=y' + LineEnding +
'       Observed on fglrx (proprietary ATI Linux driver),' + LineEnding +
'       with ATI Mobility Radeon HD 4300 (castle computer "czarny"),' + LineEnding +
'       since Ubuntu 11.4 (fglrx OpenGL version 3.3.10665).' + LineEnding +
'' + LineEnding +
'       It causes both (gl_FrontFacing) and (!gl_FrontFacing) to be true...' + LineEnding +
'       To minimize the number of problems, never use "if (!gl_FrontFacing)",' + LineEnding +
'       only "if (gl_FrontFacing)".' + LineEnding +
'    */ ; else' + LineEnding +
'    normal_eye_fragment = -normal_eye_fragment;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  /* PLUG: fragment_eye_space (castle_vertex_eye, normal_eye_fragment) */' + LineEnding +
'' + LineEnding +
'#ifdef LIT' + LineEnding +
'  vec4 fragment_color;' + LineEnding +
'' + LineEnding +
'#ifndef CASTLE_BUGGY_FRONT_FACING' + LineEnding +
'  if (gl_FrontFacing)' + LineEnding +
'  {' + LineEnding +
'#endif' + LineEnding +
'    fragment_color = gl_FrontLightModelProduct.sceneColor;' + LineEnding +
'    /* PLUG: add_light_contribution_front (fragment_color, castle_vertex_eye, normal_eye_fragment, gl_FrontMaterial) */' + LineEnding +
'' + LineEnding +
'    /* Otherwise, alpha is usually large after previous add_light_contribution,' + LineEnding +
'       and it''s always opaque.' + LineEnding +
'       Using diffuse.a is actually exactly what fixed-function pipeline does' + LineEnding +
'       too, according to http://www.sjbaker.org/steve/omniv/opengl_lighting.html */' + LineEnding +
'    fragment_color.a = gl_FrontMaterial.diffuse.a;' + LineEnding +
'#ifndef CASTLE_BUGGY_FRONT_FACING' + LineEnding +
'  } else' + LineEnding +
'  {' + LineEnding +
'    fragment_color = gl_BackLightModelProduct.sceneColor;' + LineEnding +
'    /* PLUG: add_light_contribution_back (fragment_color, castle_vertex_eye, normal_eye_fragment, gl_BackMaterial) */' + LineEnding +
'    fragment_color.a = gl_BackMaterial.diffuse.a;' + LineEnding +
'  }' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.' + LineEnding +
'     This isn''t really mandatory, but scenes with many lights could easily' + LineEnding +
'     have colors > 1 and then the textures will look "burned out".' + LineEnding +
'     Of course, for future HDR rendering we will turn this off. */' + LineEnding +
'  fragment_color.rgb = min(fragment_color.rgb, 1.0);' + LineEnding +
'#else' + LineEnding +
'  vec4 fragment_color = gl_Color;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  /* PLUG: lighting_apply (fragment_color, castle_vertex_eye, normal_eye_fragment) */' + LineEnding +
'' + LineEnding +
'  /* PLUG: texture_apply (fragment_color, normal_eye_fragment) */' + LineEnding +
'  /* PLUG: steep_parallax_shadow_apply (fragment_color) */' + LineEnding +
'  /* PLUG: fog_apply (fragment_color, normal_eye_fragment) */' + LineEnding +
'' + LineEnding +
'  /* NVidia GeForce 450 GTS (kocury) fails to compile a shader when' + LineEnding +
'     we pass gl_FragColor as inout parameter to functions' + LineEnding +
'     (testcase even fresnel_and_toon.x3dv).' + LineEnding +
'     Although on Radeon X1600 (fglrx, chantal) it works OK.' + LineEnding +
'     So we just use fragment_color everywhere above, and only assign it' + LineEnding +
'     to gl_FragColor at the end. */' + LineEnding +
'  gl_FragColor = fragment_color;' + LineEnding +
'' + LineEnding +
'  /* PLUG: fragment_end (gl_FragColor) */' + LineEnding +
'}' + LineEnding +
''
