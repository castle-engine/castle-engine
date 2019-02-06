{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_phong.fs" }
'/* Phong shading GLSL fragment shader.' + LineEnding +
'   Used by ../castlerendererinternalshader.pas to construct the final shader.' + LineEnding +
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'#ifdef GL_ES' + LineEnding +
'precision mediump float;' + LineEnding +
'#endif' + LineEnding +
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
'uniform float castle_MaterialDiffuseAlpha;' + LineEnding +
'uniform float castle_MaterialShininess;' + LineEnding +
'/* Color summed with all the lights.' + LineEnding +
'   Like gl_Front/BackLightModelProduct.sceneColor:' + LineEnding +
'   material emissive color + material ambient color * global (light model) ambient.' + LineEnding +
'*/' + LineEnding +
'uniform vec3 castle_SceneColor;' + LineEnding +
'uniform vec4 castle_UnlitColor;' + LineEnding +
'' + LineEnding +
'#ifdef COLOR_PER_VERTEX' + LineEnding +
'varying vec4 castle_ColorPerVertexFragment;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'/* Include fragment shader utilities used by both Gouraud and Phong shading. */' + LineEnding +
'/* CASTLE-COMMON-CODE */' + LineEnding +
'' + LineEnding +
'#ifdef CASTLE_SEPARATE_DIFFUSE_TEXTURE' + LineEnding +
'void separate_diffuse_apply_texture(inout vec4 fragment_color,' + LineEnding +
'  const in vec4 vertex_eye,' + LineEnding +
'  const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'  /* PLUG: separate_diffuse_apply_texture (fragment_color, normal_eye) */' + LineEnding +
'}' + LineEnding +
'#endif' + LineEnding +
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
'' + LineEnding +
'       See also https://makc3d.wordpress.com/2015/09/17/alternative-to-gl_frontfacing/ ,' + LineEnding +
'       gl_FrontFacing seems unreliable sometimes.' + LineEnding +
'    */ ; else' + LineEnding +
'    normal_eye_fragment = -normal_eye_fragment;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  /* PLUG: fragment_eye_space (castle_vertex_eye, normal_eye_fragment) */' + LineEnding +
'' + LineEnding +
'#ifdef LIT' + LineEnding +
'  /* Comparing this with Gouraud shading (template_gouraud.vs/fs:' + LineEnding +
'     this variable performs both the role of castle_Color and fragment_color' + LineEnding +
'     from that GLSL code). */' + LineEnding +
'  vec4 fragment_color;' + LineEnding +
'' + LineEnding +
'  fragment_color = vec4(castle_SceneColor, 1.0);' + LineEnding +
'' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'    /* PLUG: add_light_contribution (fragment_color, castle_vertex_eye, normal_eye_fragment, castle_MaterialShininess, castle_ColorPerVertexFragment) */' + LineEnding +
'' + LineEnding +
'    /* Apply castle_ColorPerVertexFragment alpha here.' + LineEnding +
'       The RGB portion of castle_ColorPerVertexFragment was' + LineEnding +
'       already applied in template_light.glsl . */' + LineEnding +
'    fragment_color.a = castle_ColorPerVertexFragment.a;' + LineEnding +
'  #else' + LineEnding +
'    /* PLUG: add_light_contribution (fragment_color, castle_vertex_eye, normal_eye_fragment, castle_MaterialShininess, vec4(0.0)) */' + LineEnding +
'' + LineEnding +
'    /* Apply alpha. Otherwise, alpha is usually large after previous add_light_contribution,' + LineEnding +
'       and it''s always opaque.' + LineEnding +
'       Using diffuse.a is actually exactly what fixed-function pipeline does' + LineEnding +
'       too, according to http://www.sjbaker.org/steve/omniv/opengl_lighting.html */' + LineEnding +
'    fragment_color.a = castle_MaterialDiffuseAlpha;' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.' + LineEnding +
'     This isn''t really mandatory, but scenes with many lights could easily' + LineEnding +
'     have colors > 1 and then the textures will look "burned out".' + LineEnding +
'     Of course, for future HDR rendering we will turn this off. */' + LineEnding +
'  fragment_color.rgb = min(fragment_color.rgb, 1.0);' + LineEnding +
'#else' + LineEnding +
'  vec4 fragment_color = castle_UnlitColor' + LineEnding +
'#ifdef COLOR_PER_VERTEX' + LineEnding +
'    /* Apply COLOR_PER_VERTEX, when unlit.' + LineEnding +
'       (When lit, then the analogous multiplication is done' + LineEnding +
'        inside template_add_light.glsl) */' + LineEnding +
'    * castle_ColorPerVertexFragment' + LineEnding +
'#endif' + LineEnding +
'  ;' + LineEnding +
'' + LineEnding +
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
