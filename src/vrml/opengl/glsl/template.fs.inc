{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template.fs" }
'/* Generic GLSL fragment shader template. It will be used' + LineEnding + 
'   by vrmlshadergenerator.pas to construct final shader.' + LineEnding + 
'' + LineEnding + 
'   This is converted to template.fs.inc, and is then compiled' + LineEnding + 
'   in program''s binary.' + LineEnding + 
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'varying vec4 kambi_vertex_eye;' + LineEnding + 
'varying vec3 kambi_normal_eye;' + LineEnding + 
'' + LineEnding + 
'/* Forward declare shadow maps utilities. */' + LineEnding + 
'float shadow(sampler2DShadow shadowMap, vec4 shadowMapCoord,' + LineEnding + 
'  const in float size);' + LineEnding + 
'float shadow_depth(sampler2D shadowMap, vec4 shadowMapCoord);' + LineEnding + 
'' + LineEnding + 
'/* PLUG-DECLARATIONS */' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  vec3 normal_eye_fragment = normalize(kambi_normal_eye);' + LineEnding + 
'  /* PLUG: fragment_eye_space (kambi_vertex_eye, normal_eye_fragment) */' + LineEnding + 
'' + LineEnding + 
'#ifdef LIT' + LineEnding + 
'  vec4 fragment_color;' + LineEnding + 
'' + LineEnding + 
'  if (gl_FrontFacing)' + LineEnding + 
'  {' + LineEnding + 
'    fragment_color = gl_FrontLightModelProduct.sceneColor;' + LineEnding + 
'    /* PLUG: add_light_contribution_front (fragment_color, kambi_vertex_eye, normal_eye_fragment, gl_FrontMaterial) */' + LineEnding + 
'' + LineEnding + 
'    /* Otherwise, alpha is usually large after previous add_light_contribution,' + LineEnding + 
'       and it''s always opaque.' + LineEnding + 
'       Using diffuse.a is actually exactly what fixed-function pipeline does' + LineEnding + 
'       too, according to http://www.sjbaker.org/steve/omniv/opengl_lighting.html */' + LineEnding + 
'    fragment_color.a = gl_FrontMaterial.diffuse.a;' + LineEnding + 
'  } else' + LineEnding + 
'  {' + LineEnding + 
'    fragment_color = gl_BackLightModelProduct.sceneColor;' + LineEnding + 
'    normal_eye_fragment = -normal_eye_fragment;' + LineEnding + 
'    /* PLUG: add_light_contribution_back (fragment_color, kambi_vertex_eye, normal_eye_fragment, gl_BackMaterial) */' + LineEnding + 
'    fragment_color.a = gl_BackMaterial.diffuse.a;' + LineEnding + 
'  }' + LineEnding + 
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
'  /* PLUG: lighting_apply (fragment_color, kambi_vertex_eye, normal_eye_fragment) */' + LineEnding + 
'' + LineEnding + 
'#ifdef HAS_TEXTURE_COORD_SHIFT' + LineEnding + 
'  vec2 texture_coord_shift = vec2(0.0);' + LineEnding + 
'  /* PLUG: texture_coord_shift (texture_coord_shift, kambi_vertex_eye) */' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'  /* PLUG: texture_apply (fragment_color, normal_eye_fragment) */' + LineEnding + 
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
