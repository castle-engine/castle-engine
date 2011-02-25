{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template.fs" }
'/* Generic GLSL fragment shader template. It will be used' + LineEnding + 
'   by vrmlshadergenerator.pas to construct final shader.' + LineEnding + 
'' + LineEnding + 
'   This is converted to template.fs.inc, and is them compiled' + LineEnding + 
'   in program''s binary.' + LineEnding + 
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'varying vec4 vertex_eye;' + LineEnding + 
'varying vec3 normal_eye;' + LineEnding + 
'' + LineEnding + 
'/* PLUG: $declare-variables$ */' + LineEnding + 
'/* PLUG: $declare-shadow-map-procedures$ */' + LineEnding + 
'/* PLUG: $declare-forward-procedures$ */' + LineEnding + 
'/* PLUG: $declare-procedures$ */' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  gl_FragColor = gl_FrontLightModelProduct.sceneColor;' + LineEnding + 
'' + LineEnding + 
'  vec3 normal_eye_fragment = normalize(normal_eye);' + LineEnding + 
'  /* PLUG: fragment_normal_eye (normal_eye_fragment) */' + LineEnding + 
'  /* (inout vec3 normal_eye_fragment) */' + LineEnding + 
'' + LineEnding + 
'  if (gl_FrontFacing)' + LineEnding + 
'  {' + LineEnding + 
'    gl_FragColor += gl_FrontMaterial.emission;' + LineEnding + 
'    /* PLUG: add_light_contribution_front (gl_FragColor, vertex_eye, normal_eye_fragment, gl_FrontMaterial) */' + LineEnding + 
'' + LineEnding + 
'    /* Otherwise, alpha is usually large after previous add_light_contribution,' + LineEnding + 
'       and it''s always opaque.' + LineEnding + 
'       Using diffuse.a is actually exactly what fixed-function pipeline does' + LineEnding + 
'       too, according to http://www.sjbaker.org/steve/omniv/opengl_lighting.html */' + LineEnding + 
'    gl_FragColor.a = gl_FrontMaterial.diffuse.a;' + LineEnding + 
'  } else' + LineEnding + 
'  {' + LineEnding + 
'    gl_FragColor += gl_BackMaterial.emission;' + LineEnding + 
'    normal_eye_fragment = -normal_eye_fragment;' + LineEnding + 
'    /* PLUG: add_light_contribution_back (gl_FragColor, vertex_eye, normal_eye_fragment, gl_BackMaterial) */' + LineEnding + 
'    gl_FragColor.a = gl_BackMaterial.diffuse.a;' + LineEnding + 
'  }' + LineEnding + 
'' + LineEnding + 
'  /* NVidia GeForce 450 GTS (kocury) fails to compile a shader when' + LineEnding + 
'     we pass gl_FragColor as inout parameter (testcase even fresnel_and_toon.x3dv).' + LineEnding + 
'     Radeon X1600 (fglrx, chantal) works with it OK.' + LineEnding + 
'     For now, just the simplest workaround: use temp variably. */' + LineEnding + 
'' + LineEnding + 
'  vec4 fragment_color = gl_FragColor;' + LineEnding + 
'  /* PLUG: texture_apply (fragment_color, normal_eye_fragment) */' + LineEnding + 
'  /* PLUG: fog_apply (fragment_color, normal_eye_fragment) */' + LineEnding + 
'  gl_FragColor = fragment_color;' + LineEnding + 
'' + LineEnding + 
'  /* PLUG: fragment_end (gl_FragColor) */' + LineEnding + 
'}' + LineEnding + 
''
