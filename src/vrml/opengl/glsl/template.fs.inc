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
'/* PLUG: fragment-declare-variables declaration */' + LineEnding + 
'/* PLUG: fragment-declare-procedures declaration */' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  gl_FragColor = gl_FrontLightModelProduct.sceneColor;' + LineEnding + 
'' + LineEnding + 
'  vec3 normal_eye_fragment = normalize(normal_eye);' + LineEnding + 
'  /* PLUG: fragment-normal-eye (normal_eye_fragment) (inout vec3 normal_eye_fragment) */' + LineEnding + 
'' + LineEnding + 
'  if (gl_FrontFacing)' + LineEnding + 
'  {' + LineEnding + 
'    /* PLUG: add-light-contribution-front (gl_FragColor, normal_eye_fragment, gl_FrontMaterial) (inout vec4 color, const in vec3 normal_eye, const in gl_MaterialParameters material) */' + LineEnding + 
'' + LineEnding + 
'    /* Otherwise, alpha is usually large after previous add_light_contribution,' + LineEnding + 
'       and it''s always opaque.' + LineEnding + 
'       Using diffuse.a is actually exactly what fixed-function pipeline does' + LineEnding + 
'       too, according to http://www.sjbaker.org/steve/omniv/opengl_lighting.html */' + LineEnding + 
'    gl_FragColor.a = gl_FrontMaterial.diffuse.a;' + LineEnding + 
'  } else' + LineEnding + 
'  {' + LineEnding + 
'    normal_eye_fragment = -normal_eye_fragment;' + LineEnding + 
'    /* PLUG: add-light-contribution-back (gl_FragColor, normal_eye_fragment, gl_BackMaterial) (inout vec4 color, const in vec3 normal_eye, const in gl_MaterialParameters material) */' + LineEnding + 
'    gl_FragColor.a = gl_BackMaterial.diffuse.a;' + LineEnding + 
'  }' + LineEnding + 
'' + LineEnding + 
'  /* PLUG: texture-apply (gl_FragColor) (inout vec4 fragment_color) */' + LineEnding + 
'' + LineEnding + 
'  /* PLUG: fragment-end (gl_FragColor) (const in vec4 fragment_color) */' + LineEnding + 
'}' + LineEnding + 
''
