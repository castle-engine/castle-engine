{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "glsl_bump_mapping.fs" }
'/* GLSL fragment shader to do bump mapping.' + LineEnding + 
'' + LineEnding + 
'   This is converted to glsl_bump_mapping.fs.inc, and is them compiled' + LineEnding + 
'   in program''s binary.' + LineEnding + 
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'uniform sampler2D tex_normal_map;' + LineEnding + 
'uniform sampler2D tex_original;' + LineEnding + 
'uniform vec4 light_ambient_color;' + LineEnding + 
'uniform vec4 light_diffuse_color;' + LineEnding + 
'' + LineEnding + 
'varying vec3 light_dir_tangent;' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  /* gl_FragColor = all ambient lighting. */' + LineEnding + 
'  gl_FragColor =' + LineEnding + 
'    gl_FrontLightModelProduct.sceneColor +' + LineEnding + 
'    light_ambient_color * gl_FrontMaterial.ambient;' + LineEnding + 
'' + LineEnding + 
'  /* Both light_dir and normal are in tangent space. */' + LineEnding + 
'  vec3 light_dir = normalize(light_dir_tangent);' + LineEnding + 
'' + LineEnding + 
'  /* I read normal from texture, this is the very idea of bump mapping.' + LineEnding + 
'     Unpack normals, they are in texture in [0..1] range and I want in [-1..1]. */' + LineEnding + 
'  vec3 normal = vec3(' + LineEnding + 
'    texture2D(tex_normal_map, gl_TexCoord[0].st)) * 2.0 - vec3(1, 1, 1);' + LineEnding + 
'' + LineEnding + 
'  /* I want to do two-sided lighting, so I want to have normal' + LineEnding + 
'     pointing from this side of the face that is currently displayed.' + LineEnding + 
'     Current normal is for front face, so negate it if backfacing.' + LineEnding + 
'     Since this is in tangent space, "negate" means only negate it''s z' + LineEnding + 
'     component.' + LineEnding + 
'' + LineEnding + 
'     Alt version of this, not using "if" just in case for future:' + LineEnding + 
'       normal.z -= normal.z * 2.0 * (1.0 - float(gl_FrontFacing));' + LineEnding + 
'  */' + LineEnding + 
'  if (!gl_FrontFacing)' + LineEnding + 
'    normal.z = -normal.z;' + LineEnding + 
'' + LineEnding + 
'  /* gl_FragColor += diffuse lighting */' + LineEnding + 
'  gl_FragColor += light_diffuse_color * gl_FrontMaterial.diffuse *' + LineEnding + 
'      max(dot(normal, light_dir), 0.0);' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor *= texture2D(tex_original, gl_TexCoord[0].st);' + LineEnding + 
'' + LineEnding + 
'/* This is more close to what bmDot3Normalized method did' + LineEnding + 
'   (but it''s less correct and generally worse. I present it here' + LineEnding + 
'   only to visually compare bmDot3Normalized and bmGLSL,' + LineEnding + 
'   to see that they are able to calculate the same.)' + LineEnding + 
'*/' + LineEnding + 
'/*' + LineEnding + 
'  gl_FragColor =' + LineEnding + 
'    //gl_FrontLightModelProduct.sceneColor +' + LineEnding + 
'    gl_FrontLightProduct[0].ambient +' + LineEnding + 
'    gl_FrontLightProduct[0].diffuse;' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor *= texture2D(tex_original, gl_TexCoord[0].st);' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor *= max(dot(normal, light_dir), 0.0);' + LineEnding + 
'*/' + LineEnding + 
'}' + LineEnding + 
''
