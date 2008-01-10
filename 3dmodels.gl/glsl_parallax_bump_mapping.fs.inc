{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "glsl_parallax_bump_mapping.fs" }
'/* GLSL fragment shader to do bump mapping.' + LineEnding + 
'   Version with parallax mapping.' + LineEnding + 
'' + LineEnding + 
'   This is converted to glsl_parallax_bump_mapping.fs.inc, and is them compiled' + LineEnding + 
'   in program''s binary.' + LineEnding + 
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'uniform sampler2D tex_normal_map;' + LineEnding + 
'uniform sampler2D tex_original;' + LineEnding + 
'uniform sampler2D tex_height_map;' + LineEnding + 
'uniform vec4 light_ambient_color;' + LineEnding + 
'uniform vec4 light_diffuse_color;' + LineEnding + 
'' + LineEnding + 
'uniform float scale;' + LineEnding + 
'uniform float bias;' + LineEnding + 
'' + LineEnding + 
'varying vec3 light_dir_tangent;' + LineEnding + 
'' + LineEnding + 
'varying vec3 point_to_eye_in_tangent_space;' + LineEnding + 
'' + LineEnding + 
'#define STEEP_PARALLAX' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  vec3 p_to_eye = normalize(point_to_eye_in_tangent_space);' + LineEnding + 
'  vec2 texture_coord = gl_TexCoord[0].st;' + LineEnding + 
'' + LineEnding + 
'#ifndef STEEP_PARALLAX' + LineEnding + 
'  /* I take "r" (red) component of tex_height_map.' + LineEnding + 
'     When loading texture for tex_height_map, I made sure it''s grayscale' + LineEnding + 
'     so I''m sure that all rgb components are the same. */' + LineEnding + 
'  float height = texture2D(tex_height_map, gl_TexCoord[0].st).r * scale - bias;' + LineEnding + 
'  texture_coord += height * p_to_eye.xy /* / p_to_eye.z*/;' + LineEnding + 
'#else' + LineEnding + 
'  const float num_steps = 5.0;' + LineEnding + 
'  /* step could be simple const too, but we want to avoid letting' + LineEnding + 
'     Radeon fglrx use float consts (they are incorrectly rounded to ints). */' + LineEnding + 
'  float step = 1.0;' + LineEnding + 
'  step /= num_steps;' + LineEnding + 
'' + LineEnding + 
'  /* TODO: is offset limiting, i.e. removal of ".z" useful here ?' + LineEnding + 
'     Paper says "doesn''t matter". Test. */' + LineEnding + 
'  vec2 delta = p_to_eye.xy * scale / (p_to_eye.z * num_steps);' + LineEnding + 
'' + LineEnding + 
'  float height_limit = 1.0;' + LineEnding + 
'' + LineEnding + 
'  float map_height = texture2D(tex_height_map, texture_coord).r;' + LineEnding + 
'  while (map_height < height_limit)' + LineEnding + 
'  {' + LineEnding + 
'    height_limit -= step;' + LineEnding + 
'    texture_coord += delta;' + LineEnding + 
'    map_height = texture2D(tex_height_map, texture_coord).r;' + LineEnding + 
'  }' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
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
'    texture2D(tex_normal_map, texture_coord)) * 2.0 - vec3(1, 1, 1);' + LineEnding + 
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
'/*  if (!gl_FrontFacing)' + LineEnding + 
'    normal.z = -normal.z;' + LineEnding + 
'*/' + LineEnding + 
'  /* gl_FragColor += diffuse lighting */' + LineEnding + 
'  gl_FragColor += light_diffuse_color * gl_FrontMaterial.diffuse *' + LineEnding + 
'      max(dot(normal, light_dir), 0.0);' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor *= texture2D(tex_original, texture_coord);' + LineEnding + 
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
'  gl_FragColor *= texture2D(tex_original, texture_coord);' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor *= max(dot(normal, light_dir), 0.0);' + LineEnding + 
'*/' + LineEnding + 
'}' + LineEnding + 
''
