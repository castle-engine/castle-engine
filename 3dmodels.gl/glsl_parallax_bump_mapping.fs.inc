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
'/* This will be defined (or not) right before compilation of this file,' + LineEnding + 
'   see VRMLOpenGLRenderer.pas.' + LineEnding + 
'#define STEEP' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'#ifdef STEEP' + LineEnding + 
'  /* You can define STEEP_SHADOW only if STEEP is also defined.' + LineEnding + 
'     (Although it is possible to implement self-shadowing with still shifting' + LineEnding + 
'     tex coord with classic (no steep) parallax mapping, it''s not implemented.)' + LineEnding + 
'  */' + LineEnding + 
'  #define STEEP_SHADOW' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'/* Number of steps for steep mapping.' + LineEnding + 
'   I saw implementations that mix between 50 and 10 num_steps.' + LineEnding + 
'   Mixing between 10 and 30 seems to give quite good results, and is really' + LineEnding + 
'   fast on ATI on MacBookPro.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'const float steep_steps_min = 10.0;' + LineEnding + 
'const float steep_steps_max = 30.0;' + LineEnding + 
'' + LineEnding + 
'const float steep_shadow_steps_min = 10.0;' + LineEnding + 
'const float steep_shadow_steps_max = 30.0;' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  vec3 p_to_eye = normalize(point_to_eye_in_tangent_space);' + LineEnding + 
'  vec2 texture_coord = gl_TexCoord[0].st;' + LineEnding + 
'' + LineEnding + 
'#ifndef STEEP' + LineEnding + 
'  /* I take "r" (red) component of tex_height_map.' + LineEnding + 
'     When loading texture for tex_height_map, I made sure it''s grayscale' + LineEnding + 
'     so I''m sure that all rgb components are the same. */' + LineEnding + 
'  float height = texture2D(tex_height_map, gl_TexCoord[0].st).r * scale - bias;' + LineEnding + 
'  texture_coord += height * p_to_eye.xy /* / p_to_eye.z*/;' + LineEnding + 
'#else' + LineEnding + 
'  /* At smaller view angles, much more iterations needed, otherwise ugly' + LineEnding + 
'     aliasing arifacts quickly appear. */' + LineEnding + 
'  float num_steps = mix(steep_steps_max, steep_steps_min, p_to_eye.z);' + LineEnding + 
'' + LineEnding + 
'  float step = 1.0 / num_steps;' + LineEnding + 
'' + LineEnding + 
'  /* Should we remove "p_to_eye.z" below, i.e. should we apply' + LineEnding + 
'     "offset limiting" ? In works about steep parallax mapping,' + LineEnding + 
'     p_to_eye.z is present, and in sample steep parallax mapping' + LineEnding + 
'     shader they suggest that it doesn''t really matter.' + LineEnding + 
'     My tests confirm this, so I leave p_to_eye.z component. */' + LineEnding + 
'' + LineEnding + 
'  vec2 delta = -p_to_eye.xy * scale / (p_to_eye.z * num_steps);' + LineEnding + 
'' + LineEnding + 
'  float height = 1.0;' + LineEnding + 
'' + LineEnding + 
'  float map_height = texture2D(tex_height_map, texture_coord).r;' + LineEnding + 
'' + LineEnding + 
'  /* It''s known problem that NVidia GeForce FX 5200 fails here with' + LineEnding + 
'' + LineEnding + 
'       error C5011: profile does not support "while" statements' + LineEnding + 
'       and "while" could not be unrolled.' + LineEnding + 
'' + LineEnding + 
'     I could workaround this problem (by using' + LineEnding + 
'       for (int i = 0; i < steep_steps_max; i++)' + LineEnding + 
'     loop and' + LineEnding + 
'       if (! (map_height < height)) break;' + LineEnding + 
'     , this is possible to unroll). But it turns out that this still' + LineEnding + 
'     (even with steep_steps_max = 1) works much too slow on this hardware...' + LineEnding + 
'     so I simply fallback to non-steep version of parallax mapping' + LineEnding + 
'     if this doesn''t compile. */' + LineEnding + 
'' + LineEnding + 
'  while (map_height < height)' + LineEnding + 
'  {' + LineEnding + 
'    height -= step;' + LineEnding + 
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
'  /* TODO: two-sided lighting.' + LineEnding + 
'     See glsl_bump_mapping.fs for comments why it''s not done now.' + LineEnding + 
'' + LineEnding + 
'  if (!gl_FrontFacing)' + LineEnding + 
'    normal.z = -normal.z;' + LineEnding + 
'  */' + LineEnding + 
'' + LineEnding + 
'  /* gl_FragColor += diffuse lighting */' + LineEnding + 
'  float diffuse_factor = dot(normal, light_dir);' + LineEnding + 
'#ifndef STEEP_SHADOW' + LineEnding + 
'  gl_FragColor += light_diffuse_color * gl_FrontMaterial.diffuse *' + LineEnding + 
'      max(diffuse_factor, 0.0);' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor *= texture2D(tex_original, texture_coord);' + LineEnding + 
'#else' + LineEnding + 
'  if (diffuse_factor > 0.0)' + LineEnding + 
'  {' + LineEnding + 
'    /* We basically do the same thing as when we calculate texture_coord' + LineEnding + 
'       with steep parallax mapping.' + LineEnding + 
'       Only now we increment height, and we use light_dir instead of' + LineEnding + 
'       p_to_eye. */' + LineEnding + 
'    float num_steps = mix(steep_shadow_steps_max, steep_shadow_steps_min, light_dir.z);' + LineEnding + 
'' + LineEnding + 
'    float step = 1.0 / num_steps;' + LineEnding + 
'' + LineEnding + 
'    vec2 delta = light_dir.xy * scale / (light_dir.z * num_steps);' + LineEnding + 
'' + LineEnding + 
'    /* Do the 1st step always, otherwise initial height = shadow_map_height' + LineEnding + 
'       and we would be considered in our own shadow. */' + LineEnding + 
'    float height = map_height + step;' + LineEnding + 
'    vec2 shadow_texture_coord = texture_coord + delta;' + LineEnding + 
'    float shadow_map_height = texture2D(tex_height_map, shadow_texture_coord).r;' + LineEnding + 
'' + LineEnding + 
'    while (shadow_map_height < height && height < 1.0)' + LineEnding + 
'    {' + LineEnding + 
'      height += step;' + LineEnding + 
'      shadow_texture_coord += delta;' + LineEnding + 
'      shadow_map_height = texture2D(tex_height_map, shadow_texture_coord).r;' + LineEnding + 
'    }' + LineEnding + 
'' + LineEnding + 
'    if (shadow_map_height < height)' + LineEnding + 
'    {' + LineEnding + 
'      gl_FragColor += light_diffuse_color * gl_FrontMaterial.diffuse *' + LineEnding + 
'        diffuse_factor;' + LineEnding + 
'    }' + LineEnding + 
'  }' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor *= texture2D(tex_original, texture_coord);' + LineEnding + 
'}' + LineEnding + 
''
