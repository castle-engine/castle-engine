{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "screen_effect_library.glsl" }
'/* The library of common functions for GLSL screen effects.' + LineEnding + 
'   The ObjectPascal code that includes this will take care to define (or not)' + LineEnding + 
'   symbols MULTI_SAMPLING_x, DEPTH at the beginning of this file.' + LineEnding + 
'   Screen effect code using these functions is safe to work both with and without' + LineEnding + 
'   multi-sampling.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'#ifdef MULTI_SAMPLING' + LineEnding + 
'  #extension GL_ARB_texture_multisample : enable' + LineEnding + 
'  uniform sampler2DMS screen;' + LineEnding + 
'  #ifdef DEPTH' + LineEnding + 
'    uniform sampler2DMS screen_depth;' + LineEnding + 
'  #endif' + LineEnding + 
'#else' + LineEnding + 
'  #extension GL_ARB_texture_rectangle : enable' + LineEnding + 
'  uniform sampler2DRect screen;' + LineEnding + 
'  #ifdef DEPTH' + LineEnding + 
'    uniform sampler2DRect screen_depth;' + LineEnding + 
'  #endif' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'ivec2 screen_position()' + LineEnding + 
'{' + LineEnding + 
'  return ivec2(int(gl_TexCoord[0].s), int(gl_TexCoord[0].t));' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'int screen_x()' + LineEnding + 
'{' + LineEnding + 
'  return int(gl_TexCoord[0].s);' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'int screen_y()' + LineEnding + 
'{' + LineEnding + 
'  return int(gl_TexCoord[0].t);' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'vec4 screen_get_color(ivec2 position)' + LineEnding + 
'{' + LineEnding + 
'#ifdef MULTI_SAMPLING_4' + LineEnding + 
'  return ( texelFetch(screen, position, 0) +' + LineEnding + 
'           texelFetch(screen, position, 1) +' + LineEnding + 
'           texelFetch(screen, position, 2) +' + LineEnding + 
'           texelFetch(screen, position, 3) ) / 4.0;' + LineEnding + 
'#else' + LineEnding + 
'#ifdef MULTI_SAMPLING_2' + LineEnding + 
'  return ( texelFetch(screen, position, 0) +' + LineEnding + 
'           texelFetch(screen, position, 1) ) / 2.0;' + LineEnding + 
'#else' + LineEnding + 
'  return texture2DRect(screen, vec2(position));' + LineEnding + 
'#endif' + LineEnding + 
'#endif' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'#ifdef DEPTH' + LineEnding + 
'float screen_get_depth(ivec2 position)' + LineEnding + 
'{' + LineEnding + 
'#ifdef MULTI_SAMPLING_4' + LineEnding + 
'  return ( texelFetch(screen_depth, position, 0).r +' + LineEnding + 
'           texelFetch(screen_depth, position, 1).r +' + LineEnding + 
'           texelFetch(screen_depth, position, 2).r +' + LineEnding + 
'           texelFetch(screen_depth, position, 3).r ) / 4.0;' + LineEnding + 
'#else' + LineEnding + 
'#ifdef MULTI_SAMPLING_2' + LineEnding + 
'  return ( texelFetch(screen_depth, position, 0).r +' + LineEnding + 
'           texelFetch(screen_depth, position, 1).r ) / 2.0;' + LineEnding + 
'#else' + LineEnding + 
'  return texture2DRect(screen_depth, vec2(position)).r;' + LineEnding + 
'#endif' + LineEnding + 
'#endif' + LineEnding + 
'}' + LineEnding + 
'#endif' + LineEnding + 
''
