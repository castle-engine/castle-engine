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
'  uniform sampler2D screen;' + LineEnding +
'  #ifdef DEPTH' + LineEnding +
'    uniform sampler2D screen_depth;' + LineEnding +
'  #endif' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef GL_ES' + LineEnding +
'precision mediump float;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'varying vec2 tex_coord_frag;' + LineEnding +
'' + LineEnding +
'uniform int screen_height;' + LineEnding +
'uniform int screen_width;' + LineEnding +
'' + LineEnding +
'ivec2 screen_position()' + LineEnding +
'{' + LineEnding +
'  return ivec2(' + LineEnding +
'    int(tex_coord_frag.s * float(screen_width)),' + LineEnding +
'    int(tex_coord_frag.t * float(screen_height)));' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'int screen_x()' + LineEnding +
'{' + LineEnding +
'  return int(tex_coord_frag.s * float(screen_width));' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'int screen_y()' + LineEnding +
'{' + LineEnding +
'  return int(tex_coord_frag.t * float(screen_height));' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'vec4 screen_get_color(ivec2 position)' + LineEnding +
'{' + LineEnding +
'/* TODO: it would be nice to autogenerate this code */' + LineEnding +
'#ifdef MULTI_SAMPLING_16' + LineEnding +
'  return ( texelFetch(screen, position, 0) +' + LineEnding +
'           texelFetch(screen, position, 1) +' + LineEnding +
'           texelFetch(screen, position, 2) +' + LineEnding +
'           texelFetch(screen, position, 3) +' + LineEnding +
'           texelFetch(screen, position, 4) +' + LineEnding +
'           texelFetch(screen, position, 5) +' + LineEnding +
'           texelFetch(screen, position, 6) +' + LineEnding +
'           texelFetch(screen, position, 7) +' + LineEnding +
'           texelFetch(screen, position, 8) +' + LineEnding +
'           texelFetch(screen, position, 9) +' + LineEnding +
'           texelFetch(screen, position, 10) +' + LineEnding +
'           texelFetch(screen, position, 11) +' + LineEnding +
'           texelFetch(screen, position, 12) +' + LineEnding +
'           texelFetch(screen, position, 13) +' + LineEnding +
'           texelFetch(screen, position, 14) +' + LineEnding +
'           texelFetch(screen, position, 15) ) / 16.0;' + LineEnding +
'#else' + LineEnding +
'#ifdef MULTI_SAMPLING_8' + LineEnding +
'  return ( texelFetch(screen, position, 0) +' + LineEnding +
'           texelFetch(screen, position, 1) +' + LineEnding +
'           texelFetch(screen, position, 2) +' + LineEnding +
'           texelFetch(screen, position, 3) +' + LineEnding +
'           texelFetch(screen, position, 4) +' + LineEnding +
'           texelFetch(screen, position, 5) +' + LineEnding +
'           texelFetch(screen, position, 6) +' + LineEnding +
'           texelFetch(screen, position, 7) ) / 8.0;' + LineEnding +
'#else' + LineEnding +
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
'  return texture2D(screen,' + LineEnding +
'    /* Texture coordinates provided in tex_coord_frag are already hitting' + LineEnding +
'       exactly the middle of the pixel.' + LineEnding +
'       But functions screen_position, screen_x, screen_y cut off the half pixel' + LineEnding +
'       size. So we restore it now by "+ vec2(0.5)". */' + LineEnding +
'    (vec2(position) + vec2(0.5)) / vec2(screen_width, screen_height));' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'#ifdef DEPTH' + LineEnding +
'float screen_get_depth(ivec2 position)' + LineEnding +
'{' + LineEnding +
'/* TODO: it would be nice to autogenerate this code */' + LineEnding +
'#ifdef MULTI_SAMPLING_16' + LineEnding +
'  return ( texelFetch(screen_depth, position, 0).r +' + LineEnding +
'           texelFetch(screen_depth, position, 1).r +' + LineEnding +
'           texelFetch(screen_depth, position, 2).r +' + LineEnding +
'           texelFetch(screen_depth, position, 3).r +' + LineEnding +
'           texelFetch(screen_depth, position, 4).r +' + LineEnding +
'           texelFetch(screen_depth, position, 5).r +' + LineEnding +
'           texelFetch(screen_depth, position, 6).r +' + LineEnding +
'           texelFetch(screen_depth, position, 7).r +' + LineEnding +
'           texelFetch(screen_depth, position, 8).r +' + LineEnding +
'           texelFetch(screen_depth, position, 9).r +' + LineEnding +
'           texelFetch(screen_depth, position, 10).r +' + LineEnding +
'           texelFetch(screen_depth, position, 11).r +' + LineEnding +
'           texelFetch(screen_depth, position, 12).r +' + LineEnding +
'           texelFetch(screen_depth, position, 13).r +' + LineEnding +
'           texelFetch(screen_depth, position, 14).r +' + LineEnding +
'           texelFetch(screen_depth, position, 15).r ) / 16.0;' + LineEnding +
'#else' + LineEnding +
'#ifdef MULTI_SAMPLING_8' + LineEnding +
'  return ( texelFetch(screen_depth, position, 0).r +' + LineEnding +
'           texelFetch(screen_depth, position, 1).r +' + LineEnding +
'           texelFetch(screen_depth, position, 2).r +' + LineEnding +
'           texelFetch(screen_depth, position, 3).r +' + LineEnding +
'           texelFetch(screen_depth, position, 4).r +' + LineEnding +
'           texelFetch(screen_depth, position, 5).r +' + LineEnding +
'           texelFetch(screen_depth, position, 6).r +' + LineEnding +
'           texelFetch(screen_depth, position, 7).r ) / 8.0;' + LineEnding +
'#else' + LineEnding +
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
'  return texture2D(screen_depth,' + LineEnding +
'    (vec2(position) + vec2(0.5)) / vec2(screen_width, screen_height)).r;' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'float screen_get_depth_fast(ivec2 position)' + LineEnding +
'{' + LineEnding +
'#ifdef MULTI_SAMPLING' + LineEnding +
'  return texelFetch(screen_depth, position, 0).r;' + LineEnding +
'#else' + LineEnding +
'  return texture2D(screen_depth,' + LineEnding +
'    (vec2(position) + vec2(0.5)) / vec2(screen_width, screen_height)).r;' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
'#endif' + LineEnding +
''
