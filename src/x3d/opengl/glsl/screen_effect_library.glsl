/* The library of common functions for GLSL screen effects.
   The ObjectPascal code that includes this will take care to define (or not)
   symbols MULTI_SAMPLING_x, DEPTH at the beginning of this file.
   Screen effect code using these functions is safe to work both with and without
   multi-sampling.
*/

#ifdef MULTI_SAMPLING
  #extension GL_ARB_texture_multisample : enable
  uniform sampler2DMS screen;
  #ifdef DEPTH
    uniform sampler2DMS screen_depth;
  #endif
#else
  uniform sampler2D screen;
  #ifdef DEPTH
    uniform sampler2D screen_depth;
  #endif
#endif

#ifdef GL_ES
precision mediump float;
#endif

varying vec2 tex_coord_frag;

uniform int screen_height;
uniform int screen_width;

ivec2 screen_position()
{
  return ivec2(
    int(tex_coord_frag.s * float(screen_width)),
    int(tex_coord_frag.t * float(screen_height)));
}

int screen_x()
{
  return int(tex_coord_frag.s * float(screen_width));
}

int screen_y()
{
  return int(tex_coord_frag.t * float(screen_height));
}

vec4 screen_get_color(ivec2 position)
{
/* TODO: it would be nice to autogenerate this code */
#ifdef MULTI_SAMPLING_16
  return ( texelFetch(screen, position, 0) +
           texelFetch(screen, position, 1) +
           texelFetch(screen, position, 2) +
           texelFetch(screen, position, 3) +
           texelFetch(screen, position, 4) +
           texelFetch(screen, position, 5) +
           texelFetch(screen, position, 6) +
           texelFetch(screen, position, 7) +
           texelFetch(screen, position, 8) +
           texelFetch(screen, position, 9) +
           texelFetch(screen, position, 10) +
           texelFetch(screen, position, 11) +
           texelFetch(screen, position, 12) +
           texelFetch(screen, position, 13) +
           texelFetch(screen, position, 14) +
           texelFetch(screen, position, 15) ) / 16.0;
#else
#ifdef MULTI_SAMPLING_8
  return ( texelFetch(screen, position, 0) +
           texelFetch(screen, position, 1) +
           texelFetch(screen, position, 2) +
           texelFetch(screen, position, 3) +
           texelFetch(screen, position, 4) +
           texelFetch(screen, position, 5) +
           texelFetch(screen, position, 6) +
           texelFetch(screen, position, 7) ) / 8.0;
#else
#ifdef MULTI_SAMPLING_4
  return ( texelFetch(screen, position, 0) +
           texelFetch(screen, position, 1) +
           texelFetch(screen, position, 2) +
           texelFetch(screen, position, 3) ) / 4.0;
#else
#ifdef MULTI_SAMPLING_2
  return ( texelFetch(screen, position, 0) +
           texelFetch(screen, position, 1) ) / 2.0;
#else
  return texture2D(screen,
    /* Texture coordinates provided in tex_coord_frag are already hitting
       exactly the middle of the pixel.
       But functions screen_position, screen_x, screen_y cut off the half pixel
       size. So we restore it now by "+ vec2(0.5)". */
    (vec2(position) + vec2(0.5)) / vec2(screen_width, screen_height));
#endif
#endif
#endif
#endif
}

#ifdef DEPTH
float screen_get_depth(ivec2 position)
{
/* TODO: it would be nice to autogenerate this code */
#ifdef MULTI_SAMPLING_16
  return ( texelFetch(screen_depth, position, 0).r +
           texelFetch(screen_depth, position, 1).r +
           texelFetch(screen_depth, position, 2).r +
           texelFetch(screen_depth, position, 3).r +
           texelFetch(screen_depth, position, 4).r +
           texelFetch(screen_depth, position, 5).r +
           texelFetch(screen_depth, position, 6).r +
           texelFetch(screen_depth, position, 7).r +
           texelFetch(screen_depth, position, 8).r +
           texelFetch(screen_depth, position, 9).r +
           texelFetch(screen_depth, position, 10).r +
           texelFetch(screen_depth, position, 11).r +
           texelFetch(screen_depth, position, 12).r +
           texelFetch(screen_depth, position, 13).r +
           texelFetch(screen_depth, position, 14).r +
           texelFetch(screen_depth, position, 15).r ) / 16.0;
#else
#ifdef MULTI_SAMPLING_8
  return ( texelFetch(screen_depth, position, 0).r +
           texelFetch(screen_depth, position, 1).r +
           texelFetch(screen_depth, position, 2).r +
           texelFetch(screen_depth, position, 3).r +
           texelFetch(screen_depth, position, 4).r +
           texelFetch(screen_depth, position, 5).r +
           texelFetch(screen_depth, position, 6).r +
           texelFetch(screen_depth, position, 7).r ) / 8.0;
#else
#ifdef MULTI_SAMPLING_4
  return ( texelFetch(screen_depth, position, 0).r +
           texelFetch(screen_depth, position, 1).r +
           texelFetch(screen_depth, position, 2).r +
           texelFetch(screen_depth, position, 3).r ) / 4.0;
#else
#ifdef MULTI_SAMPLING_2
  return ( texelFetch(screen_depth, position, 0).r +
           texelFetch(screen_depth, position, 1).r ) / 2.0;
#else
  return texture2D(screen_depth,
    (vec2(position) + vec2(0.5)) / vec2(screen_width, screen_height)).r;
#endif
#endif
#endif
#endif
}

float screen_get_depth_fast(ivec2 position)
{
#ifdef MULTI_SAMPLING
  return texelFetch(screen_depth, position, 0).r;
#else
  return texture2D(screen_depth,
    (vec2(position) + vec2(0.5)) / vec2(screen_width, screen_height)).r;
#endif
}
#endif
