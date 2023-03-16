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

varying vec2 screenf_01_position;

uniform int screen_height;
uniform int screen_width;

/* Library of integer-based functions ---------------------------------------- */

ivec2 screen_position()
{
  return ivec2(
    int(screenf_01_position.s * float(screen_width)),
    int(screenf_01_position.t * float(screen_height)));
}

int screen_x()
{
  return int(screenf_01_position.s * float(screen_width));
}

int screen_y()
{
  return int(screenf_01_position.t * float(screen_height));
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
    /* Texture coordinates provided in screenf_01_position are already hitting
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

/* Library of float-based functions ------------------------------------------ */

vec2 screenf_position()
{
  return screenf_01_position *
    vec2(float(screen_width), float(screen_height));
}

float screenf_x()
{
  return screenf_01_position.s * float(screen_width);
}

float screenf_y()
{
  return screenf_01_position.t * float(screen_height);
}

vec4 screenf_get_color(vec2 position)
{
/* TODO: it would be nice to autogenerate this code */
#ifdef MULTI_SAMPLING_16
  return ( texelFetch(screen, ivec2(position), 0) +
           texelFetch(screen, ivec2(position), 1) +
           texelFetch(screen, ivec2(position), 2) +
           texelFetch(screen, ivec2(position), 3) +
           texelFetch(screen, ivec2(position), 4) +
           texelFetch(screen, ivec2(position), 5) +
           texelFetch(screen, ivec2(position), 6) +
           texelFetch(screen, ivec2(position), 7) +
           texelFetch(screen, ivec2(position), 8) +
           texelFetch(screen, ivec2(position), 9) +
           texelFetch(screen, ivec2(position), 10) +
           texelFetch(screen, ivec2(position), 11) +
           texelFetch(screen, ivec2(position), 12) +
           texelFetch(screen, ivec2(position), 13) +
           texelFetch(screen, ivec2(position), 14) +
           texelFetch(screen, ivec2(position), 15) ) / 16.0;
#else
#ifdef MULTI_SAMPLING_8
  return ( texelFetch(screen, ivec2(position), 0) +
           texelFetch(screen, ivec2(position), 1) +
           texelFetch(screen, ivec2(position), 2) +
           texelFetch(screen, ivec2(position), 3) +
           texelFetch(screen, ivec2(position), 4) +
           texelFetch(screen, ivec2(position), 5) +
           texelFetch(screen, ivec2(position), 6) +
           texelFetch(screen, ivec2(position), 7) ) / 8.0;
#else
#ifdef MULTI_SAMPLING_4
  return ( texelFetch(screen, ivec2(position), 0) +
           texelFetch(screen, ivec2(position), 1) +
           texelFetch(screen, ivec2(position), 2) +
           texelFetch(screen, ivec2(position), 3) ) / 4.0;
#else
#ifdef MULTI_SAMPLING_2
  return ( texelFetch(screen, ivec2(position), 0) +
           texelFetch(screen, ivec2(position), 1) ) / 2.0;
#else
  return texture2D(screen, position / vec2(screen_width, screen_height));
#endif
#endif
#endif
#endif
}

vec4 screenf_get_original_color()
{
#ifdef MULTI_SAMPLING
  // We cannot do something much more optimal in this case, so just call full screenf_get_color
  return screenf_get_color(screenf_position());
#else
  return texture2D(screen, screenf_01_position);
#endif
}

vec4 screenf_01_get_color(vec2 position_01)
{
#ifdef MULTI_SAMPLING
  // We cannot do something much more optimal in this case, so just call full screenf_get_color
  return screenf_get_color(position_01 * vec2(float(screen_width), float(screen_height)));
#else
  return texture2D(screen, position_01);
#endif
}

#ifdef DEPTH
float screenf_get_depth(vec2 position)
{
/* TODO: it would be nice to autogenerate this code */
#ifdef MULTI_SAMPLING_16
  return ( texelFetch(screen_depth, ivec2(position), 0).r +
           texelFetch(screen_depth, ivec2(position), 1).r +
           texelFetch(screen_depth, ivec2(position), 2).r +
           texelFetch(screen_depth, ivec2(position), 3).r +
           texelFetch(screen_depth, ivec2(position), 4).r +
           texelFetch(screen_depth, ivec2(position), 5).r +
           texelFetch(screen_depth, ivec2(position), 6).r +
           texelFetch(screen_depth, ivec2(position), 7).r +
           texelFetch(screen_depth, ivec2(position), 8).r +
           texelFetch(screen_depth, ivec2(position), 9).r +
           texelFetch(screen_depth, ivec2(position), 10).r +
           texelFetch(screen_depth, ivec2(position), 11).r +
           texelFetch(screen_depth, ivec2(position), 12).r +
           texelFetch(screen_depth, ivec2(position), 13).r +
           texelFetch(screen_depth, ivec2(position), 14).r +
           texelFetch(screen_depth, ivec2(position), 15).r ) / 16.0;
#else
#ifdef MULTI_SAMPLING_8
  return ( texelFetch(screen_depth, ivec2(position), 0).r +
           texelFetch(screen_depth, ivec2(position), 1).r +
           texelFetch(screen_depth, ivec2(position), 2).r +
           texelFetch(screen_depth, ivec2(position), 3).r +
           texelFetch(screen_depth, ivec2(position), 4).r +
           texelFetch(screen_depth, ivec2(position), 5).r +
           texelFetch(screen_depth, ivec2(position), 6).r +
           texelFetch(screen_depth, ivec2(position), 7).r ) / 8.0;
#else
#ifdef MULTI_SAMPLING_4
  return ( texelFetch(screen_depth, ivec2(position), 0).r +
           texelFetch(screen_depth, ivec2(position), 1).r +
           texelFetch(screen_depth, ivec2(position), 2).r +
           texelFetch(screen_depth, ivec2(position), 3).r ) / 4.0;
#else
#ifdef MULTI_SAMPLING_2
  return ( texelFetch(screen_depth, ivec2(position), 0).r +
           texelFetch(screen_depth, ivec2(position), 1).r ) / 2.0;
#else
  return texture2D(screen_depth, position / vec2(screen_width, screen_height)).r;
#endif
#endif
#endif
#endif
}

float screenf_01_get_depth(vec2 position_01)
{
#ifdef MULTI_SAMPLING
  // We cannot do something much more optimal in this case, so just call full screenf_get_depth
  return screenf_get_depth(position_01 * vec2(float(screen_width), float(screen_height)));
#else
  return texture2D(screen_depth, position_01).r;
#endif
}

float screenf_get_original_depth()
{
#ifdef MULTI_SAMPLING
  // We cannot do something much more optimal in this case, so just call full screenf_get_depth
  return screenf_get_depth(screenf_position());
#else
  return texture2D(screen_depth, screenf_01_position).r;
#endif
}

float screenf_get_depth_fast(vec2 position)
{
#ifdef MULTI_SAMPLING
  return texelFetch(screen_depth, ivec2(position), 0).r;
#else
  return texture2D(screen_depth, position / vec2(screen_width, screen_height)).r;
#endif
}
#endif
