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
  #extension GL_ARB_texture_rectangle : enable
  uniform sampler2DRect screen;
  #ifdef DEPTH
    uniform sampler2DRect screen_depth;
  #endif
#endif

ivec2 screen_position()
{
  return ivec2(int(gl_TexCoord[0].s), int(gl_TexCoord[0].t));
}

int screen_x()
{
  return int(gl_TexCoord[0].s);
}

int screen_y()
{
  return int(gl_TexCoord[0].t);
}

vec4 screen_get_color(ivec2 position)
{
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
  return texture2DRect(screen, vec2(position)+vec2(0.5));
#endif
#endif
}

#ifdef DEPTH
float screen_get_depth(ivec2 position)
{
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
  return texture2DRect(screen_depth, vec2(position)+vec2(0.5)).r;
#endif
#endif
}
#endif
