/*
  Copyright 2009-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

/* Needed for % operator.
   Not done anymore, CGE will add defined to use latest GLSL version. */
//#extension GL_EXT_gpu_shader4 : enable
//#version 130

/*$defines*/

/* Fragment shader doing dynamic_ambient_occlusion.
   See README for links and description what it does. */

uniform sampler2D tex_elements_position_area;
uniform sampler2D tex_elements_normal;

#ifdef PASS_2
uniform sampler2D tex_elements_intensity;
#endif

const int tex_elements_size = $tex_elements_size;
const int elements_count = $elements_count;

uniform float area_scale;
uniform vec3 position_scale;
uniform vec3 position_shift;

uniform float shadow_scale;

void main(void)
{
  /* calculate current element properties.
     We know which element is "current" from gl_FragCoord. */
  vec2 current_st = gl_FragCoord.xy;
  int current_index = int(current_st.x) + int(current_st.y) * tex_elements_size;

  if (current_index < elements_count)
  {
    current_st /= float(tex_elements_size); /* make ST coords of the texture in 0..1 */
    vec4 current_pos_area = texture2D(tex_elements_position_area, current_st);
    vec3 current_pos = (current_pos_area.xyz + position_shift) * position_scale;

    vec3 current_normal = texture2D(tex_elements_normal, current_st).xyz;
    current_normal = (current_normal - 0.5) * 2.0;

    float color = 1.0;

    for (int i = 0; i < elements_count; i++)
    {
      if (i != current_index)
      {
        vec2 element_st = vec2(
          float(i % tex_elements_size),
          float(i / tex_elements_size));
        element_st /= float(tex_elements_size); /* make ST coords of the texture in 0..1 */

        vec4 element_pos_area = texture2D(tex_elements_position_area, element_st);
        vec3 element_pos = (element_pos_area.xyz + position_shift) * position_scale;
        float element_area = element_pos_area.w * area_scale;

        vec3 element_normal = texture2D(tex_elements_normal, element_st).xyz;
        element_normal = (element_normal - 0.5) * 2.0;

        vec3 direction_from_current = element_pos - current_pos;
        float sqr_distance = dot(direction_from_current, direction_from_current);

        /* normalize direction_from_current for following cos() calculations */
        direction_from_current = normalize(direction_from_current);
        float cos_emitter_angle = dot(direction_from_current, element_normal);
        float cos_current_angle = dot(direction_from_current, current_normal);

        /* We gather the light only from the dir of our normal,
           so only when cos_current_angle >= 0.
           This also means that we normalize by 2*pi (not 4*pi),
           as we have effectively already cut off to hemisphere.
           (Actually, this normalization is now in Pascal code, see ShadowScale).

           Also, we are shadowed only by items that gather light
           (that is, only if they are lighted then they also block the light).

           That's the reasoning behind these checks. */
        if (cos_emitter_angle >= 0.0 &&
            cos_current_angle >= 0.0)
        {
          color -= (element_area * cos_emitter_angle * cos_current_angle *
            shadow_scale / sqr_distance)
            #ifdef PASS_2
            /* Multiply by intensity of this element from 1st pass.
               If element is in shadow, then this is 0, and then we will
               not take it's contribution --- as this means that something else
               shadows it, so it will also shadow us. */
            * texture2D(tex_elements_intensity, element_st).x
            #endif
            ;
        }
      }
    }

    #ifdef PASS_2
    /* At the end of 2nd pass, result is the average of 1st and 2nd pass
       results. */
    color = (color + texture2D(tex_elements_intensity, current_st).x) / 2.0;
    #endif

    /* Return color only in the red component. Note that using "color"
       for all RGB components (like
         gl_FragColor = vec4(color, color, color, 1.0);
       ) causes wrong behavior if we catch the resulting fragments
       to GL_LUMINANCE texture. It looks like the resulting luminance
       is the sum of (red + green + blue), and so it's
       far larger than expected. OpenGL spec says that luminance comes
       from only red, so possibly this is a bug of fglrx (Linux ATI
       drivers, ATI Mobility Radeon X1600, Mac Book Pro, chantal). */

    gl_FragColor = vec4(color, 0.0, 0.0, 1.0);
  }
  else
  {
    /* Only for testing, to mark colors not corresponding to any element. */
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
  }
}
