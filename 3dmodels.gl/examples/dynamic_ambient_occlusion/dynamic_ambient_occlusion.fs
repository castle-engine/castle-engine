uniform sampler2D tex_elements_position_area;
uniform sampler2D tex_elements_normal;

#ifdef PASS_2
uniform sampler2D tex_elements_intensity;
#endif

#ifdef FGLRX
/* Fglrx is dumb and fails when elements_count is a constant...
   For other GPUs, elements_count as a constant is a good idea. */
uniform int tex_elements_size;
uniform int elements_count;
#else
const int tex_elements_size = $tex_elements_size;
const int elements_count = $elements_count;
#endif

uniform float area_scale;
uniform vec3 position_scale;
uniform vec3 position_shift;

/* ATI (Radeon) on Linux (fglrx) doesn't tolerate const floats in code. */
uniform float zero_5;
uniform float pi;

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
    current_normal = (current_normal - zero_5) * 2.0;

    /* Another happy ATI (Radeon) on Linux (fglrx) bug:
       When for testing I did "color = 0.0"
       initialization, it... never executed "current_index < elements_count"
       branch. Yes, I'm sure --- changing color initializer changes it's
       idea of "current_index < elements_count". FUBAR optimizer.

       So beware if experimenting with this.
    */
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
        element_normal = (element_normal - zero_5) * 2.0;

        vec3 direction_to_current = current_pos - element_pos;
        float cos_emitter_angle = dot(normalize(direction_to_current), element_normal);
        float sqr_distance = dot(direction_to_current, direction_to_current);

        float cos_current_angle = dot(-direction_to_current, current_normal);

        color -= (element_area * abs(cos_emitter_angle)
          /* TODO:
          * max(0.0, cos_current_angle) */ / (4.0 * pi * sqr_distance))
          #ifdef PASS_2
          * texture2D(tex_elements_intensity, element_st).x
          #endif
          ;
      }
    }

    /* TODO: average */

    gl_FragColor = vec4(color, color, color, 1.0);
  }
  else
  {
    /* Only for testing, to mark colors not corresponding to any element. */
    gl_FragColor = vec4(1, 0, 0, 1.0);
  }
}
