uniform sampler2D tex_elements_position_area;
uniform sampler2D tex_elements_normal;
uniform int tex_elements_size;

uniform float area_scale;
uniform vec3 position_scale;
uniform vec3 position_shift;

uniform int elements_count;

/* ATI (Radeon) on Linux (fglrx) doesn't tolerate const floats in code. */
uniform float zero_5;
uniform float pi;

void main(void)
{
  gl_Position = ftransform();

  float color = 1.0;
  /* TODO: this is bad. we want it transformed by shape transform, nothing else
     (no camera). */
  vec3 position = gl_Position.xyz;

  for (int i = 0; i < elements_count; i++)
  {
    vec2 element_st = vec2(
      float(i % tex_elements_size) / tex_elements_size,
      float(i / tex_elements_size) / tex_elements_size);

    vec4 element_pos_area = texture2D(tex_elements_position_area, element_st);
    vec3 element_pos = (element_pos_area.xyz + position_shift) * position_scale;
    float element_area = element_pos_area.w * area_scale;

    vec3 element_normal = texture2D(tex_elements_normal, element_st).xyz;
    element_normal = (element_normal - zero_5) * 2.0;

    vec3 direction_to_pos = position - element_pos;
    float cos_emitter_angle = dot(normalize(direction_to_pos), element_normal);
    float sqr_distance = dot(direction_to_pos, direction_to_pos);

    color -= element_area * cos_emitter_angle / (4.0 * pi * sqr_distance);
  }

  gl_FrontColor = vec4(color, color, color, 1.0);
}
