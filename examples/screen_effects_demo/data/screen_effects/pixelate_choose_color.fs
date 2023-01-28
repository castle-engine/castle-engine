const int box_size = 4;

void main (void)
{
  // round screen position to the nearest box_size multiple
  int box_x = box_size * (screen_x() / box_size);
  int box_y = box_size * (screen_y() / box_size);

  /* Choose max or min, depending on which is more often */
  vec4 max_color = vec4(0.0);
  float max_intensity = 0.0;
  int max_count = 0;

  vec4 min_color = vec4(1.0);
  float min_intensity = 4.0;
  int min_count = 0;

  for (int i = 0; i < box_size; i++) {
    for (int j = 0; j < box_size; j++) {
      vec4 new_color = screen_get_color(ivec2(box_x + i, box_y + j));
      float new_intensity = new_color.r + new_color.g + new_color.b + new_color.a;
      if (new_intensity > max_intensity) {
        max_intensity = new_intensity;
        max_color = new_color;
        max_count++;
      }
      if (new_intensity < min_intensity) {
        min_intensity = new_intensity;
        min_color = new_color;
        min_count++;
      }
    }
  }

  gl_FragColor = (max_count > min_count ? max_color : min_color);
}
