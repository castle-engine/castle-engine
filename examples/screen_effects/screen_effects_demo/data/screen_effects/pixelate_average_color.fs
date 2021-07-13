const int box_size = 4;

void main (void)
{
  // round screen position to the nearest box_size multiple
  int box_x = box_size * (screen_x() / box_size);
  int box_y = box_size * (screen_y() / box_size);

  vec4 color = vec4(0.0);
  for (int i = 0; i < box_size; i++) {
    for (int j = 0; j < box_size; j++) {
      color += screen_get_color(ivec2(box_x + i, box_y + j));
    }
  }
  color /= float(box_size * box_size);
  gl_FragColor = color;
}
