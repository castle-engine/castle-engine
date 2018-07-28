// for modulo operation
#extension GL_EXT_gpu_shader4 : enable

ivec2 screen_position();
vec4 screen_get_color(ivec2 position);
int screen_x();
int screen_y();

const int box_size = 4;

void main (void)
{
  int box_x = screen_x() - screen_x() % box_size;
  int box_y = screen_y() - screen_y() % box_size;
  vec4 color = vec4(0.0);
  for (int i = 0; i < box_size; i++) {
    for (int j = 0; j < box_size; j++) {
      color += screen_get_color(ivec2(box_x + i, box_y + j));
    }
  }
  color /= float(box_size * box_size);
  gl_FragColor = color;
}
