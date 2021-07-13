void main (void)
{
  vec4 color = screen_get_color(screen_position());
  gl_FragColor = vec4(0.0, 0.0, 0.0, color.a * (1.0 - color.x));
}
