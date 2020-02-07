#ifdef GL_ES
precision mediump float;
#endif

uniform vec4 color;

void main(void)
{
  gl_FragColor = color;
}
