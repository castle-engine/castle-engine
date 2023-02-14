/* Simplest GLSL fragment shader,
   used when we want to cover the pixels with one color. */

#ifdef GL_ES
precision mediump float;
#endif

uniform vec4 color;

void main(void)
{
  gl_FragColor = color;
}
