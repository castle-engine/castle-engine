/* Simplest GLSL fragment shader,
   used when we want to cover the pixels with anything. */

#ifdef GL_ES
precision mediump float;
#endif

void main(void)
{
  gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0); // ugly color to stand out
}
