/* Fallback GLSL shader, used for OpenGL ES, when normal shader did not compile.
   This is merely used to avoid crashing the application (you cannot leave
   the shader empty). */

precision mediump float;

void main(void)
{
  gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0); // ugly color to stand out
}
