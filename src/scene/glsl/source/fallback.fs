/* Fallback GLSL fragment shader,
   used when we need a shader (e.g. because it is OpenGLES or
   GLFeatures.EnableFixedFunction = false)
   but the default shader did not compile.
   You cannot leave the shader empty in OpenGLES. */

void main(void)
{
  gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0); // ugly color to stand out
}
