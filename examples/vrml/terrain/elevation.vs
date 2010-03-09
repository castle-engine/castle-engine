varying vec3 position;

void main(void)
{
  position = vec3(gl_Vertex);
  gl_FrontColor = gl_Color;
  gl_Position = ftransform();
}
