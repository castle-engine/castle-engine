varying vec3 position;
varying vec3 normal;

void main(void)
{
  position = vec3(gl_Vertex);
  normal = vec3(gl_Normal);
  gl_FrontColor = gl_Color;
  gl_Position = ftransform();
}
