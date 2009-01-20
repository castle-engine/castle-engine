void main(void)
{
  gl_Position = ftransform();
  gl_FrontColor = vec4(gl_Vertex.z, gl_Vertex.z, gl_Vertex.z, 1);
}
