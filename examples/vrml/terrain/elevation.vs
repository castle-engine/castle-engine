varying vec3 position;
varying vec3 normal;

#ifdef FOG
varying float camera_distance;
#endif

void main(void)
{
  position = vec3(gl_Vertex);
  normal = vec3(gl_Normal);
#ifdef FOG
  camera_distance = length(gl_ModelViewMatrix * gl_Vertex);
#endif
  gl_FrontColor = gl_Color;
  gl_Position = ftransform();
}
