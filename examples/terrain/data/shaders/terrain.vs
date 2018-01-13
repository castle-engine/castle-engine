varying vec3 position;
varying vec3 normal;

#ifdef FOG
varying float camera_distance;
#endif

void main(void)
{
#ifdef HEIGHT_IS_Z
  position = vec3(gl_Vertex);
  normal = vec3(gl_Normal);
#else
  position.xyz = vec3(gl_Vertex).xzy;
  position.y = -position.y;
  normal.xyz = vec3(gl_Normal).xzy;
  normal.y = -normal.y;
#endif

#ifdef FOG
  camera_distance = length(gl_ModelViewMatrix * gl_Vertex);
#endif
  gl_FrontColor = gl_Color;
  gl_Position = ftransform();
}
