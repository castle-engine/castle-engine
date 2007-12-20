// Taken from LightHouse3D.com tutorial, actually Kambi copied this from
// http://www.comp.leeds.ac.uk/vvr/papers/gpu-vvr2.pdf

varying vec3 normal;   // normal vector
void main()
{
  // Transform the normal to view coordinates
  // and normalize it
  normal = normalize(gl_NormalMatrix * gl_Normal);
  // Finally, transform vertex position
  /* Kambi fixed this to use gl_ModelViewProjectionMatrix,
     not only gl_ModelViewMatrix */
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
