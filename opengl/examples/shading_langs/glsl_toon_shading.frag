//uniform vec4 Colour1,Colour2,Colour3,Colour4; // "shades"
varying vec3 normal;
void main()
{
  float intensity;
  vec4 colour;
  // Normalize the normal, again
  vec3 n = normalize(normal);
  // Normalize light direction and convert to a vec3
  vec3 lDir = normalize(vec3(gl_LightSource[0].position));
  // Compute light intensity using dot product
  intensity = dot(lDir,n);
  // Compute light intensity using dot product
  intensity = dot(lDir,n);
  // Decide which shade to used based on intensity
  if (intensity > 0.95)
     colour = vec4(1, 1, 1, 1);
  else if (intensity > 0.5)
     colour = vec4(0.75, 0.75, 0.75, 1);
  else if (intensity > 0.25)
     colour = vec4(0.5, 0.5, 0.5, 1);
  else
     colour = vec4(0.25, 0.25, 0.25, 1);
  // Finally, set destination colour
  gl_FragColor = colour;
}