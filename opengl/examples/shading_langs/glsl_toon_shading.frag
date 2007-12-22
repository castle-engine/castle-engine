// Kambi changed these to be const in fragment code
// uniform vec4 Colour1,Colour2,Colour3,Colour4; // "shades"

/* TODO: This doesn't work like it should on Radeon with closed ATI
   drivers on Linux MacBookPro. Don't know why, seems like comparison
   "intensity > ..." is always true unless "..." is 0, and
   constant vector values also work only as 0 / 1.
   Effectively, seems that float constants are rounded up to integer
   values.... Weird. */

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
     colour = vec4(1, 1, 0, 1);
  else if (intensity > 0.5)
     colour = vec4(0.75, 0.75, 0, 1);
  else if (intensity > 0.25)
     colour = vec4(0.5, 0.5, 0, 1);
  else
     colour = vec4(0.25, 0.25, 0, 1);
  // Finally, set destination colour
  gl_FragColor = colour;
}