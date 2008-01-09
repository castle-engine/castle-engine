// Kambi changed these to be const in fragment code
// uniform vec4 Colour1,Colour2,Colour3,Colour4; // "shades"

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

  // Decide which shade to used based on intensity

  /* Kambi: originally, this was like
     "intensity > 0.95" etc.
     But on fglrx (Radeon closed-source drivers on chantal)
     it looks like constant floats are simply rounded up.

     So I code it to avoid using const floats,
     "intensity * 100 > 95" is equivalent to "intensity > 90 / 100"
     and it works Ok.
     I also avoid using vector color constants, that's why
     color is calculated so strangely.

     Full original code, for clarity:

       if (intensity > 0.95)
	  colour = vec4(1, 1, 0, 1);
       else if (intensity > 0.5)
	 colour = vec4(0.75, 0.75, 0, 1);
       else if (intensity > 0.25)
	  colour = vec4(0.5, 0.5, 0, 1);
       else
	  colour = vec4(0.25, 0.25, 0, 1);

  */

  colour = vec4(1, 1, 0, 1);
  if (intensity * 100 <= 95)
  {
    colour /= 4.0;
    if (intensity * 100 > 50)
      colour *= 3.0; else
    if (intensity * 100 > 25)
      colour *= 2.0;
    /* else stays as vec4(0.25, 0.25, 0, 1); */
  }

  // Finally, set destination colour
  gl_FragColor = colour;
}