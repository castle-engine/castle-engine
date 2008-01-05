/* This shader doesn't have much sense, it's only to see how it's translated
   to ARB fragment program:

     cgc -oglsl -profile arbfp1 glsl_test_loop.fs

   And note how loop is unrolled, with all values of "i" as constants.
   Simpler versions:
     f += gl_TexCoord[0].x/10;
   (no need to store "i" values) or
     f += 0.1;
   (no need to compile to loop at all,
   effect of loop may be calculated at compile time).
*/

void main()
{
  float f = 0;

  int i;
  for (i = 0; i < 10; i++)
  {
    f +=  i * gl_TexCoord[0].x/10;
  }

  /* Just to not let calculation of "f" to be optimized out. */
  gl_FragColor = vec4(f, f, f, 1);
}
