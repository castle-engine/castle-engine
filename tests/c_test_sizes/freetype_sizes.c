/*
  Test FreeType type sizes.

  Build with
  gcc $(pkg-config --cflags --libs freetype2) freetype_sizes.c -ofreetype_sizes

  See Pascal CastleInternalFreeTypeH and TTestCastleInternalFreeTypeH.TestTypeSizes .
*/

// including freetype: see https://freetype.org/freetype2/docs/tutorial/step1.html
#include <ft2build.h>
#include FT_FREETYPE_H

#include <stdio.h>

int main() {
  printf("FT_Bitmap size: %zu\n", sizeof(FT_Bitmap));

  FT_Bitmap bitmap;
  printf("FT_Bitmap.num_grays size: %zu\n", sizeof(bitmap.num_grays));

  long l;
  printf("long size: %zu\n", sizeof(l));

  unsigned long ul;
  printf("unsigned long size: %zu\n", sizeof(ul));

  FT_Pos pos;
  printf("FT_Pos size: %zu\n", sizeof(pos));

  return 0;
}
