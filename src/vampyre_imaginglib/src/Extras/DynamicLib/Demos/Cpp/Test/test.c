/* Vampyre Imaging Library Demo
   test (C/C++, dll library usage, Win32/Linux)
   tested in MSVC 7.1, GCC 3.2
   written by Marek Mauder
  
   Simple test program that shows how to use Imaging library from C/C++
   environment.
 
   Important:
     Compiled Imaging library must be located somewhere on system's
     search path for this sample to work (usually VampyreImaging.dll
     in C:\Windows or libVampyreImaging.so in /lib).
*/

#include "ImagingImport.h"
#include "stdio.h"

#ifdef __cplusplus
using namespace Imaging;
#endif

int main(void)
{
  TImageData Img;
  TColor32Rec * Col;
  int X = 0;
  int Major, Minor, Patch;
  
  /* load all library functions */
  if (!ImLoadLibrary())
  {
    printf("Error loading library");
    return 1; 
  }
  /* get Imaging library version */
  ImGetVersion(&Major, &Minor, &Patch);
  printf("Imaging test (library version %d.%d.%d)\n", Major, Minor, Patch);
  /* initialize image data struct */
  ImInitImage(&Img);
  /* create new 256x256 32bit image with alpha */
  ImNewImage(256, 256, ifA8R8G8B8, &Img);
  
  /* draw diagonal line across the image */
  for (;X < Img.Width; X++)
  {
    Col = (TColor32Rec*)Img.Bits + X * Img.Width + X;
    Col->A = 0xff;
    Col->R = X;
    Col->G = 0xff - X;
    Col->B = X / 2;
  }

  /* save image to Targa format */
  ImSaveImageToFile("ctestimage.png", &Img);
  /* free image data */
  ImFreeImage(&Img);
  /* free library */
  ImFreeLibrary();
  
  printf("Image written to 'ctestimage.png'\n");
  
  return 0;
}  
      
   
  
