/*
  Print sizes of some C, EGL, X types.
  To make sure our Pascal headers have same size for these types.
*/

#include <stdio.h>

#include <EGL/egl.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

void main()
{
  /* Note: Use %zu to print size_t values.
      See https://stackoverflow.com/questions/5943840/how-do-i-print-the-size-of-int-in-c#5943869
  */

  // C types
  printf("int size: %zu\n", sizeof(int));
  printf("unsigned int size: %zu\n", sizeof(unsigned int));
  printf("unsigned long size: %zu\n", sizeof(unsigned long));

  // EGL types
  printf("EGLint size: %zu\n", sizeof(EGLint));

  // EGL types that are aliases to other libraries (X, WinAPI) types,
  // we have to be ultra-careful with these to have same size in Pascal headers.
  printf("EGLNativeDisplayType size: %zu\n", sizeof(EGLNativeDisplayType));
  printf("EGLNativePixmapType size: %zu\n", sizeof(EGLNativePixmapType));
  printf("EGLNativeWindowType size: %zu\n", sizeof(EGLNativeWindowType));

  // X types
  printf("Pixmap size: %zu\n", sizeof(Pixmap));
  printf("Window size: %zu\n", sizeof(Window));
}
