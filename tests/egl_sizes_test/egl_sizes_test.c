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

  /*
    X types.
    These are pointer-sized, in practice (32 on 32-bit systems, 64 on 64-bit).

    Note: Ignore a confusing statement from X.h header, that says:

      """
      * _XSERVER64 must ONLY be defined when compiling X server sources on
      * systems where unsigned long is not 32 bits, must NOT be used in
      * client or library code.
      """

    and does

      """
      #ifndef _XSERVER64
      ...
      typedef unsigned long XID;
      #else
      ...
      typedef CARD32 XID;
      """

    So it seems that X developers wanted XID to be always 32-bit,
    even on 64-bit systems... But actually _XSERVER64 is not defined, ever.

    As this program shows in fact, on x86_64 systems,
    - "unsigned long" is 64-bit
    - and X.h *does not* define _XSERVER64
    - and thus XID is also 64-bit
  */
  printf("XID size: %zu\n", sizeof(XID));
  printf("Pixmap size: %zu\n", sizeof(Pixmap));
  printf("Window size: %zu\n", sizeof(Window));
}
