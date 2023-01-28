{*============================================================================
 * Image Debugging API
 *
 * This API can simplify the debugging of applications that handle 2D data.
 * It basically allows you to do 'printf' style debugging of images in 
 * C and C++ applications.
 *
 * Basic Usage:
 *  #include <imdebug.h>
 *  ...
 *  imdebug(format_string, ...);
 *
 *  The format string describes the type of image(s) to dump to the debug
 *  window. Here are some examples
 *
 *  Show an rgb image with width and height 16:
 *
 *     imdebug("rgb w=%d h=%d %p", 16, 16, img);
 *   
 * VARIABLE SPEC:
 *  %d  -  int
 *  %f  -  float
 *  %s  -  string
 *  %p  -  picture (pointer to raw image data)
 *  
 * INPUT FORMAT SPEC:
 *  rgb
 *  bgr
 *  abgr
 *  rgba...   - specify input image channel order
 *  lum       - 1-channel image: lumninance
 *  luma      - 2-channel image: lumninance + alpha
 *  #5        - 5-channel data (default is to use 0,1,2 as RGB, no alpha)
 *  ---> (default is rgb)
 *
 *  b=8       - size of all channels is 8 bits
 *  b=5,6,5   - size of channels is 5bits(R) 6bits(G) 5bits(B)
 *  b=32f     - size of all channels is 32 bits, float format
 *  ---> (default is b=8)
 *
 * OUTPUT FORMAT SPEC:
 *  rgba=rg__ - just display the red and green chanels
 *  rgba=aaa_ - display alpha as grayscale
 *  lum=g     - display green as grayscale
 *  rgb=#A1C  - r:=chan 10, g:=chan 1, b:=chan 12 (use HEX digits!)
 *  ---> (default is 1-1 mapping with no translation or swizzling)
 *
 * ATTRIBUTE SPEC:
 *  w=23      - width  is 23 (default 0)
 *  h=17      - height is 17 (default 0)
 *  rs=1      - skip 1 row after every row  (default 0)
 *  cs=2      - skip 2 columns after every column (default 0)
 *
 * SCALE AND BIAS:
 *  *1.2      - scale RGB by 1.2
 *  /1.2      - scale RGB by 1/1.2
 *  +128      - bias  RGB by 128
 *  -0.5      - bias  RGB by -0.5
 *  r*1.2     - scale red by 1.2
 *  rb/1.2    - scale both red and blue by 1/1.2
 *  a+128     - bias  alpha by 128
 *  rgba-0.5  - bias  RGB and alpha by -0.5
 *  *auto     - automatically scale & bias RGB based on max & min values
 *  ra*auto   - automatically scale & bias red and alpha
 *  --> Default is scale=1 and bias=0 for all channels
 *  --> Output value is computed as  (x*scale)+bias, 
 *       the same order as with OpenGL glPixelTransfer functions.
 *
 *  Order of specifiers is mostly not important, but channel swizzeling
 *  should come after input format specifier. 
 *  (i.e. do "rgb bgr=rgb",   not "bgr=rgb  rgb")
 *
 *  If no image is specified (with '%p'), then the previous 
 *  image data is used.
 *
 * THOUGHTS FOR THE FUTURE
 *
 *  handling raw graphic images in compressed form. e.g.:
 *  %pj = specify raw jpg image buffer
 *  %pp = specify raw png image buffer
 *  %pb = specify raw bmp image buffer
 *
 *  A way to perform math on images in the format specifier,
 *  or perform other arbitrary transformations on the input.
 *  Like "%p - 0.5*%p"
 *
 *  A way to specify that several images should be tiled,
 *  or opened in separate display windows.
 *
 *  More control over padding and alignment specs.
 *
 *  Ooops!  Really should allow specification of endianness!
 *  That would be handy for data not created in the local endian format.
 *
 *  Author:        William Baxter (baxter@cs.unc.edu)
 *  Created:       Sept 2002
 *  Last Modified: Jan 2003
 *============================================================================
 *  Copyright 2002 
 *        William Baxter
 *        The University of North Carolina at Chapel Hill
 * 
 *  Permission to use, copy, modify, distribute and sell this software
 *  and its documentation for any purpose is hereby granted without
 *  fee, provided that the above copyright notice appear in all copies
 *  and that both that copyright notice and this permission notice
 *  appear in supporting documentation.  Binaries may be compiled with
 *  this software without any royalties or restrictions.
 *
 *  The University of North Carolina at Chapel Hill makes no
 *  representations about the suitability of this software for any
 *  purpose. It is provided "as is" without express or implied
 *  warranty.
 *============================================================================
 *}

{
  Image Debugging API Pascal Conversion
  by Marek Mauder
}
unit PasImDebug;

interface

{$IFDEF MSWINDOWS}
const
  DLLName = 'imdebug.dll';
{$ENDIF}
{$IFDEF LINUX}
const
  DLLName = 'imdebug.so';
{$ENDIF}

procedure imdebug(const Format: PAnsiChar); cdecl; external DLLName; varargs;

implementation

end.
