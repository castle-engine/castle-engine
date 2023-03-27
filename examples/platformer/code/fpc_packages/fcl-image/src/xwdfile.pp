{
  Headers for the xwd image format.
  
  The original headers are part of the X11 headers located at:
  /usr/X11R6/include/X11/XWDFile.h
  or
  Mandriva 2006: /usr/include/X11/XWDFile.h
  
  But the file was added to fcl-image so that xwd files can be read in any system.
  
  Authors of the C to Pascal conversion:
  
  Felipe Monteiro de Carvalho
}
{***********************************************************
Copyright 1985, 1986, 1998  The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from The Open Group.

******************************************************************}
{ $XFree86: xc/include/XWDFile.h,v 1.3 Mon Jan 9 14:58:15 2006 UTC by dawes $ }

{
 * XWDFile.h	MIT Project Athena, X Window system window raster
 *		image dumper, dump file format header file.
 *
 *  Author:	Tony Della Fera, DEC
 *		27-Jun-85
 * 
 * Modifier:    William F. Wyatt, SAO
 *              18-Nov-86  - version 6 for saving/restoring color maps
}
unit xwdfile;

interface

{$PACKRECORDS c}

const
  XWD_FILE_VERSION = 7;
  sz_XWDheader = 100;
  sz_XWDColor = 12;

{ Values in the file are most significant byte first. }

type
  TXWDFileHeader = record
    { header_size = SIZEOF(XWDheader) + length of null-terminated
    * window name. }
    header_size: Cardinal;

    file_version: Cardinal;	// = XWD_FILE_VERSION above */
    pixmap_format: Cardinal;	// ZPixmap or XYPixmap */
    pixmap_depth: Cardinal;	// Pixmap depth */
    pixmap_width: Cardinal;	// Pixmap width */
    pixmap_height: Cardinal;	// Pixmap height */
    xoffset: Cardinal;		// Bitmap x offset, normally 0 */
    byte_order: Cardinal;	// of image data: MSBFirst, LSBFirst */

    { bitmap_unit applies to bitmaps (depth 1 format XY) only.
    * It is the number of bits that each scanline is padded to. }
    bitmap_unit: Cardinal;		

    bitmap_bit_order: Cardinal;	// bitmaps only: MSBFirst, LSBFirst */

    { bitmap_pad applies to pixmaps (non-bitmaps) only.
    * It is the number of bits that each scanline is padded to. }
    bitmap_pad: Cardinal;

    bits_per_pixel: Cardinal;	// Bits per pixel */

    { bytes_per_line is pixmap_width padded to bitmap_unit (bitmaps)
    * or bitmap_pad (pixmaps).  It is the delta (in bytes) to get
    * to the same x position on an adjacent row. }
    bytes_per_line: Cardinal;
    visual_class: Cardinal;	// Class of colormap
    red_mask: Cardinal;		// Z red mask
    green_mask: Cardinal;	// Z green mask
    blue_mask: Cardinal;	// Z blue mask
    bits_per_rgb: Cardinal;	// Log2 of distinct color values
    colormap_entries: Cardinal; // Number of entries in colormap; not used?
    ncolors: Cardinal;		// Number of XWDColor structures
    window_width: Cardinal;	// Window width
    window_height: Cardinal;	// Window height
    window_x: Cardinal;		// Window upper left X coordinate
    window_y: Cardinal;		// Window upper left Y coordinate
    window_bdrwidth: Cardinal;	// Window border width
  end;

{ Null-terminated window name follows the above structure. }

{ Next comes XWDColor structures, at offset XWDFileHeader.header_size in
 * the file.  XWDFileHeader.ncolors tells how many XWDColor structures
 * there are.
 }

  TXWDColor = record
    pixel: Cardinal;
    red: Word;
    green: Word;
    blue: Word;
    flags: Char;
    pad: Char;
  end;

{ Last comes the image data in the format described by XWDFileHeader. }

implementation

end.
