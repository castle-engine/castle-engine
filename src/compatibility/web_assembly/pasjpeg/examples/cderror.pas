Unit CdError;

{ This file defines the error and message codes for the cjpeg/djpeg
  applications.  These strings are not needed as part of the JPEG library
  proper.
  Edit this file to add new codes, or to translate the message strings to
  some other language. }

{ Original cderror.h  ; Copyright (C) 1994, Thomas G. Lane.  }

interface

{ To define the enum list of message codes, include this file without
  defining macro JMESSAGE.  To create a message string table, include it
  again with a suitable JMESSAGE definition (see jerror.c for an example). }


{$define TARGA_SUPPORTED}
{$define BMP_SUPPORTED}
{$define GIF_SUPPORTED}
{$define PPM_SUPPORTED}
{$define RLE_SUPPORTED}

type
  ADDON_MESSAGE_CODE =(

     JMSG_FIRSTADDONCODE,  { Must be first entry! }

   {$ifdef BMP_SUPPORTED}
     JERR_BMP_BADCMAP,  { Unsupported BMP colormap format }
     JERR_BMP_BADDEPTH,  { Only 8- and 24-bit BMP files are supported }
     JERR_BMP_BADHEADER,  { Invalid BMP file: bad header length }
     JERR_BMP_BADPLANES,  { Invalid BMP file: biPlanes not equal to 1 }
     JERR_BMP_COLORSPACE,  { BMP output must be grayscale or RGB }
     JERR_BMP_COMPRESSED,  { Sorry, compressed BMPs not yet supported }
     JERR_BMP_NOT,  { Not a BMP file - does not start with BM }
     JTRC_BMP,  { %ux%u 24-bit BMP image }
     JTRC_BMP_MAPPED,  { %ux%u 8-bit colormapped BMP image }
     JTRC_BMP_OS2,  { %ux%u 24-bit OS2 BMP image }
     JTRC_BMP_OS2_MAPPED,  { %ux%u 8-bit colormapped OS2 BMP image }
   {$endif} { BMP_SUPPORTED }

   {$ifdef GIF_SUPPORTED}
     JERR_GIF_BUG,  { GIF output got confused }
     JERR_GIF_CODESIZE,  { Bogus GIF codesize %d }
     JERR_GIF_COLORSPACE,  { GIF output must be grayscale or RGB }
     JERR_GIF_IMAGENOTFOUND,  { Too few images in GIF file }
     JERR_GIF_NOT,  { Not a GIF file }
     JTRC_GIF,  { %ux%ux%d GIF image }
     JTRC_GIF_BADVERSION,
              { Warning: unexpected GIF version number '%c%c%c' }
     JTRC_GIF_EXTENSION,  { Ignoring GIF extension block of type 0x%02x }
     JTRC_GIF_NONSQUARE,  { Caution: nonsquare pixels in input }
     JWRN_GIF_BADDATA,  { Corrupt data in GIF file }
     JWRN_GIF_CHAR,  { Bogus char 0x%02x in GIF file, ignoring }
     JWRN_GIF_ENDCODE,  { Premature end of GIF image }
     JWRN_GIF_NOMOREDATA,  { Ran out of GIF bits }
   {$endif} { GIF_SUPPORTED }

   {$ifdef PPM_SUPPORTED}
     JERR_PPM_COLORSPACE,  { PPM output must be grayscale or RGB }
     JERR_PPM_NONNUMERIC,  { Nonnumeric data in PPM file }
     JERR_PPM_NOT,  { Not a PPM file }
     JTRC_PGM,  { %ux%u PGM image }
     JTRC_PGM_TEXT,  { %ux%u text PGM image }
     JTRC_PPM,  { %ux%u PPM image }
     JTRC_PPM_TEXT,  { %ux%u text PPM image }
   {$endif} { PPM_SUPPORTED }

   {$ifdef RLE_SUPPORTED}
     JERR_RLE_BADERROR,  { Bogus error code from RLE library }
     JERR_RLE_COLORSPACE,  { RLE output must be grayscale or RGB }
     JERR_RLE_DIMENSIONS,  { Image dimensions (%ux%u) too large for RLE }
     JERR_RLE_EMPTY,  { Empty RLE file }
     JERR_RLE_EOF,  { Premature EOF in RLE header }
     JERR_RLE_MEM,  { Insufficient memory for RLE header }
     JERR_RLE_NOT,  { Not an RLE file }
     JERR_RLE_TOOMANYCHANNELS,  { Cannot handle %d output channels for RLE }
     JERR_RLE_UNSUPPORTED,  { Cannot handle this RLE setup }
     JTRC_RLE,  { %ux%u full-color RLE file }
     JTRC_RLE_FULLMAP,  { %ux%u full-color RLE file with map of length %d }
     JTRC_RLE_GRAY,  { %ux%u grayscale RLE file }
     JTRC_RLE_MAPGRAY,  { %ux%u grayscale RLE file with map of length %d }
     JTRC_RLE_MAPPED,  { %ux%u colormapped RLE file with map of length %d }
   {$endif} { RLE_SUPPORTED }

   {$ifdef TARGA_SUPPORTED}
     JERR_TGA_BADCMAP,  { Unsupported Targa colormap format }
     JERR_TGA_BADPARMS,  { Invalid or unsupported Targa file }
     JERR_TGA_COLORSPACE,  { Targa output must be grayscale or RGB }
     JTRC_TGA,  { %ux%u RGB Targa image }
     JTRC_TGA_GRAY,  { %ux%u grayscale Targa image }
     JTRC_TGA_MAPPED,  { %ux%u colormapped Targa image }
   {$else}
     JERR_TGA_NOTCOMP,  { Targa support was not compiled }
   {$endif} { TARGA_SUPPORTED }

     JERR_BAD_CMAP_FILE,
            { Color map file is invalid or of unsupported format }
     JERR_TOO_MANY_COLORS,
            { Output file format cannot handle %d colormap entries }
     JERR_UNGETC_FAILED,  { ungetc failed }
   {$ifdef TARGA_SUPPORTED}
     JERR_UNKNOWN_FORMAT,
            { Unrecognized input file format --- perhaps you need -targa }
   {$else}
     JERR_UNKNOWN_FORMAT,  { Unrecognized input file format }
   {$endif}
     JERR_UNSUPPORTED_FORMAT,  { Unsupported output file format }

     JMSG_LASTADDONCODE
   );

type
  msg_table = Array[ADDON_MESSAGE_CODE] of string[80];
const
  cdjpeg_message_table : msg_table = (

  { JMSG_FIRSTADDONCODE }  '', { Must be first entry! }

{$ifdef BMP_SUPPORTED}
  { JERR_BMP_BADCMAP } 'Unsupported BMP colormap format',
  { JERR_BMP_BADDEPTH } 'Only 8- and 24-bit BMP files are supported',
  { JERR_BMP_BADHEADER } 'Invalid BMP file: bad header length',
  { JERR_BMP_BADPLANES } 'Invalid BMP file: biPlanes not equal to 1',
  { JERR_BMP_COLORSPACE } 'BMP output must be grayscale or RGB',
  { JERR_BMP_COMPRESSED } 'Sorry, compressed BMPs not yet supported',
  { JERR_BMP_NOT } 'Not a BMP file - does not start with BM',
  { JTRC_BMP } '%ux%u 24-bit BMP image',
  { JTRC_BMP_MAPPED } '%ux%u 8-bit colormapped BMP image',
  { JTRC_BMP_OS2 } '%ux%u 24-bit OS2 BMP image',
  { JTRC_BMP_OS2_MAPPED } '%ux%u 8-bit colormapped OS2 BMP image',
{$endif} { BMP_SUPPORTED }

{$ifdef GIF_SUPPORTED}
  { JERR_GIF_BUG } 'GIF output got confused',
  { JERR_GIF_CODESIZE } 'Bogus GIF codesize %d',
  { JERR_GIF_COLORSPACE } 'GIF output must be grayscale or RGB',
  { JERR_GIF_IMAGENOTFOUND } 'Too few images in GIF file',
  { JERR_GIF_NOT } 'Not a GIF file',
  { JTRC_GIF } '%ux%ux%d GIF image',
  { JTRC_GIF_BADVERSION }
           'Warning: unexpected GIF version number "%c%c%c"',
  { JTRC_GIF_EXTENSION } 'Ignoring GIF extension block of type 0x%02x',
  { JTRC_GIF_NONSQUARE } 'Caution: nonsquare pixels in input',
  { JWRN_GIF_BADDATA } 'Corrupt data in GIF file',
  { JWRN_GIF_CHAR } 'Bogus char 0x%02x in GIF file, ignoring',
  { JWRN_GIF_ENDCODE } 'Premature end of GIF image',
  { JWRN_GIF_NOMOREDATA } 'Ran out of GIF bits',
{$endif} { GIF_SUPPORTED }

{$ifdef PPM_SUPPORTED}
  { JERR_PPM_COLORSPACE } 'PPM output must be grayscale or RGB',
  { JERR_PPM_NONNUMERIC } 'Nonnumeric data in PPM file',
  { JERR_PPM_NOT } 'Not a PPM file',
  { JTRC_PGM } '%ux%u PGM image',
  { JTRC_PGM_TEXT } '%ux%u text PGM image',
  { JTRC_PPM } '%ux%u PPM image',
  { JTRC_PPM_TEXT } '%ux%u text PPM image',
{$endif} { PPM_SUPPORTED }

{$ifdef RLE_SUPPORTED}
  { JERR_RLE_BADERROR } 'Bogus error code from RLE library',
  { JERR_RLE_COLORSPACE } 'RLE output must be grayscale or RGB',
  { JERR_RLE_DIMENSIONS } 'Image dimensions (%ux%u) too large for RLE',
  { JERR_RLE_EMPTY } 'Empty RLE file',
  { JERR_RLE_EOF } 'Premature EOF in RLE header',
  { JERR_RLE_MEM } 'Insufficient memory for RLE header',
  { JERR_RLE_NOT } 'Not an RLE file',
  { JERR_RLE_TOOMANYCHANNELS } 'Cannot handle %d output channels for RLE',
  { JERR_RLE_UNSUPPORTED } 'Cannot handle this RLE setup',
  { JTRC_RLE } '%ux%u full-color RLE file',
  { JTRC_RLE_FULLMAP } '%ux%u full-color RLE file with map of length %d',
  { JTRC_RLE_GRAY } '%ux%u grayscale RLE file',
  { JTRC_RLE_MAPGRAY } '%ux%u grayscale RLE file with map of length %d',
  { JTRC_RLE_MAPPED } '%ux%u colormapped RLE file with map of length %d',
{$endif} { RLE_SUPPORTED }

{$ifdef TARGA_SUPPORTED}
  { JERR_TGA_BADCMAP } 'Unsupported Targa colormap format',
  { JERR_TGA_BADPARMS } 'Invalid or unsupported Targa file',
  { JERR_TGA_COLORSPACE } 'Targa output must be grayscale or RGB',
  { JTRC_TGA } '%ux%u RGB Targa image',
  { JTRC_TGA_GRAY } '%ux%u grayscale Targa image',
  { JTRC_TGA_MAPPED } '%ux%u colormapped Targa image',
{$else}
  { JERR_TGA_NOTCOMP } 'Targa support was not compiled',
{$endif} { TARGA_SUPPORTED }

  { JERR_BAD_CMAP_FILE }
         'Color map file is invalid or of unsupported format',
  { JERR_TOO_MANY_COLORS }
         'Output file format cannot handle %d colormap entries',
  { JERR_UNGETC_FAILED } 'ungetc failed',
{$ifdef TARGA_SUPPORTED}
  { JERR_UNKNOWN_FORMAT }
         'Unrecognized input file format --- perhaps you need -targa',
{$else}
  { JERR_UNKNOWN_FORMAT } 'Unrecognized input file format',
{$endif}
  { JERR_UNSUPPORTED_FORMAT } 'Unsupported output file format',


  { JMSG_LASTADDONCODE } '');

implementation

end.
