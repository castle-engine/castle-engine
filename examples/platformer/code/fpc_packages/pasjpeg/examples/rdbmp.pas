Unit RdBmp;

{ rdbmp.c

  Copyright (C) 1994-1996, Thomas G. Lane.
  This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains routines to read input images in Microsoft "BMP"
  format (MS Windows 3.x, OS/2 1.x, and OS/2 2.x flavors).
  Currently, only 8-bit and 24-bit images are supported, not 1-bit or
  4-bit (feeding such low-depth images into JPEG would be silly anyway).
  Also, we don't support RLE-compressed files.

  These routines may need modification for non-Unix environments or
  specialized applications.  As they stand, they assume input from
  an ordinary stdio stream.  They further assume that reading begins
  at the start of the file; start_input may need work if the
  user interface has already read some data (e.g., to determine that
  the file is indeed BMP format).

  This code contributed by James Arthur Boucher. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jpeglib,
  jinclude,
  jdeferr,
  jerror,
  cdjpeg;               { Common decls for cjpeg/djpeg applications }

{ The module selection routine for BMP format input. }

{GLOBAL}
function jinit_read_bmp (cinfo : j_compress_ptr) : cjpeg_source_ptr;

implementation

{ Macros to deal with unsigned chars as efficiently as compiler allows }

{$define HAVE_UNSIGNED_CHAR}
{$ifdef HAVE_UNSIGNED_CHAR}
type
  U_CHAR =  byte;
  UCH = int;
{$else} { !HAVE_UNSIGNED_CHAR }
  {$ifdef CHAR_IS_UNSIGNED}
  type
    U_CHAR = char;
    UCH = int;
  {$else}
  type
    U_CHAR = char;
    UCH = int(x) and $FF
  {$endif}
{$endif} { HAVE_UNSIGNED_CHAR }


{ Private version of data source object }

type
  bmp_source_ptr = ^bmp_source_struct;
  bmp_source_struct = record
    pub : cjpeg_source_struct; { public fields }

    cinfo : j_compress_ptr;             { back link saves passing separate parm }

    colormap : JSAMPARRAY;              { BMP colormap (converted to my format) }

    whole_image : jvirt_sarray_ptr;     { Needed to reverse row order }
    source_row : JDIMENSION;    { Current source row number }
    row_width : JDIMENSION;             { Physical width of scanlines in file }

    bits_per_pixel : int;               { remembers 8- or 24-bit format }
  end; { bmp_source_struct }


{LOCAL}
function read_byte (sinfo : bmp_source_ptr) : int;
{ Read next byte from BMP file }
var
  {register} infile : FILEptr;
  {register} c : byte;
begin
  infile := sinfo^.pub.input_file;
  if JFREAD(infile, @c, 1) <> size_t(1) then
    ERREXIT(j_common_ptr(sinfo^.cinfo), JERR_INPUT_EOF);
  read_byte  := c;
end;


{LOCAL}
procedure read_colormap (sinfo : bmp_source_ptr;
                         cmaplen : int;
                         mapentrysize : int);
{ Read the colormap from a BMP file }
var
  i : int;
begin
  case (mapentrysize) of
  3:{ BGR format (occurs in OS/2 files) }
    for i := 0 to pred(cmaplen) do
    begin
      sinfo^.colormap^[2]^[i] := JSAMPLE (read_byte(sinfo));
      sinfo^.colormap^[1]^[i] := JSAMPLE (read_byte(sinfo));
      sinfo^.colormap^[0]^[i] := JSAMPLE (read_byte(sinfo));
    end;
  4:{ BGR0 format (occurs in MS Windows files) }
    for i := 0 to pred(cmaplen) do
    begin
      sinfo^.colormap^[2]^[i] := JSAMPLE (read_byte(sinfo));
      sinfo^.colormap^[1]^[i] := JSAMPLE (read_byte(sinfo));
      sinfo^.colormap^[0]^[i] := JSAMPLE (read_byte(sinfo));
      {void} read_byte(sinfo);
    end;
  else
    ERREXIT(j_common_ptr(sinfo^.cinfo), JERR_BMP_BADCMAP);
  end;
end;


{ Read one row of pixels.
  The image has been read into the whole_image array, but is otherwise
  unprocessed.  We must read it out in top-to-bottom row order, and if
  it is an 8-bit image, we must expand colormapped pixels to 24bit format. }

{METHODDEF}
function  get_8bit_row (cinfo : j_compress_ptr;
                        sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading 8-bit colormap indexes }
var
  source : bmp_source_ptr;
  {register} colormap : JSAMPARRAY;
  image_ptr : JSAMPARRAY;
  {register} t : int;
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} col : JDIMENSION;
begin
  source := bmp_source_ptr (sinfo);
  colormap := source^.colormap;
  { Fetch next row from virtual array }
  Dec(source^.source_row);
  image_ptr := cinfo^.mem^.access_virt_sarray(
     j_common_ptr (cinfo), source^.whole_image,
     source^.source_row, JDIMENSION (1), FALSE);

  { Expand the colormap indexes to real data }
  inptr := JSAMPLE_PTR(image_ptr^[0]);
  outptr := JSAMPLE_PTR(source^.pub.buffer^[0]);
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    t := GETJSAMPLE(inptr^);
    Inc(inptr);
    outptr^ := colormap^[0]^[t];       { can omit GETJSAMPLE() safely }
    Inc(outptr);
    outptr^ := colormap^[1]^[t];
    Inc(outptr);
    outptr^ := colormap^[2]^[t];
    Inc(outptr);
  end;

  get_8bit_row  := 1;
end;


{METHODDEF}
function get_24bit_row (cinfo : j_compress_ptr;
                        sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading 24-bit pixels }
var
  source : bmp_source_ptr;
  image_ptr : JSAMPARRAY;
  {register} inptr : JSAMPLE_PTR;
  {register} outptr : JSAMPROW;
  {register} col : JDIMENSION;
begin
  source := bmp_source_ptr (sinfo);
  { Fetch next row from virtual array }
  Dec(source^.source_row);
  image_ptr := cinfo^.mem^.access_virt_sarray (
     j_common_ptr (cinfo), source^.whole_image,
     source^.source_row, JDIMENSION (1), FALSE);

  { Transfer data.  Note source values are in BGR order
    (even though Microsoft's own documents say the opposite). }

  inptr := JSAMPLE_PTR(image_ptr^[0]);
  outptr := source^.pub.buffer^[0];
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    outptr^[2] := inptr^;       { can omit GETJSAMPLE() safely }
    Inc(inptr);
    outptr^[1] := inptr^;
    Inc(inptr);
    outptr^[0] := inptr^;
    Inc(inptr);
    Inc(JSAMPLE_PTR(outptr), 3);
  end;

  get_24bit_row := 1;
end;


{ This method loads the image into whole_image during the first call on
  get_pixel_rows.  The get_pixel_rows pointer is then adjusted to call
  get_8bit_row or get_24bit_row on subsequent calls. }

{METHODDEF}
function preload_image (cinfo : j_compress_ptr;
                        sinfo : cjpeg_source_ptr) : JDIMENSION; far;
var
  source : bmp_source_ptr;
  {register} infile : FILEptr;
  {$IFDEF Original}
  {register} c : int;
  {$ENDIF}
  {register} out_ptr : JSAMPLE_PTR;
  image_ptr : JSAMPARRAY;
  row : JDIMENSION;
  {$IFDEF Original}
  col : JDIMENSION;
  {$ENDIF}
  progress : cd_progress_ptr;
begin
  source := bmp_source_ptr (sinfo);
  infile := source^.pub.input_file;
  progress := cd_progress_ptr (cinfo^.progress);

  { Read the data into a virtual array in input-file row order. }
  for row := 0 to pred(cinfo^.image_height) do
  begin
    if (progress <> NIL) then
    begin
      progress^.pub.pass_counter := long (row);
      progress^.pub.pass_limit := long (cinfo^.image_height);
      progress^.pub.progress_monitor (j_common_ptr (cinfo));
    end;
    image_ptr := cinfo^.mem^.access_virt_sarray (
       j_common_ptr (cinfo), source^.whole_image,
       row, JDIMENSION (1), TRUE);
    out_ptr := JSAMPLE_PTR(image_ptr^[0]);
    {$IFDEF Original}
    for col := pred(source^.row_width) downto 0 do
    begin
      { inline copy of read_byte() for speed }
      c := getc(infile);
      if (c = EOF) then
        ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
      out_ptr^ := JSAMPLE (c);
      Inc(out_ptr);
    end;
    {$ELSE}
    if JFREAD(infile, out_ptr, source^.row_width) <>
      size_t(source^.row_width) then
        ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
    {$ENDIF}
  end;
  if (progress <> NIL) then
    Inc(progress^.completed_extra_passes);

  { Set up to read from the virtual array in top-to-bottom order }
  case (source^.bits_per_pixel) of
   8: source^.pub.get_pixel_rows := get_8bit_row;
  24: source^.pub.get_pixel_rows := get_24bit_row;
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADDEPTH);
  end;
  source^.source_row := cinfo^.image_height;

  { And read the first row }
  preload_image := source^.pub.get_pixel_rows (cinfo, sinfo);
end;


{ Read the file header; return image size and component count. }

{METHODDEF}
procedure start_input_bmp (cinfo : j_compress_ptr;
                           sinfo : cjpeg_source_ptr); far;
var
  source : bmp_source_ptr;

  bmpfileheader : packed array[0..14-1] of U_CHAR;
  bmpinfoheader : packed array[0..64-1] of U_CHAR;


  bfOffBits : INT32 ;
  headerSize : INT32;
  biWidth : INT32;              { initialize to avoid compiler warning }
  biHeight : INT32;
  biPlanes : uInt;
  biCompression : INT32;
  biXPelsPerMeter,biYPelsPerMeter : INT32;
  biClrUsed : INT32;
  mapentrysize : int;
  bPad : INT32;
  row_width : JDIMENSION;
var
  progress : cd_progress_ptr;
begin
  source := bmp_source_ptr (sinfo);
  biWidth := 0;                 { initialize to avoid compiler warning }
  biHeight := 0;
  biClrUsed := 0;
  mapentrysize := 0;            { 0 indicates no colormap }

  { Read and verify the bitmap file header }
  if JFREAD(source^.pub.input_file, @bmpfileheader, 14) <> size_t (14) then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);

  { GET_2B(bmpfileheader, 0) }
  if (uInt(UCH(bmpfileheader[0]) +
     (uInt(UCH(bmpfileheader[0+1])) shl 8)) <> $4D42) then { 'BM' }
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_NOT);

  bfOffBits := {INT32 ( GET_4B(bmpfileheader,10) );}
               INT32( INT32(UCH(bmpfileheader[10])) +
                    ((INT32(UCH(bmpfileheader[10+1])) shl 8)) +
                    ((INT32(UCH(bmpfileheader[10+2])) shl 16)) +
                    ((INT32(UCH(bmpfileheader[10+3])) shl 24)));

  { We ignore the remaining fileheader fields }

  { The infoheader might be 12 bytes (OS/2 1.x), 40 bytes (Windows),
    or 64 bytes (OS/2 2.x).  Check the first 4 bytes to find out which. }

  if JFREAD(source^.pub.input_file, @bmpinfoheader, 4) <> size_t(4) then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
  headerSize := {INT32 (GET_4B(bmpinfoheader,0));}
                INT32( INT32(UCH(bmpinfoheader[0])) +
                     ((INT32(UCH(bmpinfoheader[0+1])) shl 8)) +
                     ((INT32(UCH(bmpinfoheader[0+2])) shl 16)) +
                     ((INT32(UCH(bmpinfoheader[0+3])) shl 24)));

  if (headerSize < 12) or (headerSize > 64) then
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADHEADER);

  if JFREAD(source^.pub.input_file,@bmpinfoheader[4],headerSize-4) <>
     size_t (headerSize-4) then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);

  case int(headerSize) of
  12:begin
      { Decode OS/2 1.x header (Microsoft calls this a BITMAPCOREHEADER) }

      biWidth := {INT32 (GET_2B(bmpinfoheader,4));}
                 INT32( uInt(UCH(bmpinfoheader[4])) +
                       (uInt(UCH(bmpinfoheader[4+1])) shl 8) );

      biHeight := {INT32 (GET_2B(bmpinfoheader,6));}
                  INT32( uInt(UCH(bmpinfoheader[6])) +
                        (uInt(UCH(bmpinfoheader[6+1])) shl 8) );

      biPlanes := {GET_2B(bmpinfoheader,8);}
                  uInt(UCH(bmpinfoheader[8])) +
                  (uInt(UCH(bmpinfoheader[8+1])) shl 8);

      source^.bits_per_pixel := {int (GET_2B(bmpinfoheader,10));}
                                int( uInt(UCH(bmpinfoheader[10])) +
                                    (uInt(UCH(bmpinfoheader[10+1])) shl 8));

      case (source^.bits_per_pixel) of
      8: begin                    { colormapped image }
           mapentrysize := 3;         { OS/2 uses RGBTRIPLE colormap }
           TRACEMS2(j_common_ptr(cinfo), 1, JTRC_BMP_OS2_MAPPED, int (biWidth), int(biHeight));
         end;
      24:                       { RGB image }
        TRACEMS2(j_common_ptr(cinfo), 1, JTRC_BMP_OS2, int (biWidth), int(biHeight));
      else
        ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADDEPTH);
      end;
      if (biPlanes <> 1) then
        ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADPLANES);
    end;
  40,
  64:begin
      { Decode Windows 3.x header (Microsoft calls this a BITMAPINFOHEADER) }
      { or OS/2 2.x header, which has additional fields that we ignore }

      biWidth := {GET_4B(bmpinfoheader,4);}
                 ( INT32(UCH(bmpinfoheader[4])) +
                 ((INT32(UCH(bmpinfoheader[4+1])) shl 8)) +
                 ((INT32(UCH(bmpinfoheader[4+2])) shl 16)) +
                 ((INT32(UCH(bmpinfoheader[4+3])) shl 24)));
      biHeight := {GET_4B(bmpinfoheader,8);}
                  ( INT32(UCH(bmpinfoheader[8])) +
                  ((INT32(UCH(bmpinfoheader[8+1])) shl 8)) +
                  ((INT32(UCH(bmpinfoheader[8+2])) shl 16)) +
                  ((INT32(UCH(bmpinfoheader[8+3])) shl 24)));

      biPlanes := {GET_2B(bmpinfoheader,12);}
                  ( uInt(UCH(bmpinfoheader[12])) +
                   (uInt(UCH(bmpinfoheader[12+1])) shl 8) );

      source^.bits_per_pixel := {int (GET_2B(bmpinfoheader,14));}
                                int( uInt(UCH(bmpinfoheader[14])) +
                                   ( uInt(UCH(bmpinfoheader[14+1])) shl 8) );

      biCompression := {GET_4B(bmpinfoheader,16);}
                       ( INT32(UCH(bmpinfoheader[16])) +
                       ((INT32(UCH(bmpinfoheader[16+1])) shl 8)) +
                       ((INT32(UCH(bmpinfoheader[16+2])) shl 16)) +
                       ((INT32(UCH(bmpinfoheader[16+3])) shl 24)));

      biXPelsPerMeter := {GET_4B(bmpinfoheader,24);}
                         ( INT32(UCH(bmpinfoheader[24])) +
                         ((INT32(UCH(bmpinfoheader[24+1])) shl 8)) +
                         ((INT32(UCH(bmpinfoheader[24+2])) shl 16)) +
                         ((INT32(UCH(bmpinfoheader[24+3])) shl 24)));

      biYPelsPerMeter := {GET_4B(bmpinfoheader,28);}
                         ( INT32(UCH(bmpinfoheader[28])) +
                         ((INT32(UCH(bmpinfoheader[28+1])) shl 8)) +
                         ((INT32(UCH(bmpinfoheader[28+2])) shl 16)) +
                         ((INT32(UCH(bmpinfoheader[28+3])) shl 24)));

      biClrUsed := {GET_4B(bmpinfoheader,32);}
                   ( INT32(UCH(bmpinfoheader[32])) +
                   ((INT32(UCH(bmpinfoheader[32+1])) shl 8)) +
                   ((INT32(UCH(bmpinfoheader[32+2])) shl 16)) +
                   ((INT32(UCH(bmpinfoheader[32+3])) shl 24)));

      { biSizeImage, biClrImportant fields are ignored }

      case (source^.bits_per_pixel) of
      8: begin                     { colormapped image }
           mapentrysize := 4;           { Windows uses RGBQUAD colormap }
           TRACEMS2(j_common_ptr(cinfo), 1, JTRC_BMP_MAPPED, int (biWidth), int (biHeight));
         end;
      24:                          { RGB image }
         TRACEMS2(j_common_ptr(cinfo), 1, JTRC_BMP, int (biWidth), int (biHeight));
      else
        ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADDEPTH);
      end;
      if (biPlanes <> 1) then
        ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADPLANES);
      if (biCompression <> 0) then
        ERREXIT(j_common_ptr(cinfo), JERR_BMP_COMPRESSED);

      if (biXPelsPerMeter > 0) and (biYPelsPerMeter > 0) then
      begin
        { Set JFIF density parameters from the BMP data }
        cinfo^.X_density := UINT16 (biXPelsPerMeter div 100); { 100 cm per meter }
        cinfo^.Y_density := UINT16 (biYPelsPerMeter div 100);
        cinfo^.density_unit := 2;       { dots/cm }
      end;
    end;
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADHEADER);
  end;

  { Compute distance to bitmap data --- will adjust for colormap below }
  bPad := bfOffBits - (headerSize + 14);

  { Read the colormap, if any }
  if (mapentrysize > 0) then
  begin
    if (biClrUsed <= 0) then
      biClrUsed := 256       { assume it's 256 }
    else
      if (biClrUsed > 256) then
        ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADCMAP);
    { Allocate space to store the colormap }
    source^.colormap := cinfo^.mem^.alloc_sarray(
       j_common_ptr (cinfo), JPOOL_IMAGE,
       JDIMENSION (biClrUsed), JDIMENSION (3));
    { and read it from the file }
    read_colormap(source, int (biClrUsed), mapentrysize);
    { account for size of colormap }
    Dec(bPad, biClrUsed * mapentrysize);
  end;

  { Skip any remaining pad bytes }
  if (bPad < 0) then       { incorrect bfOffBits value? }
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADHEADER);

  while (bPad > 0) do
  begin
    Dec(bPad);
    {void} read_byte(source);
  end;

  { Compute row width in file, including padding to 4-byte boundary }
  if (source^.bits_per_pixel = 24) then
    row_width := JDIMENSION (biWidth * 3)
  else
    row_width := JDIMENSION (biWidth);
  while ((row_width and 3) <> 0) do
    Inc(row_width);
  source^.row_width := row_width;

  { Allocate space for inversion array, prepare for preload pass }
  source^.whole_image := cinfo^.mem^.request_virt_sarray(
     j_common_ptr (cinfo), JPOOL_IMAGE, FALSE,
     row_width, JDIMENSION (biHeight), JDIMENSION (1));
  source^.pub.get_pixel_rows := preload_image;
  if (cinfo^.progress <> NIL) then
  begin
    progress := cd_progress_ptr (cinfo^.progress);
    Inc(progress^.total_extra_passes); { count file input as separate pass }
  end;

  { Allocate one-row buffer for returned data }
  source^.pub.buffer := cinfo^.mem^.alloc_sarray(
     j_common_ptr (cinfo), JPOOL_IMAGE,
     JDIMENSION (biWidth * 3), JDIMENSION (1) );
  source^.pub.buffer_height := 1;

  cinfo^.in_color_space := JCS_RGB;
  cinfo^.input_components := 3;
  cinfo^.data_precision := 8;
  cinfo^.image_width := JDIMENSION (biWidth);
  cinfo^.image_height := JDIMENSION (biHeight);
end;


{ Finish up at the end of the file. }

{METHODDEF}
procedure finish_input_bmp (cinfo : j_compress_ptr;
                            sinfo : cjpeg_source_ptr); far;
begin
  { no work }
end;


{ The module selection routine for BMP format input. }

{GLOBAL}
function jinit_read_bmp (cinfo : j_compress_ptr) : cjpeg_source_ptr;
var
  source : bmp_source_ptr;
begin
  { Create module interface object }
  source := bmp_source_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                               SIZEOF(bmp_source_struct)) );
  source^.cinfo := cinfo;       { make back link for subroutines }
  { Fill in method ptrs, except get_pixel_rows which start_input sets }
  source^.pub.start_input := start_input_bmp;
  source^.pub.finish_input := finish_input_bmp;

  jinit_read_bmp  := cjpeg_source_ptr (source);
end;

end.
