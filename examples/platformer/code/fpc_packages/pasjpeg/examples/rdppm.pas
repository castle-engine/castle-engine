Unit rdppm;

{ rdppm.c

  Copyright (C) 1991-1997, Thomas G. Lane.
  This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains routines to read input images in PPM/PGM format.
  The extended 2-byte-per-sample raw PPM/PGM formats are supported.
  The PBMPLUS library is NOT required to compile this software
  (but it is highly useful as a set of PPM image manipulation programs).

  These routines may need modification for non-Unix environments or
  specialized applications.  As they stand, they assume input from
  an ordinary stdio stream.  They further assume that reading begins
  at the start of the file; start_input may need work if the
  user interface has already read some data (e.g., to determine that
  the file is indeed PPM format).
 }

interface

{$define CHAR_IS_UNSIGNED}

{$I jconfig.inc}

uses
  jdeferr,
  jmorecfg,
  jerror,
  jpeglib,
  jinclude,
  cdjpeg;               { Common decls for cjpeg/djpeg applications }

{GLOBAL}
function jinit_read_ppm (cinfo : j_compress_ptr) : cjpeg_source_ptr;


implementation

{ Portions of this code are based on the PBMPLUS library, which is:
*
* Copyright (C) 1988 by Jef Poskanzer.
*
* Permission to use, copy, modify, and distribute this software and its
* documentation for any purpose and without fee is hereby granted, provided
* that the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation.  This software is provided "as is" without express or
* implied warranty.
}


{ Macros to deal with unsigned chars as efficiently as compiler allows }

{$ifdef HAVE_UNSIGNED_CHAR}
type
  U_CHAR = unsigned char;
  UCH = int;
{$else} { !HAVE_UNSIGNED_CHAR }
  {$ifdef CHAR_IS_UNSIGNED}
  type
    U_CHAR = byte;
    U_CHARptr = ^U_CHAR;
    UCH = int;
  {$else}
  type
    U_CHAR = char;
    UCH(x) = int (x and $FF)
  {$endif}
{$endif} { HAVE_UNSIGNED_CHAR }


{ macro }
function ReadOK(f : FILEptr; buffer : pointer; len : size_t) : boolean;
begin
  ReadOK := JFREAD(f, buffer,len) = size_t(len);
end;
{
  On most systems, reading individual bytes with getc() is drastically less
  efficient than buffering a row at a time with fread().  On PCs, we must
  allocate the buffer in near data space, because we are assuming small-data
  memory model, wherein fread() can't reach far memory.  If you need to
  process very wide images on a PC, you might have to compile in large-memory
  model, or else replace fread() with a getc() loop --- which will be much
  slower.
 }


{ Private version of data source object }

type
  ppm_source_ptr = ^ppm_source_struct;
  ppm_source_struct = record
    pub : cjpeg_source_struct; { public fields }

    iobuffer : U_CHARptr;               { non-FAR pointer to I/O buffer }
    pixrow : JSAMPROW;                  { FAR pointer to same }
    buffer_width : size_t;              { width of I/O buffer }
    rescale : JSAMPROW;                 { => maxval-remapping array, or NIL }
  end;
const
  LF = #10;
  CR = #13;

{LOCAL}
function pbm_getc (var infile : file) : char;
{ Read next char, skipping over any comments }
{ A comment/newline sequence is returned as a newline }
var
  {register} ch : char;
begin
  {getch} BlockRead(infile, ch, 1);
  if (ch = '#') then
  begin
    repeat
      BlockRead(infile, ch, 1);
    until (ch = LF) or eof(infile);
  end;
  pbm_getc := ch;
end;


{LOCAL}

function read_pbm_integer (cinfo : j_compress_ptr; var infile : file) : uint;
{ Read an unsigned decimal integer from the PPM file }
{ Swallows one trailing character after the integer }
{ Note that on a 16-bit-int machine, only values up to 64k can be read. }
{ This should not be a problem in practice. }
const
  TAB = ^I;
var
  {register} ch : char;
  {register} val : uint;
begin
  { Skip any leading whitespace }
  repeat
    ch := pbm_getc(infile);
    if eof(infile) then
      ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
  until (ch <> ' ') and (ch <> TAB) and (ch <> LF) and (ch <> CR);

  if (ch < '0') or (ch > '9') then
    ERREXIT(j_common_ptr(cinfo), JERR_PPM_NONNUMERIC);

  val := ord(ch) - ord('0');
  repeat
    ch := pbm_getc(infile);
    if (ch >= '0') and (ch <= '9') then
    begin
      val := val * 10;
      Inc(val, ord(ch) - ord('0'));
    end
    else
      break;
  until FALSE;
  read_pbm_integer := val;
end;

{ Read one row of pixels.

  We provide several different versions depending on input file format.
  In all cases, input is scaled to the size of JSAMPLE.

  A really fast path is provided for reading byte/sample raw files with
  maxval := MAXJSAMPLE, which is the normal case for 8-bit data. }


{METHODDEF}
function get_text_gray_row (cinfo : j_compress_ptr;
                            sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading text-format PGM files with any maxval }
var
  source : ppm_source_ptr;
  infile : FILEptr;
  {register} ptr : JSAMPLE_PTR;
  {register} rescale : JSAMPROW;
  col : JDIMENSION;
begin
  source := ppm_source_ptr(sinfo);
  infile := source^.pub.input_file;
  rescale := source^.rescale;
  ptr := JSAMPLE_PTR(source^.pub.buffer^[0]);
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    ptr^ := rescale^[read_pbm_integer(cinfo, infile^)];
    Inc(ptr);
  end;
  get_text_gray_row := 1;
end;


{METHODDEF}
function get_text_rgb_row (cinfo : j_compress_ptr;
                           sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading text-format PPM files with any maxval }
var
  source : ppm_source_ptr;
  infile : FILEptr;
  {register} ptr : JSAMPLE_PTR;
  {register} rescale : JSAMPROW;
  col : JDIMENSION;
begin
  source := ppm_source_ptr(sinfo);
  infile := source^.pub.input_file;
  rescale := source^.rescale;
  ptr := JSAMPLE_PTR(source^.pub.buffer^[0]);
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    ptr^ := rescale^[read_pbm_integer(cinfo, infile^)];
    Inc(ptr);
    ptr^ := rescale^[read_pbm_integer(cinfo, infile^)];
    Inc(ptr);
    ptr^ := rescale^[read_pbm_integer(cinfo, infile^)];
    Inc(ptr);
  end;
  get_text_rgb_row := 1;
end;


{METHODDEF}
function get_scaled_gray_row (cinfo : j_compress_ptr;
                              sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading raw-byte-format PGM files with any maxval }
var
  source : ppm_source_ptr;
  {register} ptr : JSAMPLE_PTR;
  {register} bufferptr : U_CHARptr ;
  {register} rescale : JSAMPROW;
  col : JDIMENSION;
begin
  source := ppm_source_ptr(sinfo);
  rescale := source^.rescale;
  if not ReadOK(source^.pub.input_file, source^.iobuffer,
                source^.buffer_width) then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
  ptr := JSAMPLE_PTR(source^.pub.buffer^[0]);
  bufferptr := source^.iobuffer;
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    ptr^ := rescale^[UCH(bufferptr^)];
    Inc(ptr);
    Inc(bufferptr);
  end;
  get_scaled_gray_row := 1;
end;


{METHODDEF}
function get_scaled_rgb_row (cinfo : j_compress_ptr;
                             sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading raw-byte-format PPM files with any maxval }
var
  source : ppm_source_ptr;
  {register} ptr : JSAMPLE_PTR;
  {register} bufferptr : U_CHARptr ;
  {register} rescale : JSAMPROW;
  col : JDIMENSION;
begin
  source := ppm_source_ptr (sinfo);
  rescale := source^.rescale;

  if not ReadOK(source^.pub.input_file, source^.iobuffer,
                source^.buffer_width) then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
  ptr := JSAMPLE_PTR(source^.pub.buffer^[0]);
  bufferptr := source^.iobuffer;
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    ptr^ := rescale^[UCH(bufferptr^)];
    Inc(ptr);
    Inc(bufferptr);
    ptr^ := rescale^[UCH(bufferptr^)];
    Inc(ptr);
    Inc(bufferptr);
    ptr^ := rescale^[UCH(bufferptr^)];
    Inc(ptr);
    Inc(bufferptr);
  end;
  get_scaled_rgb_row := 1;
end;


{METHODDEF}
function get_raw_row (cinfo : j_compress_ptr;
                      sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading raw-byte-format files with maxval := MAXJSAMPLE.
  In this case we just read right into the JSAMPLE buffer!
  Note that same code works for PPM and PGM files. }
var
  source : ppm_source_ptr;
begin
  source := ppm_source_ptr(sinfo);

  if not ReadOK(source^.pub.input_file, source^.iobuffer,
                source^.buffer_width) then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
  get_raw_row := 1;
end;


{METHODDEF}
function get_word_gray_row (cinfo : j_compress_ptr;
                            sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading raw-word-format PGM files with any maxval }
var
  source : ppm_source_ptr;
  {register} ptr : JSAMPLE_PTR;
  {register} bufferptr : U_CHARptr;
  {register} rescale : JSAMPROW;
  col : JDIMENSION;
var
  {register} temp : int;
begin
  source := ppm_source_ptr (sinfo);
  rescale := source^.rescale;
  if not ReadOK(source^.pub.input_file, source^.iobuffer,
                source^.buffer_width) then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
  ptr := JSAMPLE_PTR(source^.pub.buffer^[0]);
  bufferptr := source^.iobuffer;
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    temp  := UCH(bufferptr^);
    Inc(bufferptr);
    temp := temp or (UCH(bufferptr^) shl 8);
    Inc(bufferptr);
    ptr^ := rescale^[temp];
    Inc(ptr);
  end;
  get_word_gray_row := 1;
end;


{METHODDEF}
function get_word_rgb_row (cinfo : j_compress_ptr;
                           sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading raw-word-format PPM files with any maxval }
var
  source : ppm_source_ptr;
  {register} ptr : JSAMPLE_PTR;
  {register} bufferptr : U_CHARptr;
  {register} rescale : JSAMPROW;
  col : JDIMENSION;
var
  {register} temp : int;
begin
  source := ppm_source_ptr(sinfo);
  rescale := source^.rescale;
  if not ReadOK(source^.pub.input_file, source^.iobuffer,
                source^.buffer_width) then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
  ptr := JSAMPLE_PTR(source^.pub.buffer^[0]);
  bufferptr := source^.iobuffer;
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    temp  := UCH(bufferptr^);
    Inc(bufferptr);
    temp := temp or (UCH(bufferptr^) shl 8);
    Inc(bufferptr);
    ptr^ := rescale^[temp];
    Inc(ptr);
    temp  := UCH(bufferptr^);
    Inc(bufferptr);
    temp := temp or (UCH(bufferptr^) shl 8);
    Inc(bufferptr);
    ptr^ := rescale^[temp];
    Inc(ptr);
    temp  := UCH(bufferptr^);
    Inc(bufferptr);
    temp := temp or (UCH(bufferptr^) shl 8);
    Inc(bufferptr);
    ptr^ := rescale^[temp];
    Inc(ptr);
  end;
  get_word_rgb_row := 1;
end;


{ Read the file header; return image size and component count. }

{METHODDEF}
procedure start_input_ppm (cinfo : j_compress_ptr;
                           sinfo : cjpeg_source_ptr); far;
var
  source : ppm_source_ptr;
  c : char;
  w, h, maxval : uint;
  need_iobuffer, use_raw_buffer, need_rescale : boolean;
var
  val, half_maxval : INT32;
begin
  source := ppm_source_ptr(sinfo);
  {getch} BlockRead(source^.pub.input_file^, c, 1);
  if (c <> 'P') then
    ERREXIT(j_common_ptr(cinfo), JERR_PPM_NOT);

  {getch} BlockRead(source^.pub.input_file^, c, 1);
  { subformat discriminator character }

  { detect unsupported variants (ie, PBM) before trying to read header }
  case (c) of
  '2',                  { it's a text-format PGM file }
  '3',                  { it's a text-format PPM file }
  '5',                  { it's a raw-format PGM file }
  '6':;                 { it's a raw-format PPM file }
  else
    ERREXIT(j_common_ptr(cinfo), JERR_PPM_NOT);
  end;

  { fetch the remaining header info }
  w := read_pbm_integer(cinfo, source^.pub.input_file^);
  h := read_pbm_integer(cinfo, source^.pub.input_file^);
  maxval := read_pbm_integer(cinfo, source^.pub.input_file^);

  if (w <= 0) or (h <= 0) or (maxval <= 0) then { error check }
    ERREXIT(j_common_ptr(cinfo), JERR_PPM_NOT);

  cinfo^.data_precision := BITS_IN_JSAMPLE; { we always rescale data to this }
  cinfo^.image_width := JDIMENSION (w);
  cinfo^.image_height := JDIMENSION (h);

  { initialize flags to most common settings }
  need_iobuffer := TRUE;                { do we need an I/O buffer? }
  use_raw_buffer := FALSE;      { do we map input buffer onto I/O buffer? }
  need_rescale := TRUE;         { do we need a rescale array? }

  case (c) of
  '2':                  { it's a text-format PGM file }
    begin
      cinfo^.input_components := 1;
      cinfo^.in_color_space := JCS_GRAYSCALE;
      {$IFDEF DEBUG}
      TRACEMS2(j_common_ptr(cinfo), 1, JTRC_PGM_TEXT, w, h);
      {$ENDIF}
      source^.pub.get_pixel_rows := get_text_gray_row;
      need_iobuffer := FALSE;
    end;

  '3':                  { it's a text-format PPM file }
    begin
      cinfo^.input_components := 3;
      cinfo^.in_color_space := JCS_RGB;
      {$IFDEF DEBUG}
      TRACEMS2(j_common_ptr(cinfo), 1, JTRC_PPM_TEXT, w, h);
      {$ENDIF}
      source^.pub.get_pixel_rows := get_text_rgb_row;
      need_iobuffer := FALSE;
    end;

  '5':                  { it's a raw-format PGM file }
    begin
      cinfo^.input_components := 1;
      cinfo^.in_color_space := JCS_GRAYSCALE;
      TRACEMS2(j_common_ptr(cinfo), 1, JTRC_PGM, w, h);
      if (maxval > 255) then
      begin
        source^.pub.get_pixel_rows := get_word_gray_row;
      end
      else
        if (maxval = MAXJSAMPLE) and (SIZEOF(JSAMPLE) = SIZEOF(U_CHAR)) then
        begin
          source^.pub.get_pixel_rows := get_raw_row;
          use_raw_buffer := TRUE;
          need_rescale := FALSE;
        end
        else
        begin
          source^.pub.get_pixel_rows := get_scaled_gray_row;
        end;
    end;

  '6':                  { it's a raw-format PPM file }
    begin
      cinfo^.input_components := 3;
      cinfo^.in_color_space := JCS_RGB;
      {$IFDEF DEBUG}
      TRACEMS2(j_common_ptr(cinfo), 1, JTRC_PPM, w, h);
      {$ENDIF}
      if (maxval > 255) then
      begin
        source^.pub.get_pixel_rows := get_word_rgb_row;
      end
      else
        if (maxval = MAXJSAMPLE) and (SIZEOF(JSAMPLE) = SIZEOF(U_CHAR)) then
        begin
          source^.pub.get_pixel_rows := get_raw_row;
          use_raw_buffer := TRUE;
          need_rescale := FALSE;
        end
        else
        begin
          source^.pub.get_pixel_rows := get_scaled_rgb_row;
        end;
    end;
  end;

  { Allocate space for I/O buffer: 1 or 3 bytes or words/pixel. }
  if (need_iobuffer) then
  begin
    if (maxval<=255) then
      source^.buffer_width := size_t ( w * cinfo^.input_components *
                                     SIZEOF(U_CHAR) )
    else
      source^.buffer_width := size_t ( w * cinfo^.input_components *
                                     (2*SIZEOF(U_CHAR)) );

    source^.iobuffer := U_CHARptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  source^.buffer_width) );
  end;

  { Create compressor input buffer. }
  if (use_raw_buffer) then
  begin
    { For unscaled raw-input case, we can just map it onto the I/O buffer. }
    { Synthesize a JSAMPARRAY pointer structure }
    { Cast here implies near^.far pointer conversion on PCs }
    source^.pixrow := JSAMPROW (source^.iobuffer);
    source^.pub.buffer := JSAMPARRAY(@source^.pixrow);
    source^.pub.buffer_height := 1;
  end
  else
  begin
    { Need to translate anyway, so make a separate sample buffer. }
    source^.pub.buffer := cinfo^.mem^.alloc_sarray
      (j_common_ptr(cinfo), JPOOL_IMAGE,
       JDIMENSION (w * cinfo^.input_components), JDIMENSION(1) );
    source^.pub.buffer_height := 1;
  end;

  { Compute the rescaling array if required. }
  if (need_rescale) then
  begin
    { On 16-bit-int machines we have to be careful of maxval := 65535 }
    source^.rescale := JSAMPROW (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                          size_t ((long(maxval) + long(1)) * SIZEOF(JSAMPLE))) );
    half_maxval := maxval div 2;
    for val := 0 to INT32(maxval) do
    begin
      { The multiplication here must be done in 32 bits to avoid overflow }
      source^.rescale^[val] := JSAMPLE ((val*MAXJSAMPLE + half_maxval) div maxval);
    end;
  end;
end;


{ Finish up at the end of the file. }

{METHODDEF}
procedure finish_input_ppm (cinfo : j_compress_ptr;
                            sinfo : cjpeg_source_ptr); far;
begin
  { no work }
end;


{ The module selection routine for PPM format input. }

{GLOBAL}
function jinit_read_ppm (cinfo : j_compress_ptr) : cjpeg_source_ptr;
var
  source : ppm_source_ptr;
begin
  { Create module interface object }
  source := ppm_source_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  SIZEOF(ppm_source_struct)) );
  { Fill in method ptrs, except get_pixel_rows which start_input sets }
  source^.pub.start_input := start_input_ppm;
  source^.pub.finish_input := finish_input_ppm;

  jinit_read_ppm  := cjpeg_source_ptr(source);
end;


end.
