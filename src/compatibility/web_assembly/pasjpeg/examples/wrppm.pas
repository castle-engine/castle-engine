Unit WrPPM;

{ wrppm.c

  Copyright (C) 1991-1996, Thomas G. Lane.
  This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains routines to write output images in PPM/PGM format.
  The extended 2-byte-per-sample raw PPM/PGM formats are supported.
  The PBMPLUS library is NOT required to compile this software
  (but it is highly useful as a set of PPM image manipulation programs).

  These routines may need modification for non-Unix environments or
  specialized applications.  As they stand, they assume output to
  an ordinary stdio stream. }

interface

{$I jconfig.inc}

uses
  jdeferr,
  jmorecfg,
  jerror,
  jpeglib,
  jinclude,
  jdmaster,
  cdjpeg;               { Common decls for cjpeg/djpeg applications }

{GLOBAL}
function jinit_write_ppm (cinfo : j_decompress_ptr) : djpeg_dest_ptr;

implementation

{ For 12-bit JPEG data, we either downscale the values to 8 bits
  (to write standard byte-per-sample PPM/PGM files), or output
  nonstandard word-per-sample PPM/PGM files.  Downscaling is done
  if PPM_NORAWWORD is defined (this can be done in the Makefile
  or in jconfig.h).
  (When the core library supports data precision reduction, a cleaner
  implementation will be to ask for that instead.) }

type
  CharPtr = ^char;


{$ifdef BITS_IN_JSAMPLE_IS_8}

procedure PUTPPMSAMPLE(var ptr : CharPtr; v : byte);
begin
  ptr^ := char(v);
  Inc(ptr);
end;

const
  BYTESPERSAMPLE = 1;
  PPM_MAXVAL = 255;
{$else}
  {$ifdef PPM_NORAWWORD}

procedure PUTPPMSAMPLE(var ptr : CharPtr; v : byte);
begin
  ptr^ := char (v shr (BITS_IN_JSAMPLE-8));
  Inc(ptr);
end;

const
  BYTESPERSAMPLE = 1;
  PPM_MAXVAL = 255;

  {$else}
  { The word-per-sample format always puts the LSB first. }

procedure PUTPPMSAMPLE(var ptr : CharPtr; v : int);
var
  {register} val_ : int;
begin
  val_ := v;
  ptr^ := char (val_ and $FF);
  Inc(ptr);
  ptr^ := char ((val_ shr 8) and $FF);
  Inc(ptr);
end;
const
  BYTESPERSAMPLE = 2;
  PPM_MAXVAL = (1 shl BITS_IN_JSAMPLE)-1;
  {$endif}
{$endif}


{ When JSAMPLE is the same size as char, we can just fwrite() the
  decompressed data to the PPM or PGM file.  On PCs, in order to make this
  work the output buffer must be allocated in near data space, because we are
  assuming small-data memory model wherein fwrite() can't reach far memory.
  If you need to process very wide images on a PC, you might have to compile
  in large-memory model, or else replace fwrite() with a putc() loop ---
  which will be much slower. }


{ Private version of data destination object }

type
  ppm_dest_ptr = ^ppm_dest_struct;
  ppm_dest_struct = record
    pub : djpeg_dest_struct;    { public fields }

    { Usually these two pointers point to the same place: }
    iobuffer : CharPtr;          { fwrite's I/O buffer }
    pixrow : JSAMPROW;           { decompressor output buffer }
    buffer_width : size_t;       { width of I/O buffer }
    samples_per_row : JDIMENSION; { JSAMPLEs per output row }
  end;


{ Write some pixel data.
  In this module rows_supplied will always be 1.

  put_pixel_rows handles the "normal" 8-bit case where the decompressor
  output buffer is physically the same as the fwrite buffer. }

{METHODDEF}
procedure put_pixel_rows (cinfo : j_decompress_ptr;
                          dinfo : djpeg_dest_ptr;
                          rows_supplied : JDIMENSION); far;
var
  dest : ppm_dest_ptr;
begin
  dest := ppm_dest_ptr(dinfo);
  {void} JFWRITE(dest^.pub.output_file, dest^.iobuffer, dest^.buffer_width);
end;


{ This code is used when we have to copy the data and apply a pixel
  format translation.  Typically this only happens in 12-bit mode. }

{METHODDEF}
procedure copy_pixel_rows (cinfo : j_decompress_ptr;
                           dinfo : djpeg_dest_ptr;
                           rows_supplied : JDIMENSION); far;
var
  dest : ppm_dest_ptr;
  {register} bufferptr : CharPtr;
  {register} ptr : JSAMPLE_PTR;
  {register} col : JDIMENSION;
begin
  dest := ppm_dest_ptr(dinfo);
  ptr := JSAMPLE_PTR(dest^.pub.buffer^[0]);
  bufferptr := dest^.iobuffer;
  for col := pred(dest^.samples_per_row) downto 0 do
  begin
    PUTPPMSAMPLE(bufferptr, GETJSAMPLE(ptr^));
    Inc(ptr);
  end;
  {void} JFWRITE(dest^.pub.output_file, dest^.iobuffer, dest^.buffer_width);
end;


{ Write some pixel data when color quantization is in effect.
  We have to demap the color index values to straight data. }

{METHODDEF}
procedure put_demapped_rgb (cinfo : j_decompress_ptr;
                            dinfo : djpeg_dest_ptr;
                            rows_supplied : JDIMENSION); far;
var
  dest : ppm_dest_ptr;
  {register} bufferptr : CharPtr;
  {register} ptr : JSAMPLE_PTR;
  {register} col : JDIMENSION;

  {register} pixval : int;
  {register} color_map0 : JSAMPROW;
  {register} color_map1 : JSAMPROW;
  {register} color_map2 : JSAMPROW;
begin
  dest := ppm_dest_ptr(dinfo);
  ptr := JSAMPLE_PTR(dest^.pub.buffer^[0]);
  bufferptr := dest^.iobuffer;
  color_map0 := cinfo^.colormap^[0];
  color_map1 := cinfo^.colormap^[1];
  color_map2 := cinfo^.colormap^[2];

  for col := pred(cinfo^.output_width) downto 0 do
  begin
    pixval := GETJSAMPLE(ptr^);
    Inc(ptr);
    PUTPPMSAMPLE(bufferptr, GETJSAMPLE(color_map0^[pixval]));
    PUTPPMSAMPLE(bufferptr, GETJSAMPLE(color_map1^[pixval]));
    PUTPPMSAMPLE(bufferptr, GETJSAMPLE(color_map2^[pixval]));
  end;
  {void} JFWRITE(dest^.pub.output_file, dest^.iobuffer, dest^.buffer_width);
end;


{METHODDEF}
procedure put_demapped_gray (cinfo : j_decompress_ptr;
                             dinfo : djpeg_dest_ptr;
                             rows_supplied : JDIMENSION); far;
var
  dest : ppm_dest_ptr;
  {register} bufferptr : CharPtr;
  {register} ptr : JSAMPLE_PTR;
  {register} color_map : JSAMPROW;
  {register} col : JDIMENSION;
begin
  dest := ppm_dest_ptr(dinfo);
  color_map := cinfo^.colormap^[0];
  ptr := JSAMPLE_PTR(dest^.pub.buffer^[0]);
  bufferptr := dest^.iobuffer;
  for col := pred(cinfo^.output_width) downto 0 do
  begin
    PUTPPMSAMPLE(bufferptr, GETJSAMPLE(color_map^[GETJSAMPLE(ptr^)]));
    Inc(ptr);
  end;
  {void} JFWRITE(dest^.pub.output_file, dest^.iobuffer, dest^.buffer_width);
end;


{ Startup: write the file header. }

{METHODDEF}
procedure start_output_ppm (cinfo : j_decompress_ptr;
                            dinfo : djpeg_dest_ptr); far;
const
  LF = #10;
var
  dest : ppm_dest_ptr;
var
  header : string[200];

  function LongToStr(l : long) : string;
  var
    helpstr : string[20];
  begin
    Str(l, helpstr);
    LongToStr := helpstr;
  end;

begin
  dest := ppm_dest_ptr(dinfo);
  { Emit file header }
  case (cinfo^.out_color_space) of
  JCS_GRAYSCALE:
    begin
      { emit header for raw PGM format }
      header := 'P5'+LF+LongToStr(cinfo^.output_width)+' '+
                LongToStr(cinfo^.output_height)+LF+
                LongToStr(Long(PPM_MAXVAL)) + LF;
      JFWRITE(dest^.pub.output_file, @header[1], Length(header));
    end;
  JCS_RGB:
    begin
      { emit header for raw PPM format }
      header := 'P6'+LF+LongToStr(cinfo^.output_width)+' '+
                LongToStr(cinfo^.output_height)+LF+
                LongToStr(Long(PPM_MAXVAL)) + LF;
      JFWRITE(dest^.pub.output_file, @header[1], Length(header));
    end;
  else
    ERREXIT(j_common_ptr(cinfo), JERR_PPM_COLORSPACE);
  end;
end;


{ Finish up at the end of the file. }

{METHODDEF}
procedure finish_output_ppm (cinfo : j_decompress_ptr;
                             dinfo : djpeg_dest_ptr); far;
begin
  { Make sure we wrote the output file OK }
  {Flush(dinfo^.output_file^);}
  if (IOresult <> 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
end;

{ The module selection routine for PPM format output. }

{GLOBAL}
function jinit_write_ppm (cinfo : j_decompress_ptr) : djpeg_dest_ptr;
var
  dest : ppm_dest_ptr;
begin
  { Create module interface object, fill in method pointers }
  dest := ppm_dest_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  SIZEOF(ppm_dest_struct)) );
  dest^.pub.start_output := start_output_ppm;
  dest^.pub.finish_output := finish_output_ppm;

  { Calculate output image dimensions so we can allocate space }
  jpeg_calc_output_dimensions(cinfo);

  { Create physical I/O buffer.  Note we make this near on a PC. }
  dest^.samples_per_row := cinfo^.output_width * cinfo^.out_color_components;
  dest^.buffer_width := dest^.samples_per_row * (BYTESPERSAMPLE * SIZEOF(char));
  dest^.iobuffer := CharPtr( cinfo^.mem^.alloc_small
    (j_common_ptr(cinfo), JPOOL_IMAGE, dest^.buffer_width) );

  if (cinfo^.quantize_colors) or (BITS_IN_JSAMPLE <> 8) or
      (SIZEOF(JSAMPLE) <> SIZEOF(char)) then
  begin
    { When quantizing, we need an output buffer for colormap indexes
      that's separate from the physical I/O buffer.  We also need a
      separate buffer if pixel format translation must take place. }

    dest^.pub.buffer := cinfo^.mem^.alloc_sarray
      (j_common_ptr(cinfo), JPOOL_IMAGE,
       cinfo^.output_width * cinfo^.output_components, JDIMENSION(1));
    dest^.pub.buffer_height := 1;
    if (not cinfo^.quantize_colors) then
      dest^.pub.put_pixel_rows := copy_pixel_rows
    else
      if (cinfo^.out_color_space = JCS_GRAYSCALE) then
        dest^.pub.put_pixel_rows := put_demapped_gray
      else
        dest^.pub.put_pixel_rows := put_demapped_rgb;
  end
  else
  begin
    { We will fwrite() directly from decompressor output buffer. }
    { Synthesize a JSAMPARRAY pointer structure }
    { Cast here implies near^.far pointer conversion on PCs }
    dest^.pixrow := JSAMPROW(dest^.iobuffer);
    dest^.pub.buffer := JSAMPARRAY (@dest^.pixrow);
    dest^.pub.buffer_height := 1;
    dest^.pub.put_pixel_rows := put_pixel_rows;
  end;

  jinit_write_ppm := djpeg_dest_ptr(dest);
end;


end.
