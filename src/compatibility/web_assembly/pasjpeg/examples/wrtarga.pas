Unit wrtarga;

{ Copyright (C) 1991-1996, Thomas G. Lane.
  Based on code contributed by Lee Daniel Crocker.

  This file contains routines to write output images in Targa format. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jpeglib,
  jdeferr,
  jerror,
  jinclude,
  jdmaster,
  cdjpeg;               { Common decls for cjpeg/djpeg applications }

function jinit_write_targa (cinfo : j_decompress_ptr) : djpeg_dest_ptr;

implementation

{ To support 12-bit JPEG data, we'd have to scale output down to 8 bits.
  This is not yet implemented. }

{$ifndef BITS_IN_JSAMPLE_IS_8}
  Sorry, this code only copes with 8-bit JSAMPLEs. { deliberate syntax err }
{$endif}

{ The output buffer needs to be writable by fwrite().  On PCs, we must
  allocate the buffer in near data space, because we are assuming small-data
  memory model, wherein fwrite() can't reach far memory.  If you need to
  process very wide images on a PC, you might have to compile in large-memory
  model, or else replace fwrite() with a putc() loop --- which will be much
  slower. }


{ Private version of data destination object }

type
  tga_dest_ptr = ^tga_dest_struct;
  tga_dest_struct = record
    pub : djpeg_dest_struct;    { public fields }

    iobuffer : byteptr;         { physical I/O buffer }
    buffer_width : JDIMENSION;  { width of one row }
  end;

{LOCAL}
procedure write_header (cinfo : j_decompress_ptr;
                        dinfo : djpeg_dest_ptr;
                        num_colors : int);
{ Create and write a Targa header }
var
  targaheader : array[0..18-1] of byte;
begin
  { Set unused fields of header to 0 }
  MEMZERO(@targaheader, SIZEOF(targaheader));

  if (num_colors > 0) then
  begin
    targaheader[1] := 1;                { color map type 1 }
    targaheader[5] := byte (num_colors and $FF);
    targaheader[6] := byte (num_colors shr 8);
    targaheader[7] := 24;       { 24 bits per cmap entry }
  end;

  targaheader[12] := byte (cinfo^.output_width and $FF);
  targaheader[13] := byte (cinfo^.output_width shr 8);
  targaheader[14] := byte (cinfo^.output_height and $FF);
  targaheader[15] := byte (cinfo^.output_height shr 8);
  targaheader[17] := $20;       { Top-down, non-interlaced }

  if (cinfo^.out_color_space = JCS_GRAYSCALE) then
  begin
    targaheader[2] := 3;                { image type := uncompressed gray-scale }
    targaheader[16] := 8;       { bits per pixel }
  end
  else
  begin                 { must be RGB }
    if (num_colors > 0) then
    begin
      targaheader[2] := 1;      { image type = colormapped RGB }
      targaheader[16] := 8;
    end
    else
    begin
      targaheader[2] := 2;      { image type = uncompressed RGB }
      targaheader[16] := 24;
    end;
  end;

  if (JFWRITE(dinfo^.output_file, @targaheader, 18) <> size_t (18)) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
end;

{ Write some pixel data.
  In this module rows_supplied will always be 1. }

{METHODDEF}
procedure put_pixel_rows (cinfo : j_decompress_ptr;
                          dinfo : djpeg_dest_ptr;
                          rows_supplied : JDIMENSION); far;
{ used for unquantized full-color output }
var
  dest : tga_dest_ptr;
  {register} inptr : RGBptr;
  {register} outptr : BGRptr;
  {register} col : JDIMENSION;
begin
  dest := tga_dest_ptr (dinfo);

  inptr := RGBptr(dest^.pub.buffer^[0]);
  outptr := BGRptr(dest^.iobuffer);
  for col := pred(cinfo^.output_width) downto 0 do
  begin
    outptr^.b := byte (GETJSAMPLE(inptr^.b)); { RGB to BGR order }
    outptr^.g := byte (GETJSAMPLE(inptr^.g));
    outptr^.r := byte (GETJSAMPLE(inptr^.r));
    Inc(inptr);
    Inc(outptr);
  end;
  {void} JFWRITE(dest^.pub.output_file, dest^.iobuffer, dest^.buffer_width);
end;

{METHODDEF}
procedure put_gray_rows (cinfo : j_decompress_ptr;
                         dinfo : djpeg_dest_ptr;
                         rows_supplied : JDIMENSION); far;
{ used for grayscale OR quantized color output }
var
  dest : tga_dest_ptr;
  {register} inptr : JSAMPLE_PTR;
  {register} outptr : byteptr;
  {register} col : JDIMENSION;
begin
  dest := tga_dest_ptr (dinfo);

  inptr := JSAMPLE_PTR(dest^.pub.buffer^[0]);
  outptr := dest^.iobuffer;
  for col := pred(cinfo^.output_width) downto 0 do
  begin
    outptr^ := byte( GETJSAMPLE(inptr^) );
    Inc(inptr);
    Inc(outptr);
  end;
  {void} JFWRITE(dest^.pub.output_file, dest^.iobuffer, dest^.buffer_width);
end;


{ Write some demapped pixel data when color quantization is in effect.
  For Targa, this is only applied to grayscale data. }

{METHODDEF}
procedure put_demapped_gray (cinfo : j_decompress_ptr;
                             dinfo : djpeg_dest_ptr;
                             rows_supplied : JDIMENSION); far;
var
  dest : tga_dest_ptr;
  {register} inptr : JSAMPLE_PTR;
  {register} outptr : byteptr;
  {register} color_map0 : JSAMPROW;
  {register} col : JDIMENSION;
begin
  dest := tga_dest_ptr (dinfo);
  color_map0 := cinfo^.colormap^[0];

  inptr := JSAMPLE_PTR(dest^.pub.buffer^[0]);
  outptr := dest^.iobuffer;
  for col := pred(cinfo^.output_width) downto 0 do
  begin
    outptr^ := byte( GETJSAMPLE(color_map0^[GETJSAMPLE(inptr^)]) );
    Inc(inptr);
    Inc(outptr);
  end;
  {void} JFWRITE(dest^.pub.output_file, dest^.iobuffer, dest^.buffer_width);
end;


{ Startup: write the file header. }

{METHODDEF}
procedure start_output_tga (cinfo : j_decompress_ptr;
                            dinfo : djpeg_dest_ptr); far;
var
  dest : tga_dest_ptr;
  num_colors, i : int;
  outfile : FILEptr;
var
  output_color_map : Array[0..255] of BGRtype;
begin
  dest := tga_dest_ptr (dinfo);

  if (cinfo^.out_color_space = JCS_GRAYSCALE) then
  begin
    { Targa doesn't have a mapped grayscale format, so we will }
    { demap quantized gray output.  Never emit a colormap. }
    write_header(cinfo, dinfo, 0);
    if (cinfo^.quantize_colors) then
      dest^.pub.put_pixel_rows := put_demapped_gray
    else
      dest^.pub.put_pixel_rows := put_gray_rows;
  end
  else
    if (cinfo^.out_color_space = JCS_RGB) then
    begin
      if (cinfo^.quantize_colors) then
      begin
        { We only support 8-bit colormap indexes, so only 256 colors }
        num_colors := cinfo^.actual_number_of_colors;
        if (num_colors > 256) then
          ERREXIT1(j_common_ptr(cinfo), JERR_TOO_MANY_COLORS, num_colors);
        write_header(cinfo, dinfo, num_colors);
        { Write the colormap.  Note Targa uses BGR byte order }
        outfile := dest^.pub.output_file;
        for i := 0 to pred(num_colors) do
        begin
          output_color_map[i].b := cinfo^.colormap^[2]^[i];
          output_color_map[i].g := cinfo^.colormap^[1]^[i];
          output_color_map[i].r := cinfo^.colormap^[0]^[i];
        end;
        JFWRITE(outfile, @output_color_map, num_colors*3);
        dest^.pub.put_pixel_rows := put_gray_rows;
      end
      else
      begin
        write_header(cinfo, dinfo, 0);
        dest^.pub.put_pixel_rows := put_pixel_rows;
      end;
    end
    else
    begin
      ERREXIT(j_common_ptr(cinfo), JERR_TGA_COLORSPACE);
    end;
end;


{ Finish up at the end of the file. }

{METHODDEF}
procedure finish_output_tga (cinfo : j_decompress_ptr;
                             dinfo : djpeg_dest_ptr); far;
begin
  { Make sure we wrote the output file OK }
  {fflush(dinfo^.output_file^);
  if (ferror(dinfo^.output_file)) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  }
end;


{ The module selection routine for Targa format output. }

{GLOBAL}
function jinit_write_targa (cinfo : j_decompress_ptr) : djpeg_dest_ptr;
var
  dest : tga_dest_ptr;
begin
  { Create module interface object, fill in method pointers }
  dest := tga_dest_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
                                  SIZEOF(tga_dest_struct)) );
  dest^.pub.start_output := start_output_tga;
  dest^.pub.finish_output := finish_output_tga;

  { Calculate output image dimensions so we can allocate space }
  jpeg_calc_output_dimensions(cinfo);

  { Create I/O buffer.  Note we make this near on a PC. }
  dest^.buffer_width := cinfo^.output_width * cinfo^.output_components;
  dest^.iobuffer := byteptr(
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
                            size_t (dest^.buffer_width * SIZEOF(byte))));

  { Create decompressor output buffer. }
  dest^.pub.buffer := cinfo^.mem^.alloc_sarray
    (j_common_ptr (cinfo), JPOOL_IMAGE, dest^.buffer_width, JDIMENSION (1));
  dest^.pub.buffer_height := 1;

  jinit_write_targa := djpeg_dest_ptr (dest);
end;

end. { TARGA_SUPPORTED }
