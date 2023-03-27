Unit wrbmp;

{ Copyright (C) 1994-1996, Thomas G. Lane.
  This code contributed by James Arthur Boucher.

  This file contains routines to write output images in Microsoft "BMP"
  format (MS Windows 3.x and OS/2 1.x flavors).
  Either 8-bit colormapped or 24-bit full-color format can be written.
  No compression is supported. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jpeglib,
  jinclude,
  jdeferr,
  jerror,
  jdmaster,
  cdjpeg;               { Common decls for cjpeg/djpeg applications }

{ The module selection routine for BMP format output. }

{GLOBAL}
function jinit_write_bmp (cinfo : j_decompress_ptr;
                          is_os2 : boolean) : djpeg_dest_ptr;

implementation

{ To support 12-bit JPEG data, we'd have to scale output down to 8 bits.
  This is not yet implemented. }

{$ifndef BITS_IN_JSAMPLE_IS_8}
  Sorry, this code only copes with 8-bit JSAMPLEs. { deliberate syntax err }
{$endif}

{ Since BMP stores scanlines bottom-to-top, we have to invert the image
  from JPEG's top-to-bottom order.  To do this, we save the outgoing data
  in a virtual array during put_pixel_row calls, then actually emit the
  BMP file during finish_output.  The virtual array contains one JSAMPLE per
  pixel if the output is grayscale or colormapped, three if it is full color.}

{ Private version of data destination object }

type
  bmp_dest_ptr = ^bmp_dest_struct;
  bmp_dest_struct = record
    pub : djpeg_dest_struct;    { public fields }

    is_os2 : boolean;           { saves the OS2 format request flag }

    whole_image : jvirt_sarray_ptr; { needed to reverse row order }
    data_width : JDIMENSION;        { JSAMPLEs per row }
    row_width : JDIMENSION;         { physical width of one row in the BMP file }
    pad_bytes : int;                { number of padding bytes needed per row }
    cur_output_row : JDIMENSION;    { next row# to write to virtual array }
  end;

{ Forward declarations }
{LOCAL}
procedure write_colormap(cinfo : j_decompress_ptr;
                         dest : bmp_dest_ptr;
                         map_colors : int;
                         map_entry_size : int); forward;

{ Write some pixel data.
  In this module rows_supplied will always be 1. }

{METHODDEF}
procedure put_pixel_rows (cinfo : j_decompress_ptr;
                          dinfo : djpeg_dest_ptr;
                          rows_supplied : JDIMENSION); far;
{ This version is for writing 24-bit pixels }
var
  dest : bmp_dest_ptr;
  image_ptr : JSAMPARRAY;
  {register} inptr : JSAMPLE_PTR;
             outptr : BGRptr;
  {register} col : JDIMENSION;
  pad : int;
begin
  dest := bmp_dest_ptr (dinfo);

  { Access next row in virtual array }
  image_ptr := cinfo^.mem^.access_virt_sarray
    (j_common_ptr(cinfo), dest^.whole_image,
     dest^.cur_output_row, JDIMENSION (1), TRUE);
  Inc(dest^.cur_output_row);


  { Transfer data.  Note destination values must be in BGR order
    (even though Microsoft's own documents say the opposite). }

  inptr := JSAMPLE_PTR(dest^.pub.buffer^[0]);
  outptr := BGRptr(image_ptr^[0]);
  for col := pred(cinfo^.output_width) downto 0 do
  begin
    outptr^.r := inptr^;        { can omit GETJSAMPLE() safely }
    Inc(inptr);
    outptr^.g := inptr^;
    Inc(inptr);
    outptr^.b := inptr^;
    Inc(inptr);
    Inc(outptr);
  end;

  { Zero out the pad bytes. }
  pad := dest^.pad_bytes;
  while (pad > 0) do
  begin
    Dec(pad);
    JSAMPLE_PTR(outptr)^ := 0;
    Inc(JSAMPLE_PTR(outptr));
  end;
end;

{METHODDEF}
procedure put_gray_rows (cinfo : j_decompress_ptr;
                         dinfo : djpeg_dest_ptr;
                         rows_supplied : JDIMENSION); far;
{ This version is for grayscale OR quantized color output }
var
  dest : bmp_dest_ptr;
  image_ptr : JSAMPARRAY;
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} col : JDIMENSION;
  pad : int;
begin
  dest := bmp_dest_ptr (dinfo);

  { Access next row in virtual array }
  image_ptr := cinfo^.mem^.access_virt_sarray
    (j_common_ptr(cinfo), dest^.whole_image,
     dest^.cur_output_row, JDIMENSION (1), TRUE);
  Inc(dest^.cur_output_row);

  { Transfer data. }
  inptr := JSAMPLE_PTR(dest^.pub.buffer^[0]);
  outptr := JSAMPLE_PTR(image_ptr^[0]);
  for col := pred(cinfo^.output_width) downto 0 do
  begin
    outptr^ := inptr^;  { can omit GETJSAMPLE() safely }
    Inc(outptr);
    Inc(inptr);
  end;

  { Zero out the pad bytes. }
  pad := dest^.pad_bytes;
  while (pad > 0) do
  begin
    Dec(pad);
    outptr^ := 0;
    Inc(outptr);
  end;
end;


{ Startup: normally writes the file header.
  In this module we may as well postpone everything until finish_output. }

{METHODDEF}
procedure start_output_bmp (cinfo : j_decompress_ptr;
                            dinfo : djpeg_dest_ptr); far;
begin
  { no work here }
end;


{ Finish up at the end of the file.

  Here is where we really output the BMP file.

  First, routines to write the Windows and OS/2 variants of the file header. }


{LOCAL}
procedure write_bmp_header (cinfo : j_decompress_ptr;
                            dest : bmp_dest_ptr);
{ Write a Windows-style BMP file header, including colormap if needed }
var
  bmpfileheader : packed array[0..14-1] of byte;
  bmpinfoheader : packed array[0..40-1] of byte;
var
  headersize, bfSize : INT32 ;
  bits_per_pixel, cmap_entries : int;
begin
  { Compute colormap size and total file size }
  if (cinfo^.out_color_space = JCS_RGB) then
  begin
    if (cinfo^.quantize_colors) then
    begin
      { Colormapped RGB }
      bits_per_pixel := 8;
      cmap_entries := 256;
    end
    else
    begin
      { Unquantized, full color RGB }
      bits_per_pixel := 24;
      cmap_entries := 0;
    end;
  end
  else
  begin
    { Grayscale output.  We need to fake a 256-entry colormap. }
    bits_per_pixel := 8;
    cmap_entries := 256;
  end;
  { File size }
  headersize := 14 + 40 + cmap_entries * 4; { Header and colormap }
  bfSize := headersize + INT32 (dest^.row_width) * INT32 (cinfo^.output_height);

  { Set unused fields of header to 0 }
  MEMZERO(@bmpfileheader, SIZEOF(bmpfileheader));
  MEMZERO(@bmpinfoheader, SIZEOF(bmpinfoheader));

  { Fill the file header }
  bmpfileheader[0] := $42;      { first 2 bytes are ASCII 'B', 'M' }
  bmpfileheader[1] := $4D;
  {PUT_4B(bmpfileheader, 2, bfSize);} { bfSize }
         bmpfileheader[2] := byte ((bfSize) and $FF);
         bmpfileheader[2+1] := byte (((bfSize) shr 8) and $FF);
         bmpfileheader[2+2] := byte (((bfSize) shr 16) and $FF);
         bmpfileheader[2+3] := byte (((bfSize) shr 24) and $FF);
  { we leave bfReserved1 & bfReserved2 = 0 }
  {PUT_4B(bmpfileheader, 10, headersize);} { bfOffBits }
         bmpfileheader[10] := byte (headersize and $FF);
         bmpfileheader[10+1] := byte ((headersize shr 8) and $FF);
         bmpfileheader[10+2] := byte ((headersize shr 16) and $FF);
         bmpfileheader[10+3] := byte ((headersize shr 24) and $FF);

  { Fill the info header (Microsoft calls this a BITMAPINFOHEADER) }
  {PUT_2B(bmpinfoheader, 0, 40);}   { biSize }
         bmpinfoheader[0] := byte ((40) and $FF);
         bmpinfoheader[0+1] := byte (((40) shr 8) and $FF);

  {PUT_4B(bmpinfoheader, 4, cinfo^.output_width);} { biWidth }
         bmpinfoheader[4] := byte ((cinfo^.output_width) and $FF);
         bmpinfoheader[4+1] := byte ((cinfo^.output_width shr 8) and $FF);
         bmpinfoheader[4+2] := byte ((cinfo^.output_width shr 16) and $FF);
         bmpinfoheader[4+3] := byte ((cinfo^.output_width shr 24) and $FF);
  {PUT_4B(bmpinfoheader, 8, cinfo^.output_height);} { biHeight }
         bmpinfoheader[8] := byte (cinfo^.output_height and $FF);
         bmpinfoheader[8+1] := byte ((cinfo^.output_height shr 8) and $FF);
         bmpinfoheader[8+2] := byte ((cinfo^.output_height shr 16) and $FF);
         bmpinfoheader[8+3] := byte ((cinfo^.output_height shr 24) and $FF);
  {PUT_2B(bmpinfoheader, 12, 1);}       { biPlanes - must be 1 }
         bmpinfoheader[12] := byte (1 and $FF);
         bmpinfoheader[12+1] := byte ((1 shr 8) and $FF);

  {PUT_2B(bmpinfoheader, 14, bits_per_pixel);} { biBitCount }
         bmpinfoheader[14] := byte (bits_per_pixel and $FF);
         bmpinfoheader[14+1] := byte ((bits_per_pixel shr 8) and $FF);
  { we leave biCompression = 0, for none }
  { we leave biSizeImage = 0; this is correct for uncompressed data }
  if (cinfo^.density_unit = 2) then
  begin { if have density in dots/cm, then }
    {PUT_4B(bmpinfoheader, 24, INT32 (cinfo^.X_density*100));} { XPels/M }
         bmpinfoheader[24] := byte (INT32 (cinfo^.X_density*100) and $FF);
         bmpinfoheader[24+1] := byte ((INT32 (cinfo^.X_density*100) shr 8) and $FF);
         bmpinfoheader[24+2] := byte ((INT32 (cinfo^.X_density*100) shr 16) and $FF);
         bmpinfoheader[24+3] := byte ((INT32 (cinfo^.X_density*100) shr 24) and $FF);
    {PUT_4B(bmpinfoheader, 28, INT32 (cinfo^.Y_density*100));} { XPels/M }
         bmpinfoheader[28] := byte (INT32 (cinfo^.Y_density*100) and $FF);
         bmpinfoheader[28+1] := byte ((INT32 (cinfo^.Y_density*100) shr 8) and $FF);
         bmpinfoheader[28+2] := byte ((INT32 (cinfo^.Y_density*100) shr 16) and $FF);
         bmpinfoheader[28+3] := byte ((INT32 (cinfo^.Y_density*100) shr 24) and $FF);
  end;
  {PUT_2B(bmpinfoheader, 32, cmap_entries);} { biClrUsed }
         bmpinfoheader[32] := byte (cmap_entries and $FF);
         bmpinfoheader[32+1] := byte ((cmap_entries shr 8) and $FF);
  { we leave biClrImportant := 0 }

  if (JFWRITE(dest^.pub.output_file, @bmpfileheader, 14) <> size_t (14)) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  if (JFWRITE(dest^.pub.output_file, @bmpinfoheader, 40) <> size_t (40)) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);

  if (cmap_entries > 0) then
    write_colormap(cinfo, dest, cmap_entries, 4);
end;


{LOCAL}
procedure write_os2_header (cinfo : j_decompress_ptr;
                            dest : bmp_dest_ptr);
{ Write an OS2-style BMP file header, including colormap if needed }
var
  bmpfileheader : array[0..14-1] of byte;
  bmpcoreheader : array[0..12-1] of byte;
  headersize, bfSize : INT32;
  bits_per_pixel, cmap_entries : int;
begin
  { Compute colormap size and total file size }
  if (cinfo^.out_color_space = JCS_RGB) then
  begin
    if (cinfo^.quantize_colors) then
    begin
      { Colormapped RGB }
      bits_per_pixel := 8;
      cmap_entries := 256;
    end
    else
    begin
      { Unquantized, full color RGB }
      bits_per_pixel := 24;
      cmap_entries := 0;
    end;
  end
  else
  begin
    { Grayscale output.  We need to fake a 256-entry colormap. }
    bits_per_pixel := 8;
    cmap_entries := 256;
  end;
  { File size }
  headersize := 14 + 12 + cmap_entries * 3; { Header and colormap }
  bfSize := headersize + INT32 (dest^.row_width) * INT32 (cinfo^.output_height);

  { Set unused fields of header to 0 }
  MEMZERO(@bmpfileheader, SIZEOF(bmpfileheader));
  MEMZERO(@bmpcoreheader, SIZEOF(bmpcoreheader));

  { Fill the file header }
  bmpfileheader[0] := $42;      { first 2 bytes are ASCII 'B', 'M' }
  bmpfileheader[1] := $4D;
  {PUT_4B(bmpfileheader, 2, bfSize);} { bfSize }
         bmpfileheader[2] := byte ((bfSize) and $FF);
         bmpfileheader[2+1] := byte (((bfSize) shr 8) and $FF);
         bmpfileheader[2+2] := byte (((bfSize) shr 16) and $FF);
         bmpfileheader[2+3] := byte (((bfSize) shr 24) and $FF);
  { we leave bfReserved1 & bfReserved2 := 0 }
  {PUT_4B(bmpfileheader, 10, headersize);} { bfOffBits }
         bmpfileheader[10] := byte ((headersize) and $FF);
         bmpfileheader[10+1] := byte (((headersize) shr 8) and $FF);
         bmpfileheader[10+2] := byte (((headersize) shr 16) and $FF);
         bmpfileheader[10+3] := byte (((headersize) shr 24) and $FF);

  { Fill the info header (Microsoft calls this a BITMAPCOREHEADER) }
  {PUT_2B(bmpcoreheader, 0, 12);}       { bcSize }
         bmpcoreheader[0] := byte (12 and $FF);
         bmpcoreheader[0+1] := byte ((12 shr 8) and $FF);
  {PUT_2B(bmpcoreheader, 4, cinfo^.output_width);} { bcWidth }
         bmpcoreheader[4] := byte (cinfo^.output_width and $FF);
         bmpcoreheader[4+1] := byte ((cinfo^.output_width shr 8) and $FF);
  {PUT_2B(bmpcoreheader, 6, cinfo^.output_height);} { bcHeight }
         bmpcoreheader[6] := byte (cinfo^.output_height and $FF);
         bmpcoreheader[6+1] := byte ((cinfo^.output_height shr 8) and $FF);
  {PUT_2B(bmpcoreheader, 8, 1);}        { bcPlanes - must be 1 }
         bmpcoreheader[8] := byte (1 and $FF);
         bmpcoreheader[8+1] := byte ((1 shr 8) and $FF);
  {PUT_2B(bmpcoreheader, 10, bits_per_pixel);} { bcBitCount }
         bmpcoreheader[10] := byte (bits_per_pixel and $FF);
         bmpcoreheader[10+1] := byte ((bits_per_pixel shr 8) and $FF);

  if (JFWRITE(dest^.pub.output_file, @bmpfileheader, 14) <> size_t (14)) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  if (JFWRITE(dest^.pub.output_file, @bmpcoreheader, 12) <> size_t (12)) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);

  if (cmap_entries > 0) then
    write_colormap(cinfo, dest, cmap_entries, 3);
end;


{ Write the colormap.
  Windows uses BGR0 map entries; OS/2 uses BGR entries. }

{LOCAL}
procedure write_colormap (cinfo : j_decompress_ptr;
                          dest : bmp_dest_ptr;
                          map_colors : int;
                          map_entry_size : int);
var
  colormap : JSAMPARRAY;
  num_colors : int;
  outfile : FILEptr;
  i : int;
var
  output_color_map : Array[0..255] of BGRtype;
  output_ext_color_map : Array[0..255] of record
                                            b,g,r,a : byte;
                                          end;
begin
  colormap := cinfo^.colormap;
  num_colors := cinfo^.actual_number_of_colors;
  outfile := dest^.pub.output_file;

  if (colormap <> NIL) then
  begin
    if (cinfo^.out_color_components = 3) then
    begin
      { Normal case with RGB colormap }
      if (map_entry_size = 4) then
        for i := 0 to pred(num_colors) do
        with output_ext_color_map[i] do
        begin
          b := GETJSAMPLE(cinfo^.colormap^[2]^[i]);
          g := GETJSAMPLE(cinfo^.colormap^[1]^[i]);
          r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
          a := 0;
        end
      else
        for i := 0 to pred(num_colors) do
        with output_color_map[i] do
        begin
          b := GETJSAMPLE(cinfo^.colormap^[2]^[i]);
          g := GETJSAMPLE(cinfo^.colormap^[1]^[i]);
          r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
        end;
    end
    else
    begin
      { Grayscale colormap (only happens with grayscale quantization) }
      if (map_entry_size = 4) then
        for i := 0 to pred(num_colors) do
        with output_ext_color_map[i] do
        begin
          b := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
          g := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
          r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
          a := 0;
        end
      else
        for i := 0 to pred(num_colors) do
        with output_color_map[i] do
        begin
          b := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
          g := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
          r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
        end;
    end;
    i := num_colors;
  end
  else
  begin
    { If no colormap, must be grayscale data.  Generate a linear "map". }
    { Nomssi: do not use "num_colors" here, it should be 0 }
    if (map_entry_size = 4) then
      for i := 0 to pred(256) do
      with output_ext_color_map[i] do
      begin
        b := i;
        g := i;
        r := i;
        a := 0;
      end
    else
      for i := 0 to pred(256) do
      with output_color_map[i] do
      begin
        b := i;
        g := i;
        r := i;
      end;
    i := 256;
  end;
  { Pad colormap with zeros to ensure specified number of colormap entries }

  if (i > map_colors) then
    ERREXIT1(j_common_ptr(cinfo), JERR_TOO_MANY_COLORS, i);
  while (i < map_colors) do
  begin
    if (map_entry_size = 4) then
    with output_ext_color_map[i] do
    begin
      b := 0;
      g := 0;
      r := 0;
      a := 0;
    end
    else
    with output_color_map[i] do
    begin
      b := 0;
      g := 0;
      r := 0;
    end;
    Inc(i);
  end;
  if (map_entry_size = 4) then
    JFWRITE(outfile, @output_ext_color_map, map_colors*4)
  else
    JFWRITE(outfile, @output_color_map, map_colors*3);
end;


{METHODDEF}
procedure finish_output_bmp (cinfo : j_decompress_ptr;
                             dinfo : djpeg_dest_ptr); far;
var
  dest : bmp_dest_ptr;
  {register} outfile : FILEptr;
  image_ptr : JSAMPARRAY;
  {register} data_ptr : JSAMPLE_PTR;
  row : JDIMENSION;
  {register} { col : JDIMENSION; }
  progress : cd_progress_ptr;
begin
  dest := bmp_dest_ptr (dinfo);
  outfile := dest^.pub.output_file;
  progress := cd_progress_ptr (cinfo^.progress);

  { Write the header and colormap }
  if (dest^.is_os2) then
    write_os2_header(cinfo, dest)
  else
    write_bmp_header(cinfo, dest);

  { Write the file body from our virtual array }
  for row := cinfo^.output_height downto 1 do
  begin
    if (progress <> NIL) then
    begin
      progress^.pub.pass_counter := long (cinfo^.output_height - row);
      progress^.pub.pass_limit := long (cinfo^.output_height);
      progress^.pub.progress_monitor (j_common_ptr(cinfo));
    end;
    image_ptr := cinfo^.mem^.access_virt_sarray
      (j_common_ptr(cinfo), dest^.whole_image, row-1, JDIMENSION(1), FALSE);
    data_ptr := JSAMPLE_PTR(image_ptr^[0]);
    { Nomssi - This won't work for 12bit samples }
    JFWRITE(outfile, data_ptr, dest^.row_width);
    {
    for col := pred(dest^.row_width) downto 0 do
    begin
      putc(GETJSAMPLE(data_ptr^), outfile);
      Inc(data_ptr);
    end;
    }
  end;
  if (progress <> NIL) then
    Inc(progress^.completed_extra_passes);

  { Make sure we wrote the output file OK }
  {fflush(outfile);
  if (ferror(outfile)) then
    ERREXIT(cinfo, JERR_FILE_WRITE);}
end;


{ The module selection routine for BMP format output. }

{GLOBAL}
function jinit_write_bmp (cinfo : j_decompress_ptr;
                          is_os2 : boolean) : djpeg_dest_ptr;
var
  dest : bmp_dest_ptr;
  row_width : JDIMENSION;
var
  progress : cd_progress_ptr;
begin
  { Create module interface object, fill in method pointers }
  dest := bmp_dest_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  SIZEOF(bmp_dest_struct)) );
  dest^.pub.start_output := start_output_bmp;
  dest^.pub.finish_output := finish_output_bmp;
  dest^.is_os2 := is_os2;

  if (cinfo^.out_color_space = JCS_GRAYSCALE) then
  begin
    dest^.pub.put_pixel_rows := put_gray_rows;
  end
  else
    if (cinfo^.out_color_space = JCS_RGB) then
    begin
      if (cinfo^.quantize_colors) then
        dest^.pub.put_pixel_rows := put_gray_rows
      else
        dest^.pub.put_pixel_rows := put_pixel_rows;
  end
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_COLORSPACE);

  { Calculate output image dimensions so we can allocate space }
  jpeg_calc_output_dimensions(cinfo);

  { Determine width of rows in the BMP file (padded to 4-byte boundary). }
  row_width := cinfo^.output_width * cinfo^.output_components;
  dest^.data_width := row_width;
  while ((row_width and 3) <> 0) do
    Inc(row_width);
  dest^.row_width := row_width;
  dest^.pad_bytes := int (row_width - dest^.data_width);

  { Allocate space for inversion array, prepare for write pass }
  dest^.whole_image := cinfo^.mem^.request_virt_sarray
    (j_common_ptr(cinfo), JPOOL_IMAGE, FALSE,
     row_width, cinfo^.output_height, JDIMENSION (1));
  dest^.cur_output_row := 0;
  if (cinfo^.progress <> NIL) then
  begin
    progress := cd_progress_ptr (cinfo^.progress);
    Inc(progress^.total_extra_passes); { count file input as separate pass }
  end;

  { Create decompressor output buffer. }
  dest^.pub.buffer := cinfo^.mem^.alloc_sarray
    (j_common_ptr(cinfo), JPOOL_IMAGE, row_width, JDIMENSION (1));
  dest^.pub.buffer_height := 1;

  jinit_write_bmp := djpeg_dest_ptr(dest);
end;

end. { BMP_SUPPORTED }
