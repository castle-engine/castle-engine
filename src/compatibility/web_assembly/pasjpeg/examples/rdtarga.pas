Unit RdTarga;

{ rdtarga.c ;  Copyright (C) 1991-1996, Thomas G. Lane.

  These routines may need modification for non-Unix environments or
  specialized applications.  As they stand, they assume input from
  an ordinary stdio stream.  They further assume that reading begins
  at the start of the file; start_input may need work if the
  user interface has already read some data (e.g., to determine that
  the file is indeed Targa format).

  Based on code contributed by Lee Daniel Crocker. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jpeglib,
  jinclude,
  jdeferr,
  jerror,
  cdjpeg;               { Common decls for cjpeg/djpeg applications }


{ The module selection routine for Targa format input. }

{GLOBAL}
function jinit_read_targa (cinfo : j_compress_ptr) : cjpeg_source_ptr;

implementation

{ Macros to deal with unsigned chars as efficiently as compiler allows }

type
  U_CHAR = byte;
  UCH = int;
(*type
{$ifdef CHAR_IS_UNSIGNED}
  UCH = int;
{$else}
  UCH = int (x and $FF);
{$endif}
*)
{ Private version of data source object }

type
  tga_source_ptr = ^tga_source_struct;
  tga_source_struct = record
    pub : cjpeg_source_struct; { public fields }

    cinfo : j_compress_ptr;             { back link saves passing separate parm }

    colormap : JSAMPARRAY;              { Targa colormap (converted to my format) }

    whole_image : jvirt_sarray_ptr;     { Needed if funny input row order }
    current_row : JDIMENSION;           { Current logical row number to read }

    { Pointer to routine to extract next Targa pixel from input file }
    read_pixel : procedure (sinfo : tga_source_ptr);

    { Result of read_pixel is delivered here: }
    tga_pixel : array[0..4-1] of U_CHAR;

    pixel_size : int;                   { Bytes per Targa pixel (1 to 4) }

    { State info for reading RLE-coded pixels; both counts must be init to 0 }
    block_count : int;          { # of pixels remaining in RLE block }
    dup_pixel_count : int;      { # of times to duplicate previous pixel }

    { This saves the correct pixel-row-expansion method for preload_image }
    get_pixel_rows : function(cinfo : j_compress_ptr;
                              sinfo : cjpeg_source_ptr) : JDIMENSION;
  end;

{ For expanding 5-bit pixel values to 8-bit with best rounding }

const
  c5to8bits : array[0..32-1] of UINT8 =
               (  0,   8,  16,  25,  33,  41,  49,  58,
                 66,  74,  82,  90,  99, 107, 115, 123,
                132, 140, 148, 156, 165, 173, 181, 189,
                197, 206, 214, 222, 230, 239, 247, 255);

function getc(f : fileptr) : byte;
begin
  getc := 0;
end;
const
  EOF = byte(26);  { ^Z }

{LOCAL}
function read_byte (sinfo : tga_source_ptr) : int;
{ Read next byte from Targa file }
var
  {register} infile : FILEptr;
  {register} c : int;
begin
  infile := sinfo^.pub.input_file;
  c := getc(infile);
  if (c = EOF) then
    ERREXIT(j_common_ptr(sinfo^.cinfo), JERR_INPUT_EOF);
  read_byte := c;
end;


{LOCAL}
procedure read_colormap (sinfo : tga_source_ptr;
                         cmaplen : int;
                         mapentrysize : int);
{ Read the colormap from a Targa file }
var
  i : int;
begin
  { Presently only handles 24-bit BGR format }
  if (mapentrysize <> 24) then
    ERREXIT(j_common_ptr(sinfo^.cinfo), JERR_TGA_BADCMAP);

  for i := 0 to pred(cmaplen) do
  begin
    sinfo^.colormap^[2]^[i] := JSAMPLE (read_byte(sinfo));
    sinfo^.colormap^[1]^[i] := JSAMPLE (read_byte(sinfo));
    sinfo^.colormap^[0]^[i] := JSAMPLE (read_byte(sinfo));
  end;
end;


{ read_pixel methods: get a single pixel from Targa file into tga_pixel[] }

{METHODDEF}
procedure read_non_rle_pixel (sinfo : tga_source_ptr); far;
{ Read one Targa pixel from the input file; no RLE expansion }
var
  {register} infile : FILEptr;
  {register} i : int;
begin
  infile := sinfo^.pub.input_file;
  for i := 0 to pred(sinfo^.pixel_size) do
  begin
    sinfo^.tga_pixel[i] := U_CHAR (getc(infile));
  end;
end;


{METHODDEF}
procedure read_rle_pixel (sinfo : tga_source_ptr); far;
{ Read one Targa pixel from the input file, expanding RLE data as needed }
var
  {register} infile : FILEptr;
  {register} i : int;
begin
  infile := sinfo^.pub.input_file;

  { Duplicate previously read pixel? }
  if (sinfo^.dup_pixel_count > 0) then
  begin
    Dec(sinfo^.dup_pixel_count);
    exit;
  end;

  { Time to read RLE block header? }
  Dec(sinfo^.block_count);
  if (sinfo^.block_count < 0) then
  begin { decrement pixels remaining in block }
    i := read_byte(sinfo);
    if (i and $80) <> 0 then
    begin               { Start of duplicate-pixel block? }
      sinfo^.dup_pixel_count := i and $7F; { number of dups after this one }
      sinfo^.block_count := 0;  { then read new block header }
    end
    else
    begin
      sinfo^.block_count := i and $7F; { number of pixels after this one }
    end;
  end;

  { Read next pixel }
  for i := 0 to pred(sinfo^.pixel_size) do
  begin
    sinfo^.tga_pixel[i] := U_CHAR (getc(infile));
  end;
end;


{ Read one row of pixels.

  We provide several different versions depending on input file format. }

{METHODDEF}
function get_8bit_gray_row (cinfo : j_compress_ptr;
                            sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading 8-bit grayscale pixels }
var
  source : tga_source_ptr;
  {register} ptr : JSAMPLE_PTR;
  {register} col : JDIMENSION;
begin
  source := tga_source_ptr (sinfo);
  ptr := JSAMPLE_PTR(source^.pub.buffer^[0]);
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    source^.read_pixel (source); { Load next pixel into tga_pixel }
    ptr^ := JSAMPLE (UCH(source^.tga_pixel[0]));
    Inc(ptr);
  end;
  get_8bit_gray_row  := 1;
end;

{METHODDEF}
function get_8bit_row (cinfo : j_compress_ptr;
                       sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading 8-bit colormap indexes }
var
  source : tga_source_ptr;
  {register} t : int;
  {register} ptr : JSAMPLE_PTR;
  {register} col : JDIMENSION;
  {register} colormap : JSAMPARRAY;
begin
  source := tga_source_ptr (sinfo);
  colormap := source^.colormap;

  ptr := JSAMPLE_PTR(source^.pub.buffer^[0]);
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    source^.read_pixel (source); { Load next pixel into tga_pixel }
    t := UCH(source^.tga_pixel[0]);
    ptr^ := colormap^[0]^[t];
    Inc(ptr);
    ptr^ := colormap^[1]^[t];
    Inc(ptr);
    ptr^ := colormap^[2]^[t];
    Inc(ptr);
  end;
  get_8bit_row  := 1;
end;

{METHODDEF}
function get_16bit_row (cinfo : j_compress_ptr;
                        sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading 16-bit pixels }
var
  source : tga_source_ptr;

  {register} t : int;
  {register} ptr : JSAMPROW;
  {register} col : JDIMENSION;
begin
  source := tga_source_ptr (sinfo);

  ptr := source^.pub.buffer^[0];
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    source^.read_pixel (source); { Load next pixel into tga_pixel }
    t := UCH(source^.tga_pixel[0]);
    Inc(t, UCH(source^.tga_pixel[1]) shr 8);
    { We expand 5 bit data to 8 bit sample width.
      The format of the 16-bit (LSB first) input word is
          xRRRRRGGGGGBBBBB
     }
    ptr^[2] := JSAMPLE (c5to8bits[t and $1F]);
    t := t shr 5;
    ptr^[1] := JSAMPLE (c5to8bits[t and $1F]);
    t := t shr 5;
    ptr^[0] := JSAMPLE (c5to8bits[t and $1F]);
    Inc(JSAMPLE_PTR(ptr), 3);
  end;
  get_16bit_row  :=1;
end;

{METHODDEF}
function get_24bit_row (cinfo : j_compress_ptr;
                        sinfo : cjpeg_source_ptr) : JDIMENSION; far;
{ This version is for reading 24-bit pixels }
var
  source : tga_source_ptr;

  {register} ptr : JSAMPLE_PTR;
  {register} col : JDIMENSION;
begin
  source := tga_source_ptr (sinfo);
  ptr := JSAMPLE_PTR(source^.pub.buffer^[0]);
  for col := pred(cinfo^.image_width) downto 0 do
  begin
    source^.read_pixel (source); { Load next pixel into tga_pixel }
    ptr^ := JSAMPLE (UCH(source^.tga_pixel[2])); { change BGR to RGB order }
    Inc(ptr);
    ptr^ := JSAMPLE (UCH(source^.tga_pixel[1]));
    Inc(ptr);
    ptr^ := JSAMPLE (UCH(source^.tga_pixel[0]));
    Inc(ptr);
  end;
  get_24bit_row := 1;
end;

{ Targa also defines a 32-bit pixel format with order B,G,R,A.
  We presently ignore the attribute byte, so the code for reading
  these pixels is identical to the 24-bit routine above.
  This works because the actual pixel length is only known to read_pixel. }

const
  get_32bit_row : function (cinfo : j_compress_ptr;
                          sinfo : cjpeg_source_ptr) : JDIMENSION
   = get_24bit_row;


{ This method is for re-reading the input data in standard top-down
  row order.  The entire image has already been read into whole_image
  with proper conversion of pixel format, but it's in a funny row order. }

{METHODDEF}
function get_memory_row (cinfo : j_compress_ptr;
                         sinfo : cjpeg_source_ptr) : JDIMENSION; far;
var
  source : tga_source_ptr;
  source_row : JDIMENSION;
begin
  source := tga_source_ptr (sinfo);
  { Compute row of source that maps to current_row of normal order }
  { For now, assume image is bottom-up and not interlaced. }
  { NEEDS WORK to support interlaced images! }
  source_row := cinfo^.image_height - source^.current_row - 1;

  { Fetch that row from virtual array }
  source^.pub.buffer := cinfo^.mem^.access_virt_sarray
                        (j_common_ptr (cinfo), source^.whole_image,
                        source_row, JDIMENSION (1), FALSE);

  Inc(source^.current_row);
  get_memory_row := 1;
end;


{ This method loads the image into whole_image during the first call on
  get_pixel_rows.  The get_pixel_rows pointer is then adjusted to call
  get_memory_row on subsequent calls. }

{METHODDEF}
function preload_image (cinfo : j_compress_ptr;
                        sinfo : cjpeg_source_ptr) : JDIMENSION; far;
var
  source : tga_source_ptr;
  row : JDIMENSION;
  progress : cd_progress_ptr;
begin
  source := tga_source_ptr (sinfo);
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
    source^.pub.buffer := cinfo^.mem^.access_virt_sarray
      (j_common_ptr(cinfo), source^.whole_image, row, JDIMENSION(1), TRUE);
    source^.get_pixel_rows (cinfo, sinfo);
  end;
  if (progress <> NIL) then
    Inc(progress^.completed_extra_passes);

  { Set up to read from the virtual array in unscrambled order }
  source^.pub.get_pixel_rows := get_memory_row;
  source^.current_row := 0;
  { And read the first row }
  preload_image := get_memory_row(cinfo, sinfo);
end;


{ Read the file header; return image size and component count. }

{METHODDEF}
procedure start_input_tga (cinfo : j_compress_ptr;
                           sinfo : cjpeg_source_ptr); far;
var
  source : tga_source_ptr;
  targaheader : array[0..18-1] of U_CHAR;
  idlen, cmaptype, subtype, flags, interlace_type, components : int;
  width, height, maplen : uInt;
  is_bottom_up : boolean;
var
  progress : cd_progress_ptr;
begin
  source := tga_source_ptr (sinfo);

  if JFREAD(source^.pub.input_file, @targaheader, 18) <> size_t(18) then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);

  { Pretend "15-bit" pixels are 16-bit --- we ignore attribute bit anyway }
  if (targaheader[16] = 15) then
    targaheader[16] := 16;

  idlen := UCH(targaheader[0]);
  cmaptype := UCH(targaheader[1]);
  subtype := UCH(targaheader[2]);
  maplen := {GET_2B(5);}
            uInt (UCH(targaheader[5])) +
            ( uInt (UCH(targaheader[5+1])) ) shl 8;
  width := {GET_2B(12);}
           ( uInt(UCH(targaheader[12])) +
           ( uInt(UCH(targaheader[12+1])) ) shl 8);
  height := {GET_2B(14);}
           ( uInt(UCH(targaheader[14])) +
           ( uInt(UCH(targaheader[14+1])) ) shl 8);

  source^.pixel_size := UCH(targaheader[16]) shl 3;
  flags := UCH(targaheader[17]);        { Image Descriptor byte }

  is_bottom_up := (flags and $20) = 0;  { bit 5 set => top-down }
  interlace_type := flags shl 6;        { bits 6/7 are interlace code }

  if (cmaptype > 1) or                  { cmaptype must be 0 or 1 }
     (source^.pixel_size < 1) or (source^.pixel_size > 4) or
     ((UCH(targaheader[16]) and 7) <> 0) or { bits/pixel must be multiple of 8 }
     (interlace_type <> 0) then  { currently don't allow interlaced image }
    ERREXIT(j_common_ptr(cinfo), JERR_TGA_BADPARMS);

  if (subtype > 8) then
  begin
    { It's an RLE-coded file }
    source^.read_pixel := read_rle_pixel;
    source^.block_count := 0;
    source^.dup_pixel_count := 0;
    Dec(subtype, 8);
  end
  else
  begin
    { Non-RLE file }
    source^.read_pixel := read_non_rle_pixel;
  end;

  { Now should have subtype 1, 2, or 3 }
  components := 3;              { until proven different }
  cinfo^.in_color_space := JCS_RGB;

  case (subtype) of
  1:begin  { Colormapped image }
      if (source^.pixel_size = 1) and (cmaptype = 1) then
        source^.get_pixel_rows := get_8bit_row
      else
        ERREXIT(j_common_ptr(cinfo), JERR_TGA_BADPARMS);
      TRACEMS2(j_common_ptr(cinfo), 1, JTRC_TGA_MAPPED, width, height);
    end;
  2:begin  { RGB image }
      case (source^.pixel_size) of
      2: source^.get_pixel_rows := get_16bit_row;
      3: source^.get_pixel_rows := get_24bit_row;
      4: source^.get_pixel_rows := get_32bit_row;
      else
        ERREXIT(j_common_ptr(cinfo), JERR_TGA_BADPARMS);
      end;
      TRACEMS2(j_common_ptr(cinfo), 1, JTRC_TGA, width, height);
    end;
  3:begin       { Grayscale image }
      components := 1;
      cinfo^.in_color_space := JCS_GRAYSCALE;
      if (source^.pixel_size = 1) then
        source^.get_pixel_rows := get_8bit_gray_row
      else
        ERREXIT(j_common_ptr(cinfo), JERR_TGA_BADPARMS);
      TRACEMS2(j_common_ptr(cinfo), 1, JTRC_TGA_GRAY, width, height);
    end;
  else
    ERREXIT(j_common_ptr(cinfo), JERR_TGA_BADPARMS);
  end;

  if (is_bottom_up) then
  begin
    { Create a virtual array to buffer the upside-down image. }
    source^.whole_image := cinfo^.mem^.request_virt_sarray
      (j_common_ptr (cinfo), JPOOL_IMAGE, FALSE,
       JDIMENSION(width * components), JDIMENSION (height), JDIMENSION (1));
    if (cinfo^.progress <> NIL) then
    begin
      progress := cd_progress_ptr (cinfo^.progress);
      Inc(progress^.total_extra_passes); { count file input as separate pass }
    end;
    { source^.pub.buffer will point to the virtual array. }
    source^.pub.buffer_height := 1; { in case anyone looks at it }
    source^.pub.get_pixel_rows := preload_image;
  end
  else
  begin
    { Don't need a virtual array, but do need a one-row input buffer. }
    source^.whole_image := NIL;
    source^.pub.buffer := cinfo^.mem^.alloc_sarray (
       j_common_ptr (cinfo), JPOOL_IMAGE,
       JDIMENSION (width * components), JDIMENSION (1)) ;
    source^.pub.buffer_height := 1;
    source^.pub.get_pixel_rows := source^.get_pixel_rows;
  end;

  while (idlen > 0) do  { Throw away ID field }
  begin
    Dec(idlen);
    {void} read_byte(source);
  end;

  if (maplen > 0) then
  begin
    if (maplen > 256) or {GET_2B(3) <> 0}
       ( (uInt (UCH(targaheader[3])) +
         (uInt (UCH(targaheader[3+1])) ) shl 8) <> 0) then
      ERREXIT(j_common_ptr(cinfo), JERR_TGA_BADCMAP);
    { Allocate space to store the colormap }
    source^.colormap := cinfo^.mem^.alloc_sarray (
                        j_common_ptr (cinfo), JPOOL_IMAGE,
                        JDIMENSION (maplen), JDIMENSION (3));
    { and read it from the file }
    read_colormap(source, int (maplen), UCH(targaheader[7]));
  end
  else
  begin
    if (cmaptype <> 0) then         { but you promised a cmap! }
      ERREXIT(j_common_ptr(cinfo), JERR_TGA_BADPARMS);
    source^.colormap := NIL;
  end;

  cinfo^.input_components := components;
  cinfo^.data_precision := 8;
  cinfo^.image_width := width;
  cinfo^.image_height := height;
end;


{ Finish up at the end of the file. }

{METHODDEF}
procedure finish_input_tga (cinfo : j_compress_ptr;
                            sinfo : cjpeg_source_ptr); far;
begin
  { no work }
end;


{ The module selection routine for Targa format input. }

{GLOBAL}
function jinit_read_targa (cinfo : j_compress_ptr) : cjpeg_source_ptr;
var
  source : tga_source_ptr;
begin
  { Create module interface object }
  source := tga_source_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
                                  SIZEOF(tga_source_struct)) );
  source^.cinfo := cinfo;       { make back link for subroutines }
  { Fill in method ptrs, except get_pixel_rows which start_input sets }
  source^.pub.start_input := start_input_tga;
  source^.pub.finish_input := finish_input_tga;

  jinit_read_targa  := cjpeg_source_ptr (source);
end;

end. { TARGA_SUPPORTED }
