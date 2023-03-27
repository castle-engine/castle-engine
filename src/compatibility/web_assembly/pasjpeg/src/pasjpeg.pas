unit PasJPeg;

{$I jconfig.inc}

interface

uses
  Classes, SysUtils;

type
  EJPEG = class(Exception);
  JPEG_ProgressMonitor = procedure(Percent: Integer);

procedure LoadJPEG(
  {streams:}
  const infile, outfile: TStream; inmemory: boolean;
  {decompression parameters:}
  numcolors: integer;
  {progress monitor}
  callback: JPEG_ProgressMonitor);

procedure StoreJPEG(
  {streams}
  const infile, outfile: TStream; inmemory: boolean;
  {compression parameters:}
  quality: integer;
  {progress monitor}
  callback: JPEG_ProgressMonitor);

implementation

uses
//  WinTypes, Dialogs,
  {PASJPG10 library}
  jmorecfg,
  jpeglib,
  jerror,
  jdeferr,
  jdmarker,
  jdmaster,
  jdapimin,
  jdapistd,
  jcparam,
  jcapimin,
  jcapistd,
  jcomapi;

{ ---------------------------------------------------------------------- }
{   source manager to read compressed data                               }
{   for reference: JDATASRC.PAS in PASJPG10 library                      }
{ ---------------------------------------------------------------------- }

type
  my_src_ptr = ^my_source_mgr;
  my_source_mgr = record
    pub    : jpeg_source_mgr;   {public fields}
    infile : TStream;           {source stream}
    buffer : JOCTET_FIELD_PTR;  {start of buffer}
    start_of_file : boolean;    {have we gotten any data yet?}
  end;

const
  INPUT_BUF_SIZE = 4096;

procedure init_source(cinfo : j_decompress_ptr); far;
var
  src : my_src_ptr;
begin
  src := my_src_ptr(cinfo^.src);
  src^.start_of_file := TRUE;
end;

function fill_input_buffer(cinfo : j_decompress_ptr) : boolean; far;
var
  src : my_src_ptr;
  nbytes : size_t;
begin
  src := my_src_ptr(cinfo^.src);
  nbytes := src^.infile.Read(src^.buffer^, INPUT_BUF_SIZE);
  if (nbytes <= 0) then begin
    if (src^.start_of_file) then   {Treat empty input file as fatal error}
      ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EMPTY);
    WARNMS(j_common_ptr(cinfo), JWRN_JPEG_EOF);
    {Insert a fake EOI marker}
    src^.buffer^[0] := JOCTET ($FF);
    src^.buffer^[1] := JOCTET (JPEG_EOI);
    nbytes := 2;
  end;
  src^.pub.next_input_byte := JOCTETptr(src^.buffer);
  src^.pub.bytes_in_buffer := nbytes;
  src^.start_of_file := FALSE;
  fill_input_buffer := TRUE;
end;

procedure skip_input_data(cinfo : j_decompress_ptr;
                      num_bytes : long); far;
var
  src : my_src_ptr;
begin
  src := my_src_ptr (cinfo^.src);
  if (num_bytes > 0) then begin
    while (num_bytes > long(src^.pub.bytes_in_buffer)) do begin
      Dec(num_bytes, long(src^.pub.bytes_in_buffer));
      fill_input_buffer(cinfo);
      { note we assume that fill_input_buffer will never return FALSE,
        so suspension need not be handled. }
    end;
    Inc( src^.pub.next_input_byte, size_t(num_bytes) );
    Dec( src^.pub.bytes_in_buffer, size_t(num_bytes) );
  end;
end;

procedure term_source(cinfo : j_decompress_ptr); far;
begin
  { no work necessary here }
end;

procedure jpeg_stream_src(cinfo : j_decompress_ptr; const infile: TStream);
var
  src : my_src_ptr;
begin
  if (cinfo^.src = nil) then begin {first time for this JPEG object?}
    cinfo^.src := jpeg_source_mgr_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                  SIZEOF(my_source_mgr)) );
    src := my_src_ptr (cinfo^.src);
    src^.buffer := JOCTET_FIELD_PTR(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                  INPUT_BUF_SIZE * SIZEOF(JOCTET)) );
  end;
  src := my_src_ptr (cinfo^.src);
  {override pub's method pointers}
  src^.pub.init_source := init_source;
  src^.pub.fill_input_buffer := fill_input_buffer;
  src^.pub.skip_input_data := skip_input_data;
  src^.pub.resync_to_restart := jpeg_resync_to_restart; {use default method}
  src^.pub.term_source := term_source;
  {define our fields}
  src^.infile := infile;
  src^.pub.bytes_in_buffer := 0;   {forces fill_input_buffer on first read}
  src^.pub.next_input_byte := nil; {until buffer loaded}
end;

{ ---------------------------------------------------------------------- }
{   destination manager to write compressed data                         }
{   for reference: JDATADST.PAS in PASJPG10 library                      }
{ ---------------------------------------------------------------------- }

type
  my_dest_ptr = ^my_destination_mgr;
  my_destination_mgr = record
    pub     : jpeg_destination_mgr;  {public fields}
    outfile : TStream;               {target stream}
    buffer  : JOCTET_FIELD_PTR;      {start of buffer}
  end;

const
  OUTPUT_BUF_SIZE = 4096;

procedure init_destination(cinfo : j_compress_ptr); far;
var
  dest : my_dest_ptr;
begin
  dest := my_dest_ptr(cinfo^.dest);
  dest^.buffer := JOCTET_FIELD_PTR(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  OUTPUT_BUF_SIZE * SIZEOF(JOCTET)) );
  dest^.pub.next_output_byte := JOCTETptr(dest^.buffer);
  dest^.pub.free_in_buffer := OUTPUT_BUF_SIZE;
end;

function empty_output_buffer(cinfo : j_compress_ptr) : boolean; far;
var
  dest : my_dest_ptr;
begin
  dest := my_dest_ptr(cinfo^.dest);
  if (dest^.outfile.Write(dest^.buffer^, OUTPUT_BUF_SIZE)
        <> size_t(OUTPUT_BUF_SIZE))
  then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  dest^.pub.next_output_byte := JOCTETptr(dest^.buffer);
  dest^.pub.free_in_buffer := OUTPUT_BUF_SIZE;
  empty_output_buffer := TRUE;
end;

procedure term_destination(cinfo : j_compress_ptr); far;
var
  dest : my_dest_ptr;
  datacount : size_t;
begin
  dest := my_dest_ptr (cinfo^.dest);
  datacount := OUTPUT_BUF_SIZE - dest^.pub.free_in_buffer;
  {write any data remaining in the buffer}
  if (datacount > 0) then
    if dest^.outfile.Write(dest^.buffer^, datacount) <> datacount then
      ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
end;

procedure jpeg_stream_dest(cinfo : j_compress_ptr; const outfile: TStream);
var
  dest : my_dest_ptr;
begin
  if (cinfo^.dest = nil) then begin {first time for this JPEG object?}
    cinfo^.dest := jpeg_destination_mgr_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                  SIZEOF(my_destination_mgr)) );
  end;
  dest := my_dest_ptr (cinfo^.dest);
  {override pub's method pointers}
  dest^.pub.init_destination := init_destination;
  dest^.pub.empty_output_buffer := empty_output_buffer;
  dest^.pub.term_destination := term_destination;
  {define our fields}
  dest^.outfile := outfile;
end;

{ ------------------------------------------------------------------------ }
{   Bitmap writing routines                                                }
{   for reference: WRBMP.PAS in PASJPG10 library                           }
{ ------------------------------------------------------------------------ }
{   NOTE: we always write BMP's in Windows format, no OS/2 formats!        }
{         however, we read all bitmap flavors (see bitmap reading)         }
{ ------------------------------------------------------------------------ }

{ To support 12-bit JPEG data, we'd have to scale output down to 8 bits.
  This is not yet implemented. }

{$ifndef BITS_IN_JSAMPLE_IS_8}
  Sorry, this code only copes with 8-bit JSAMPLEs. { deliberate syntax err }
{$endif}

type
  BGRptr = ^BGRtype;
  BGRtype = packed record
    b,g,r : byte;
  end;

  RGBptr = ^RGBtype;
  RGBtype = packed record
    r,g,b : JSAMPLE;
  end;

  bmp_dest_ptr = ^bmp_dest_struct;
  bmp_dest_struct = record
    outfile : TStream;              {Stream to write to}
    inmemory : boolean;             {keep whole image in memory}
    {image info}
    data_width : JDIMENSION;        {JSAMPLEs per row}
    row_width : JDIMENSION;         {physical width of one row in the BMP file}
    pad_bytes : INT;                {number of padding bytes needed per row}
    grayscale : boolean;            {grayscale or quantized color table ?}
    {pixelrow buffer}
    buffer : JSAMPARRAY;            {pixelrow buffer}
    buffer_height : JDIMENSION;     {normally, we'll use 1}
    {image buffer}
    image_buffer : jvirt_sarray_ptr;{needed to reverse row order BMP<>JPG}
    image_buffer_height : JDIMENSION;  {}
    cur_output_row : JDIMENSION;    {next row# to write to virtual array}
    row_offset : INT32;             {position of next row to write to BMP}
  end;

procedure write_bmp_header (cinfo : j_decompress_ptr;
                             dest : bmp_dest_ptr);
  {Write a Windows-style BMP file header, including colormap if needed}
var
  bmpfileheader : TBitmapFileHeader;
  bmpinfoheader : TBitmapInfoHeader;
  headersize    : INT32;
  bits_per_pixel, cmap_entries, num_colors, i : INT;
  output_ext_color_map : array[0..255] of record b,g,r,a: byte; end;
begin
  {colormap size and total file size}
  if (cinfo^.out_color_space = JCS_RGB) then begin
    if (cinfo^.quantize_colors) then begin {colormapped RGB}
      bits_per_pixel := 8;
      cmap_entries := 256;
    end else begin {unquantized, full color RGB}
      bits_per_pixel := 24;
      cmap_entries := 0;
    end;
  end else begin {grayscale output. We need to fake a 256-entry colormap.}
    bits_per_pixel := 8;
    cmap_entries := 256;
  end;
  headersize := SizeOf(TBitmapFileHeader)+SizeOf(TBitmapInfoHeader)+
                  cmap_entries * 4;
  {define headers}
  FillChar(bmpfileheader, SizeOf(bmpfileheader), $0);
  FillChar(bmpinfoheader, SizeOf(bmpinfoheader), $0);
  with bmpfileheader do begin
    bfType := $4D42; {BM}
    bfSize := headersize + INT32(dest^.row_width) * INT32(cinfo^.output_height);
    bfOffBits := headersize;
  end;
  with bmpinfoheader do begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := cinfo^.output_width;
    biHeight := cinfo^.output_height;
    biPlanes := 1;
    biBitCount := bits_per_pixel;
    if (cinfo^.density_unit = 2) then begin
      biXPelsPerMeter := INT32(cinfo^.X_density*100);
      biYPelsPerMeter := INT32(cinfo^.Y_density*100);
    end;
    biClrUsed := cmap_entries;
  end;
  if dest^.outfile.Write(bmpfileheader, SizeOf(bmpfileheader))
       <> size_t(SizeOf(bmpfileheader)) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  if dest^.outfile.Write(bmpinfoheader, SizeOf(bmpinfoheader))
       <> size_t(SizeOf(bmpinfoheader)) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  {colormap}
  if cmap_entries > 0 then begin
    num_colors := cinfo^.actual_number_of_colors;
    if cinfo^.colormap <> nil then begin
      if cinfo^.out_color_components = 3 then
        for i := 0 to pred(num_colors) do
          with output_ext_color_map[i] do begin
            b := GETJSAMPLE(cinfo^.colormap^[2]^[i]);
            g := GETJSAMPLE(cinfo^.colormap^[1]^[i]);
            r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
            a := 0;
          end
      else
        {grayscale colormap (only happens with grayscale quantization)}
        for i := 0 to pred(num_colors) do
          with output_ext_color_map[i] do begin
            b := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
            g := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
            r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
            a := 0;
          end;
      i := num_colors;
    end else begin
      {if no colormap, must be grayscale data. Generate a linear "map".}
      {Nomssi: do not use "num_colors" here, it should be 0}
      for i := 0 to pred(256) do
        with output_ext_color_map[i] do begin
          b := i;
          g := i;
          r := i;
          a := 0;
        end;
      i := 256;
    end;
    {pad colormap with zeros to ensure specified number of colormap entries}
    if i > cmap_entries then
      ERREXIT1(j_common_ptr(cinfo), JERR_TOO_MANY_COLORS, i);
    while i < cmap_entries do begin
      with output_ext_color_map[i] do begin
        b := 0;
        g := 0;
        r := 0;
        a := 0;
      end;
      Inc(i);
    end;
    if dest^.outfile.Write(output_ext_color_map, cmap_entries*4)
         <> cmap_entries*4 then
      ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  end;
  dest^.row_offset := bmpfileheader.bfSize;
end;

procedure write_bmp_pixelrow (cinfo : j_decompress_ptr;
                               dest : bmp_dest_ptr;
                      rows_supplied : JDIMENSION);
var
  image_ptr : JSAMPARRAY;
  inptr, outptr : JSAMPLE_PTR;
  BGR : BGRptr;
  col,row : JDIMENSION;
  pad : int;
begin
  if dest^.inmemory then begin
    row := dest^.cur_output_row;
    Inc(dest^.cur_output_row);
  end else begin
    row := 0;
    Dec(dest^.row_offset, dest^.row_width);
  end;
  image_ptr := cinfo^.mem^.access_virt_sarray ( j_common_ptr(cinfo),
     dest^.image_buffer, row, JDIMENSION (1), TRUE);
  inptr := JSAMPLE_PTR(dest^.buffer^[0]);
  if not dest^.grayscale then begin
    BGR := BGRptr(image_ptr^[0]);
    for col := pred(cinfo^.output_width) downto 0 do begin
      BGR^.r := inptr^;
      Inc(inptr);
      BGR^.g := inptr^;
      Inc(inptr);
      BGR^.b := inptr^;
      Inc(inptr);
      Inc(BGR);
    end;
    outptr := JSAMPLE_PTR(BGR);
  end else begin
    outptr := JSAMPLE_PTR(image_ptr^[0]);
    for col := pred(cinfo^.output_width) downto 0 do begin
      outptr^ := inptr^;
      Inc(outptr);
      Inc(inptr);
    end;
  end;
  {zero out the pad bytes}
  pad := dest^.pad_bytes;
  while (pad > 0) do begin
    Dec(pad);
    outptr^ := 0;
    Inc(outptr);
  end;
  if not dest^.inmemory then begin
    {store row in output stream}
    image_ptr := cinfo^.mem^.access_virt_sarray ( j_common_ptr(cinfo),
         dest^.image_buffer, 0, JDIMENSION(1), FALSE);
    outptr := JSAMPLE_PTR(image_ptr^[0]);
    if dest^.outfile.Seek(dest^.row_offset, 0) <> dest^.row_offset then
      ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
    if dest^.outfile.Write(outptr^, dest^.row_width) <> dest^.row_width then
      ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  end;
end;

procedure write_bmp_image (cinfo : j_decompress_ptr;
                            dest : bmp_dest_ptr);
var
  row, col  : JDIMENSION;
  image_ptr : JSAMPARRAY;
  data_ptr  : JSAMPLE_PTR;
begin
  if dest^.inmemory then {write the image data from our virtual array}
    for row := cinfo^.output_height downto 1 do begin
      image_ptr := cinfo^.mem^.access_virt_sarray( j_common_ptr(cinfo),
         dest^.image_buffer, row-1, JDIMENSION(1), FALSE);
      data_ptr := JSAMPLE_PTR(image_ptr^[0]);
      {Nomssi - This won't work for 12bit samples}
      if dest^.outfile.Write(data_ptr^, dest^.row_width) <> dest^.row_width then
        ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
    end;
end;

function jinit_write_bmp (cinfo : j_decompress_ptr;
                        outfile : TStream;
                       inmemory : boolean) : bmp_dest_ptr;
var
  dest : bmp_dest_ptr;
begin
  dest := bmp_dest_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  SIZEOF(bmp_dest_struct)) );
  dest^.outfile := outfile;
  dest^.inmemory := inmemory;
  {image info}
  jpeg_calc_output_dimensions(cinfo);
  dest^.data_width := cinfo^.output_width * cinfo^.output_components;
  dest^.row_width := dest^.data_width;
  while ((dest^.row_width and 3) <> 0) do
    Inc(dest^.row_width);
  dest^.pad_bytes := int(dest^.row_width-dest^.data_width);
  if (cinfo^.out_color_space = JCS_GRAYSCALE) then
    dest^.grayscale := True
  else if (cinfo^.out_color_space = JCS_RGB) then
    if (cinfo^.quantize_colors) then
      dest^.grayscale := True
    else
      dest^.grayscale := False
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_COLORSPACE);
  {decompress buffer}
  dest^.buffer := cinfo^.mem^.alloc_sarray
    (j_common_ptr(cinfo), JPOOL_IMAGE, dest^.row_width, JDIMENSION (1));
  dest^.buffer_height := 1;
  {image buffer}
  if inmemory then
    dest^.image_buffer_height := cinfo^.output_height
  else
    dest^.image_buffer_height := 1;
  dest^.image_buffer := cinfo^.mem^.request_virt_sarray (
     j_common_ptr(cinfo), JPOOL_IMAGE, FALSE, dest^.row_width,
     dest^.image_buffer_height, JDIMENSION (1) );
  dest^.cur_output_row := 0;
  {result}
  jinit_write_bmp := dest;
end;

{ ------------------------------------------------------------------------ }
{   Bitmap reading routines                                                }
{   for reference: RDBMP.PAS in PASJPG10 library                           }
{ ------------------------------------------------------------------------ }

type
  bmp_source_ptr = ^bmp_source_struct;
  bmp_source_struct = record
    infile : TStream;               {stream to read from}
    inmemory : boolean;             {keep whole image in memory}
    {image info}
    bits_per_pixel : INT;           {bit depth}
    colormap : JSAMPARRAY;          {BMP colormap (converted to my format)}
    row_width : JDIMENSION;         {physical width of one row in the BMP file}
    {pixelrow buffer}
    buffer : JSAMPARRAY;            {pixelrow buffer}
    buffer_height : JDIMENSION;     {normally, we'll use 1}
    {image buffer}
    image_buffer : jvirt_sarray_ptr;   {needed to reverse order BMP<>JPG}
    image_buffer_height : JDIMENSION;  {image_height}
    cur_input_row : JDIMENSION;        {current source row number}
    row_offset : INT32;             {position of next row to read from BMP}
  end;

procedure read_bmp_header (cinfo : j_compress_ptr;
                          source : bmp_source_ptr);
var
  bmpfileheader : TBitmapFileHeader;
  bmpcoreheader : TBitmapCoreHeader;
  bmpinfoheader : TBitmapInfoHeader;
  i, cmap_entrysize : INT;

  function read_byte: INT;
    {Read next byte from BMP file}
  var
    c: byte;
  begin
    if source^.infile.Read(c, 1) <> size_t(1) then
      ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
    read_byte  := c;
  end;

begin
  cmap_entrysize := 0;          { 0 indicates no colormap }

  {bitmap file header:}
  if source^.infile.Read(bmpfileheader, SizeOf(bmpfileheader))
       <> size_t(SizeOf(bmpfileheader)) then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
  if bmpfileheader.bfType <> $4D42 then {'BM'}
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_NOT);

  {bitmap infoheader: might be 12 bytes (OS/2 1.x), 40 bytes (Windows),
   or 64 bytes (OS/2 2.x).  Check the first 4 bytes to find out which}
  if source^.infile.Read(bmpinfoheader, SizeOf(INT32)) <> size_t(SizeOf(INT32)) then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
  {OS/2 1.x format}
  if bmpinfoheader.biSize = SizeOf(TBitmapCoreHeader) then begin
    bmpcoreheader.bcSize := bmpinfoheader.biSize;
    if source^.infile.Read(bmpcoreheader.bcWidth, bmpcoreheader.bcSize-SizeOf(INT32))
         <> size_t (bmpcoreheader.bcSize-SizeOf(INT32)) then
      ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
    bmpinfoheader.biWidth := bmpcoreheader.bcWidth;
    bmpinfoheader.biHeight := bmpcoreheader.bcHeight;
    bmpinfoheader.biPlanes := bmpcoreheader.bcPlanes;
    bmpinfoheader.biBitCount := bmpcoreheader.bcBitCount;
    bmpinfoheader.biClrUsed := 0;
    source^.bits_per_pixel := bmpinfoheader.biBitCount;
    case source^.bits_per_pixel of
       8: begin {colormapped image}
            cmap_entrysize := 3;  {OS/2 uses RGBTRIPLE colormap}
            TRACEMS2( j_common_ptr(cinfo), 1, JTRC_BMP_OS2_MAPPED,
              int (bmpinfoheader.biWidth), int(bmpinfoheader.biHeight));
          end;
      24: { RGB image }
          TRACEMS2( j_common_ptr(cinfo), 1, JTRC_BMP_OS2,
            int (bmpinfoheader.biWidth), int(bmpinfoheader.biHeight) );
    else
      ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADDEPTH);
    end;
    if bmpinfoheader.biPlanes <> 1 then
      ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADPLANES);
  end else
  {Windows 3.x or OS/2 2.x header, which has additional fields that we ignore }
  if (bmpinfoheader.biSize = SizeOf(TBitmapInfoHeader)) or
     (bmpinfoheader.biSize = 64) then
  begin
    if source^.infile.Read(bmpinfoheader.biWidth, SizeOf(bmpinfoheader)-SizeOf(INT32))
         <> size_t (SizeOf(bmpinfoheader)-SizeOf(INT32)) then
      ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
    if bmpinfoheader.biSize = 64 then
      source^.infile.Seek(64-SizeOf(TBitmapInfoHeader), 1);
    source^.bits_per_pixel := bmpinfoheader.biBitCount;
    case source^.bits_per_pixel of
       8: begin {colormapped image}
            cmap_entrysize := 4;        {Windows uses RGBQUAD colormap}
            TRACEMS2( j_common_ptr(cinfo), 1, JTRC_BMP_MAPPED,
              int (bmpinfoheader.biWidth), int(bmpinfoheader.biHeight) );
          end;
      24: {RGB image}
          TRACEMS2( j_common_ptr(cinfo), 1, JTRC_BMP,
            int (bmpinfoheader.biWidth), int(bmpinfoheader.biHeight) );
    else
      ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADDEPTH);
    end;
    if (bmpinfoheader.biPlanes <> 1) then
      ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADPLANES);
    if (bmpinfoheader.biCompression <> 0) then
      ERREXIT(j_common_ptr(cinfo), JERR_BMP_COMPRESSED);
    if (bmpinfoheader.biXPelsPerMeter > 0) and (bmpinfoheader.biYPelsPerMeter > 0) then
    begin
      {Set JFIF density parameters from the BMP data}
      cinfo^.X_density := bmpinfoheader.biXPelsPerMeter div 100; {100 cm per meter}
      cinfo^.Y_density := bmpinfoheader.biYPelsPerMeter div 100;
      cinfo^.density_unit := 2; { dots/cm }
    end;
  end else
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADHEADER);

  {colormap}
  if cmap_entrysize > 0 then begin
    if bmpinfoheader.biClrUsed <= 0 then
      bmpinfoheader.biClrUsed := 256 {assume it's 256}
    else
      if bmpinfoheader.biClrUsed > 256 then
        ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADCMAP);
    {allocate colormap}
    source^.colormap := cinfo^.mem^.alloc_sarray( j_common_ptr (cinfo),
      JPOOL_IMAGE, JDIMENSION(bmpinfoheader.biClrUsed), JDIMENSION (3));
    {read it}
    case cmap_entrysize of
      3: {BGR format (occurs in OS/2 files)}
        for i := 0 to pred(bmpinfoheader.biClrUsed) do begin
          source^.colormap^[2]^[i] := JSAMPLE (read_byte);
          source^.colormap^[1]^[i] := JSAMPLE (read_byte);
          source^.colormap^[0]^[i] := JSAMPLE (read_byte);
        end;
      4: {BGR0 format (occurs in MS Windows files)}
        for i := 0 to pred(bmpinfoheader.biClrUsed) do begin
          source^.colormap^[2]^[i] := JSAMPLE (read_byte);
          source^.colormap^[1]^[i] := JSAMPLE (read_byte);
          source^.colormap^[0]^[i] := JSAMPLE (read_byte);
          read_byte;
        end;
    else
      ERREXIT(j_common_ptr(cinfo), JERR_BMP_BADCMAP);
    end;
  end;

  {initialize bmp_source_struc}

  {row width, including padding to 4-byte boundary}
  if source^.bits_per_pixel = 24 then
    source^.row_width := JDIMENSION(bmpinfoheader.biWidth*3)
  else
    source^.row_width := JDIMENSION (bmpinfoheader.biWidth);
  while ((source^.row_width and 3) <> 0) do
    Inc(source^.row_width);

  {allocate pixelrow buffer}
  source^.buffer := cinfo^.mem^.alloc_sarray( j_common_ptr (cinfo),
    JPOOL_IMAGE, JDIMENSION (bmpinfoheader.biWidth*3), JDIMENSION (1) );
  source^.buffer_height := 1;

  {allocate image buffer}
  if source^.inmemory then begin
    source^.image_buffer_height := bmpinfoheader.biHeight;
    source^.cur_input_row := bmpinfoheader.biHeight;
  end else begin
    source^.image_buffer_height := 1;
    source^.row_offset := bmpfileheader.bfSize;
  end;
  source^.image_buffer := cinfo^.mem^.request_virt_sarray (
    j_common_ptr (cinfo), JPOOL_IMAGE, FALSE, source^.row_width,
     JDIMENSION(source^.image_buffer_height), JDIMENSION (1) );

  {set decompress parameters}
  cinfo^.in_color_space := JCS_RGB;
  cinfo^.input_components := 3;
  cinfo^.data_precision := 8;
  cinfo^.image_width := JDIMENSION (bmpinfoheader.biWidth);
  cinfo^.image_height := JDIMENSION (bmpinfoheader.biHeight);
end;

function read_bmp_pixelrow (cinfo : j_compress_ptr;
                           source : bmp_source_ptr) : JDIMENSION;
  { Read one row of pixels:
    the image has been read into the image_buffer array, but is otherwise
    unprocessed.  we must read it out in top-to-bottom row order, and if
    it is an 8-bit image, we must expand colormapped pixels to 24bit format. }
var
  col, row : JDIMENSION;
  image_ptr : JSAMPARRAY;
  inptr, outptr : JSAMPLE_PTR;
  outptr24 : JSAMPROW;
  t : INT;
begin
  if source^.inmemory then begin
    Dec(source^.cur_input_row);
    row := source^.cur_input_row;
  end else begin
    Dec(source^.row_offset, source^.row_width);
    row := 0;
  end;
  if not source^.inmemory then begin
    image_ptr := cinfo^.mem^.access_virt_sarray ( j_common_ptr (cinfo),
       source^.image_buffer, row, JDIMENSION (1), TRUE);
    inptr := JSAMPLE_PTR(image_ptr^[0]);
    if source^.infile.Seek(source^.row_offset, 0) <> source^.row_offset then
      ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
    if source^.infile.Read(inptr^, source^.row_width)
         <> size_t(source^.row_width) then
      ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
  end;
  image_ptr := cinfo^.mem^.access_virt_sarray ( j_common_ptr (cinfo),
    source^.image_buffer, row, JDIMENSION (1), FALSE);
  {}
  inptr := JSAMPLE_PTR(image_ptr^[0]);
  case source^.bits_per_pixel of
     8: begin
          {expand the colormap indexes to real data}
          outptr := JSAMPLE_PTR(source^.buffer^[0]);
          for col := pred(cinfo^.image_width) downto 0 do begin
            t := GETJSAMPLE(inptr^);
            Inc(inptr);
            outptr^ := source^.colormap^[0]^[t];
            Inc(outptr);
            outptr^ := source^.colormap^[1]^[t];
            Inc(outptr);
            outptr^ := source^.colormap^[2]^[t];
            Inc(outptr);
          end;
        end;
    24: begin
          outptr24 := source^.buffer^[0];
          for col := pred(cinfo^.image_width) downto 0 do begin
            outptr24^[2] := inptr^;
            Inc(inptr);
            outptr24^[1] := inptr^;
            Inc(inptr);
            outptr24^[0] := inptr^;
            Inc(inptr);
            Inc(JSAMPLE_PTR(outptr24), 3);
          end;
        end;
  end;
  read_bmp_pixelrow := 1;
end;

procedure read_bmp_image(cinfo : j_compress_ptr;
                        source : bmp_source_ptr);
var
  row, col : JDIMENSION;
  image_ptr : JSAMPARRAY;
  inptr : JSAMPLE_PTR;
begin
  if source^.inmemory then
    for row := 0 to pred(cinfo^.image_height) do begin
      image_ptr := cinfo^.mem^.access_virt_sarray ( j_common_ptr (cinfo),
         source^.image_buffer, row, JDIMENSION (1), TRUE);
      inptr := JSAMPLE_PTR(image_ptr^[0]);
      if source^.infile.Read(inptr^, source^.row_width)
           <> size_t(source^.row_width)
      then
        ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EOF);
    end;
end;

function jinit_read_bmp (cinfo : j_compress_ptr;
                        infile : TStream;
                      inmemory : boolean) : bmp_source_ptr;
var
  source : bmp_source_ptr;
begin
  source := bmp_source_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                               SIZEOF(bmp_source_struct)) );
  source^.infile := infile;
  source^.inmemory := inmemory;
  jinit_read_bmp := source;
end;

{ ------------------------------------------------------------------------ }
{   JPEG progress monitor support                                          }
{   for reference: LIPJPEG.DOC in \JPEG\C directory                        }
{ ------------------------------------------------------------------------ }

type
  my_progress_ptr = ^my_progress_mgr;
  my_progress_mgr = record
    pub : jpeg_progress_mgr;
    proc : JPEG_ProgressMonitor;
    percent_done : INT;
    completed_extra_passes : INT;
    total_extra_passes : INT;
  end;

procedure progress_monitor(cinfo: j_common_ptr); far;
var
  progress : my_progress_ptr;
  total_passes : INT;
  percent_done : INT;
begin
  progress := my_progress_ptr(cinfo^.progress);
  total_passes :=
    progress^.pub.total_passes + progress^.total_extra_passes;
  percent_done :=
    ( ((progress^.pub.completed_passes+progress^.completed_extra_passes)*100) +
      ((progress^.pub.pass_counter*100) div progress^.pub.pass_limit)
    ) div total_passes;
  {}
  if percent_done <> progress^.percent_done then begin
    progress^.percent_done := percent_done;
    progress^.proc(percent_done);
  end;
end;

procedure jpeg_my_progress(cinfo : j_common_ptr;
                        progress : my_progress_ptr;
                        callback : JPEG_ProgressMonitor);
begin
  if @callback = nil then
    Exit;
  {set method}
  progress^.pub.progress_monitor := progress_monitor;
  {set fields}
  progress^.proc := callback;
  progress^.percent_done := -1;
  progress^.completed_extra_passes := 0;
  progress^.total_extra_passes := 0;
  {link to cinfo}
  cinfo^.progress := @progress^.pub;
end;

procedure jpeg_finish_progress(cinfo : j_common_ptr);
var
  progress : my_progress_ptr;
begin
  progress := my_progress_ptr(cinfo^.progress);
  if progress^.percent_done <> 100 then begin
    progress^.percent_done := 100;
    progress^.proc(progress^.percent_done);
  end;
end;

{ ------------------------------------------------------------------------ }
{   JPEG error handler                                                     }
{   for reference: JERROR.PAS in PASJPG10 library                          }
{                  LIPJPEG.DOC in \JPEG\C directory                        }
{   NOTE: we have replaced jpeg_std_error because it stores a static       }
{         message table (JDEFERR.PAS) in the jpeg_message_table field.     }
{ ------------------------------------------------------------------------ }

type
  my_error_ptr = ^my_error_mgr;
  my_error_mgr = record
    pub: jpeg_error_mgr;
  end;

procedure error_exit (cinfo : j_common_ptr); far;
var
  buffer : string;
begin
  cinfo^.err^.format_message(cinfo, buffer);
  raise EJPEG.Create(buffer);
end;

procedure emit_message (cinfo : j_common_ptr; msg_level : int); far;
var
  err : jpeg_error_mgr_ptr;
begin
  err := cinfo^.err;
  if (msg_level < 0) then begin
    {It's a warning message. Since corrupt files may generate many warnings,}
    {the policy implemented here is to show only the first warning,}
    {unless trace_level >= 3}
    if (err^.num_warnings = 0) or (err^.trace_level >= 3) then
      err^.output_message(cinfo);
    {Always count warnings in num_warnings}
    Inc( err^.num_warnings );
  end else
    {It's a trace message. Show it if trace_level >= msg_level}
    if (err^.trace_level >= msg_level) then
      err^.output_message (cinfo);
end;

procedure output_message (cinfo : j_common_ptr); far;
var
  buffer : string;
begin
  cinfo^.err^.format_message (cinfo, buffer);
  {message dialog}
  ShowMessage(buffer);
end;

procedure format_message (cinfo : j_common_ptr; var buffer : string); far;
begin
  buffer :=
    'JPEG ERROR -- #' + IntToStr(cinfo^.err^.msg_code);
end;

procedure reset_error_mgr (cinfo : j_common_ptr); far;
begin
  cinfo^.err^.num_warnings := 0;
  {trace_level is not reset since it is an application-supplied parameter}
  cinfo^.err^.msg_code := 0;      {may be useful as a flag for "no error"}
end;

function jpeg_my_error (var err : my_error_mgr) : jpeg_error_mgr_ptr;
begin
  {methods}
  err.pub.error_exit := error_exit;
  err.pub.emit_message := emit_message;
  err.pub.output_message := output_message;
  err.pub.format_message := format_message;
  err.pub.reset_error_mgr := reset_error_mgr;
  {fields}
  err.pub.trace_level := 0;         {default := no tracing}
  err.pub.num_warnings := 0;        {no warnings emitted yet}
  err.pub.msg_code := 0;            {may be useful as a flag for "no error"}
  {message table(s)}
  err.pub.jpeg_message_table := nil;    {we don't want to use a static table}
  err.pub.last_jpeg_message := pred(JMSG_LASTMSGCODE);
  err.pub.addon_message_table := nil;
  err.pub.first_addon_message := JMSG_NOMESSAGE;   {for safety}
  err.pub.last_addon_message := JMSG_NOMESSAGE;
  {return result}
  jpeg_my_error := @err;
end;

{ ------------------------------------------------------------------------ }
{   load JPEG stream and save as BITMAP stream                             }
{   for reference: DJPEG.PAS in PASJPG10 library                           }
{ ------------------------------------------------------------------------ }

procedure LoadJPEG(const infile, outfile: TStream; inmemory: boolean;
                   {decompression parameters:}
                   numcolors: integer;
                   {progress monitor}
                   callback: JPEG_ProgressMonitor);
var
  cinfo : jpeg_decompress_struct;
  err   : my_error_mgr;
  dest  : bmp_dest_ptr;
  progress : my_progress_mgr;
  num_scanlines : JDIMENSION;
begin
  {initialize the JPEG decompression object with default error handling.}
  cinfo.err := jpeg_my_error(err);
  jpeg_create_decompress(@cinfo);
  try
    {specify the source of the compressed data}
      jpeg_stream_src(@cinfo, infile);
    {progress monitor}
      jpeg_my_progress(@cinfo, @progress, callback);
    {obtain image info from header, set default decompression parameters}
      jpeg_read_header(@cinfo, TRUE);
    {set parameters for decompression}
      if numcolors <> 0 then begin
        cinfo.desired_number_of_colors := numcolors;
        cinfo.quantize_colors := True;
      end;
      {...}
    {prepare for decompression, initialize internal state}
      dest := jinit_write_bmp(@cinfo, outfile, inmemory);
      jpeg_start_decompress(@cinfo);
    {process data}
      write_bmp_header(@cinfo, dest);
      while (cinfo.output_scanline < cinfo.output_height) do begin
        num_scanlines :=
          jpeg_read_scanlines(@cinfo, dest^.buffer, dest^.buffer_height);
        write_bmp_pixelrow(@cinfo, dest, num_scanlines);
      end;
      write_bmp_image(@cinfo, dest);
    {finish}
      jpeg_finish_decompress(@cinfo);
      jpeg_finish_progress(@cinfo);
  finally
    {destroy}
    jpeg_destroy_decompress(@cinfo);
  end;
end;

{ ------------------------------------------------------------------------ }
{   read BITMAP stream and save as JPEG                                    }
{   for reference: CJPEG.PAS in PASJPG10 library                           }
{ ------------------------------------------------------------------------ }

procedure StoreJPEG(const infile, outfile: TStream; inmemory: boolean;
                    {compression parameters:}
                    quality: INT;
                    {progress monitor}
                    callback: JPEG_ProgressMonitor);
var
  cinfo  : jpeg_compress_struct;
  err    : my_error_mgr;
  source : bmp_source_ptr;
  progress : my_progress_mgr;
  num_scanlines : JDIMENSION;
begin
  {initialize the JPEG compression object with default error handling.}
  cinfo.err := jpeg_my_error(err);
  jpeg_create_compress(@cinfo);
  try
    {specify the destination for the compressed data}
      jpeg_stream_dest(@cinfo, outfile);
    {set jpeg defaults}
      cinfo.in_color_space := JCS_RGB; {arbitrary guess}
      jpeg_set_defaults(@cinfo);
    {progress monitor}
      jpeg_my_progress(@cinfo, @progress, callback);
    {obtain image info from bitmap header, set default compression parameters}
      source := jinit_read_bmp(@cinfo, infile, inmemory);
      read_bmp_header(@cinfo, source);
    {now we know input colorspace, fix colorspace-dependent defaults}
      jpeg_default_colorspace(@cinfo);
    {set parameters for compression (most likely only quality)}
      jpeg_set_quality(@cinfo, quality, TRUE);
      {...}
    {prepare for compression, initialize internal state}
      jpeg_start_compress(@cinfo, TRUE);
    {process data}
      read_bmp_image(@cinfo, source);
      while (cinfo.next_scanline < cinfo.image_height) do begin
        num_scanlines := read_bmp_pixelrow(@cinfo, source);
        jpeg_write_scanlines(@cinfo, source^.buffer, num_scanlines);
      end;
    {finish}
      jpeg_finish_compress(@cinfo);
      jpeg_finish_progress(@cinfo);
  finally
    {destroy}
    jpeg_destroy_compress(@cinfo);
  end;
end;

end.
