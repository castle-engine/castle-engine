Unit Test;

interface

uses
  jmorecfg, jpeglib;

const
  MaxWidth = 175;
  MaxLines = 4;
type
  RGB_pixel = packed record
             Case byte of
             0:(r,g,b : byte);
             1:(color:array[0..2] of byte);
             2:(cyan,magenta,yellow : byte);
             3:(Y,Cb,Cr : byte);
           end;
var
  image_line : array[0..MaxLines-1,0..MaxWidth-1] of RGB_pixel;
var
  image_buffer : JSAMPROW;      { Points to large array of R,G,B-order data }
  image_height : int;           { Number of rows in image }
  image_width : int;            { Number of columns in image }
var
  current_line : int;
type
  jmp_buf = pointer;

  { This routine does the output }
  procedure put_scanline_someplace(buffer : JSAMPROW; row_stride : int);

  { define an error recovery point. Return 0 when OK }
  function setjmp(setjmp_buffer : jmp_buf) : int;

  { Return control to the setjmp point }
  procedure longjmp(setjmp_buffer : jmp_buf; flag : int);

  procedure save_color_map(cinfo : j_decompress_ptr);

  procedure define_image_params;

  procedure  pre_decode;

  procedure  post_decode;

implementation

var
  outfile : file;

{ This routine does the output }
procedure put_scanline_someplace(buffer : JSAMPROW; row_stride : int);
var
  line_size : int;
begin
  WriteLn(output, current_line:3, '. line of image data read');
  line_size := 3 * MaxWidth;

  BlockWrite(outfile, buffer^, row_stride);

  if line_size > row_stride then
    line_size := row_stride;

  if current_line < MaxLines then
    Move(buffer^, image_line[current_line], line_size);
  Inc(current_line);
end;

{ define an error recovery point. Return 0 when OK }
function setjmp(setjmp_buffer : jmp_buf) : int;
begin
  setjmp := 0;
  current_line := 0;
end;

{ Return control to the setjmp point }
procedure longjmp(setjmp_buffer : jmp_buf; flag : int);
begin
  Halt(2);
end;

procedure define_image_params;
var
  i, j : JDIMENSION;
  r0, b0, g0 : byte;
begin
  r0 := 255;
  g0 := 255;
  b0 := 255;
  for j := 0 to pred(MaxLines) do
  begin
    for i := 0 to Pred(MaxWidth) do
    with image_line[j][i] do
    begin
      r := r0;
      Dec(r0);
      g := g0;
      b := b0;
    end;
    Dec(b0, 16);
  end;
  image_buffer := JSAMPROW(@image_line);
  image_height := MaxLines;
  image_width := MaxWidth;
end;


procedure pre_decode;
begin
  Assign(outfile, 'PasJpeg.raw');
  ReWrite(outfile, 1);
end;

procedure save_color_map(cinfo : j_decompress_ptr);
var
  VGAPalette : Array[0..255] of RGB_pixel;
  i, count : int;
begin
  count := cinfo^.actual_number_of_colors;
  if (cinfo^.colormap <> NIL) and (count > 0) then
  begin
    if count > 256 then
      count := 256;
    if (cinfo^.out_color_components = 3) then
      for i := 0 to pred(count) do
      begin
        VGAPalette[i].r := cinfo^.colormap^[0]^[i];
        VGAPalette[i].g := cinfo^.colormap^[1]^[i];
        VGAPalette[i].b := cinfo^.colormap^[2]^[i];
      end
    else { Grayscale colormap (only happens with grayscale quantization) }
      for i := 0 to pred(count) do
      begin
        VGAPalette[i].r := cinfo^.colormap^[0]^[i];
        VGAPalette[i].g := cinfo^.colormap^[0]^[i];
        VGAPalette[i].b := cinfo^.colormap^[0]^[i];
      end;
    BlockWrite(outfile, VGAPalette, 3*count);
  end;
end;

procedure  post_decode;
begin
  Close(outfile);
end;

end.
