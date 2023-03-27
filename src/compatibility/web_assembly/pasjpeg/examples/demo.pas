Program Demo;
{ for Delphi3 // same name as project }
{ Test program - This program may hang your machine !! }
uses
  test, example;
var
  fname : string;
begin
  WriteLn('PASJPEG Demo');

  define_image_params;
  write_JPEG_file ('PasJpeg.jpg', 75);
  WriteLn('JPEG encoding OK.');

  if ParamCount = 0 then
  begin
    Write('JFIF file name :');
    ReadLn(fname);
  end
  else
  begin
    fname := ParamStr(1);
    WriteLn('JFIF file name :', fname);
  end;

  pre_decode;

  if not read_JPEG_file (fname) then
  begin
    WriteLn('JPEG decoding error : ', fname);
    Halt(1);
  end;
  post_decode;

  WriteLn('JPEG decoding OK.');
end.
