uses
  sysutils, fpreadpng, fpimage, fpwriteQoi;

var
  image: TFPCustomImage;
  reader: TFPCustomImageReader;
  writer: TFPWriterQoi;
  AFileName : string;

  useAlpha : boolean;
begin
  AFileName:=paramstr(1);

  Image := TFPMemoryImage.Create(0, 0);
  Reader := TFPReaderPNG.Create;
  Writer := TFPWriterQoi.Create;
  Image.LoadFromFile(AFileName, Reader);

  UseAlpha := TFPReaderPNG(Reader).UseAlpha;
  Writer.UseAlpha:=UseAlpha;


  Image.SaveToFile(ChangeFileExt(aFileName,'.qoi'),Writer);

  Writeln;
  writeln(' BitDepth ',TFPReaderPNG(Reader).BitDepth );
  writeln(' ColorType ',TFPReaderPNG(Reader).ColorType );
  writeln(' Width ',Image.Width,'  Height ', Image.Height, '  UseAlpha ',UseAlpha);
  Writeln;


  image.Free;
  Reader.Free;
  Writer.Free;

end.
