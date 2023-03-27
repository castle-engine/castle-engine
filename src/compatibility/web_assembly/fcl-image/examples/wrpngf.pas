uses
  sysutils, fpreadqoi, fpimage, fpwritePNG;

var
  image: TFPCustomImage;
  reader: TFPCustomImageReader;
  writer: TFPWriterPNG;
  AFileName : string;

  useAlpha : boolean;
begin
  AFileName:=paramstr(1);
  Image := TFPMemoryImage.Create(0, 0);
  Reader := TFPReaderQoi.Create;
  Writer := TFPWriterPNG.Create;
  Image.LoadFromFile(AFileName, Reader);

  UseAlpha := TFPReaderQoi(Reader).UseAlpha;
  Writer.UseAlpha:=UseAlpha;


  Image.SaveToFile(ChangeFileExt(aFileName,'.png'),Writer);

  Writeln;
  writeln(' Width ',Image.Width,'  Height ', Image.Height, '  UseAlpha ',UseAlpha);
  Writeln;

  image.Free;
  Reader.Free;
  Writer.Free;

end.
