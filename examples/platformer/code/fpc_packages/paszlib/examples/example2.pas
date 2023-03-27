program EnhancedZipperExample;

{$mode objfpc}{$H+}

uses
  Classes, zstream, zipper;

var
   z: TZipper;
   zfe: TZipFileEntry;
begin
  z:=TZipper.Create;
  z.FileName:='fpcCompressionLevelTestFile.zip';
  try
    //Default Compression Level
    zfe:=z.Entries.AddFileEntry(ParamStr(0));
    //Compression Level = none ( Store )
    zfe:=z.Entries.AddFileEntry(ParamStr(0));
    zfe.CompressionLevel:=clnone;
    z.ZipAllFiles;
  finally
    z.Free;
  end;
  {
   The result can be checked with the command(On Linux):
   unzip -v fpcCompressionLevelTestFile.zip
   The column Method Shows different values to each file
  }
end.




