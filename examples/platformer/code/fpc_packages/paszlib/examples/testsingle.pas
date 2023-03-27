program testsingle;

uses sysutils, zipper;

Var
  FN : String;

begin
  FN:=GetTempFileName;
  TUnzipper.Unzip('test.zip','files/file1.txt',FN);
  if not FileExists(FN) then
    Writeln('Error')
  else
    begin
    DeleteFile(FN);
    Writeln('OK');
    end;
end.

