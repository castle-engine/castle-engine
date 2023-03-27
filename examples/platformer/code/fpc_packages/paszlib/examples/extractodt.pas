program ExtractODT;
// minimal function test zipper unit (related to mantis 15836)

{$mode objfpc}{$H+}

Uses Zipper,sysutils;

procedure usage;
begin
  Writeln('ExtractOdt <filename>');
  halt;
end;

var unzipper : TUnzipper;
    EDir,
    FileName : string;
begin
  if paramcount<1 then
    Usage;
  FileName:=paramstr(1);
  if not fileexists(FileName) then
    Usage;
  edir:=extractfilename(filename)+'.extractiondir';
  mkdir(edir);
  unzipper:=TUnzipper.create;
  unzipper.FileName:=FileName;
  unzipper.outputpath:=edir;
  unzipper.UnzipAllFiles;
 
end.