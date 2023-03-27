program testfpimage;

{$mode objfpc}{$H+}

uses
  sysutils,Classes, consoletestrunner, tcbarcodes, fpbarcode;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;
begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=true;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
