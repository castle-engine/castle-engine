{
  Copyright 2013-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple extension of standard ConsoleTestRunner to set program ExitCode
  to non-zero when some test failed. Useful if you run the test suite from
  a script (like cron) and you want to automatically get notified when
  something failed. }
unit CastleConsoleTestRunner;

interface

uses FPCUnit, FPCUnitReport, ConsoleTestRunner, PlainTestReport;

type
  TCastleConsoleTestRunner = class(TTestRunner)
  protected
    function GetResultsWriter: TCustomResultsWriter; override;
  end;

  TCastlePlainResultsWriter = class(TPlainResultsWriter)
  protected
    procedure WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer;
      ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer;
      ANumFailures: integer; ANumIgnores: integer); override;
  end;

implementation

function TCastleConsoleTestRunner.GetResultsWriter: TCustomResultsWriter;
begin
  if FormatParam = fPlain then
    Result := TCastlePlainResultsWriter.Create(nil) else
    Result := inherited;
end;

procedure TCastlePlainResultsWriter.WriteSuiteFooter(ATestSuite: TTestSuite;
  ALevel: integer;
  ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer;
  ANumFailures: integer; ANumIgnores: integer);
begin
  inherited;
  if (ANumErrors <> 0) or (ANumFailures <> 0) then
    ExitCode := 1;
end;

end.
