{ This is a hacked version of XMLReporter, to report test results
  in simple plain text. It's supposed to be used with
  ConsoleTestRunner when the resulting output should be readable
  by human (if it should be readable by programs, than XMLReporter
  is a better idea).

  XMLReporter copyright:

  @preformatted(
  Copyright (C) 2006 Graeme Geldenhuys <graemeg@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
  )
}

unit PlainReporter;

{$mode objfpc}{$H+}

interface
uses
  Classes
  ,SysUtils
  ,fpcUnit
  ,TestUtils
  ;

type
  TPlainResultsWriter = class(TNoRefCountObject, ITestListener)
  public
    constructor Create;
    destructor  Destroy; override;

    procedure   WriteResult(pTestResult: TTestResult);
  public
    Verbose: boolean;

    { ITestListener interface requirements }
    procedure   AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure   AddError(ATest: TTest; AError: TTestFailure);
    procedure   StartTest(ATest: TTest);
    procedure   EndTest(ATest: TTest);   
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  end;

implementation


{ TPlainResultsWriter }

procedure TPlainResultsWriter.WriteResult(pTestResult: TTestResult);
begin
  Writeln(Format('Result: %d runned, %d errors, %d failures',
    [ pTestResult.RunTests, pTestResult.NumberOfErrors,
      pTestResult.NumberOfFailures ]));
end;

constructor TPlainResultsWriter.Create;
begin
  Writeln('Generated using FPCUnit on ' +
    FormatDateTime('yyyy-mm-dd hh:mm ', Now));
  //Verbose := true;
end;

destructor TPlainResultsWriter.Destroy;
begin
  inherited Destroy;
end;

procedure TPlainResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  Writeln('Failure:');
  Writeln('  Message: ' + AFailure.AsString);
  Writeln('  ExceptionClass: ' + AFailure.ExceptionClassName);
  Writeln('  ExceptionMessage: ' + AFailure.ExceptionMessage);
end;

procedure TPlainResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  Writeln('Error:');
  Writeln('  Message: ' + AError.AsString);
  Writeln('  ExceptionClass: ' + AError.ExceptionClassName);
  Writeln('  ExceptionMessage: ' + AError.ExceptionMessage);
  Writeln('  SourceUnitName: ' + AError.SourceUnitName);
  Writeln('  LineNumber: ', AError.LineNumber);
  Writeln('  FailedMethodName: ' + AError.FailedMethodName);
end;

procedure TPlainResultsWriter.StartTest(ATest: TTest);
begin
  if Verbose then
    Writeln(ATest.TestSuiteName + '.' + ATest.TestName + ': Start');
end;

procedure TPlainResultsWriter.EndTest(ATest: TTest);
begin
  if Verbose then
    Writeln(ATest.TestSuiteName + '.' + ATest.TestName + ': End');
end;

procedure TPlainResultsWriter.StartTestSuite(ATestSuite: TTestSuite);
begin
end;

procedure TPlainResultsWriter.EndTestSuite(ATestSuite: TTestSuite);
begin
end;

end.