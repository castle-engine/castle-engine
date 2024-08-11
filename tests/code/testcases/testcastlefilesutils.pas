// -*- compile-command: "./test_single_testcase.sh TTestCastleFilesUtils" -*-
{
  Copyright 2007-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test OS-specific utilities mostly in CastleFilesUtils unit. }
unit TestCastleFilesUtils;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleFilesUtils = class(TCastleTestCase)
  published
    procedure TestPathDelim;
    procedure TestExeName;
    procedure TestTimer;
    {$ifdef UNIX} procedure TestHomePath; {$endif}
    procedure TestGetTempDir;
    procedure TestApplicationData;
    procedure TestCombinePaths;
    { Test CombinePaths with BasePath relative.
      CGE editor "new unit" depends on it when it does
        FinalUnitRelative := CombinePaths(EditUnitDir.Text, LowerCase(EditUnitName.Text) + '.pas');
        FinalDesignRelative := CombinePaths(EditDesignDir.Text, LowerCase(EditUnitName.Text) + '.castle-user-interface')
    }
    procedure TestCombinePathsRelative;
  end;

implementation

uses CastleUtils, CastleFindFiles, CastleFilesUtils, CastleTimeUtils
  {$ifdef UNIX}, BaseUnix {$endif}{$ifndef FPC}, IOUtils{$endif};

procedure TTestCastleFilesUtils.TestPathDelim;
begin
  { slash should be added / stripped on both Windows and Unix }

  AssertEquals('/tmp', ExclPathDelim('/tmp/'));
  AssertEquals('', ExclPathDelim('/'));
  AssertEquals('/tmp', ExclPathDelim('/tmp'));
  AssertEquals('', ExclPathDelim(''));

  AssertEquals('/tmp/', InclPathDelim('/tmp/'));
  AssertEquals('/', InclPathDelim('/'));
  AssertEquals('/tmp' + PathDelim, InclPathDelim('/tmp'));
  AssertEquals(PathDelim, InclPathDelim(''));

  { backslash should be likewise added / stripped on Windows }
  {$ifdef MSWINDOWS}
  AssertEquals('\tmp', ExclPathDelim('\tmp\'));
  AssertEquals('', ExclPathDelim('\'));
  AssertEquals('\tmp', ExclPathDelim('\tmp'));
  AssertEquals('', ExclPathDelim(''));

  AssertEquals('\tmp\', InclPathDelim('\tmp\'));
  AssertEquals('\', InclPathDelim('\'));
  AssertEquals('\tmp\', InclPathDelim('\tmp'));
  AssertEquals('\', InclPathDelim(''));
  {$endif}

  { backslash should not be treated like dir separator on Unix }
  {$ifdef UNIX}
  AssertEquals('\tmp\', ExclPathDelim('\tmp\'));
  AssertEquals('\', ExclPathDelim('\'));
  AssertEquals('\tmp', ExclPathDelim('\tmp'));
  AssertEquals('', ExclPathDelim(''));

  AssertEquals('\tmp\/', InclPathDelim('\tmp\'));
  AssertEquals('\/', InclPathDelim('\'));
  AssertEquals('\tmp/', InclPathDelim('\tmp'));
  AssertEquals('/', InclPathDelim(''));
  {$endif}
end;

procedure TTestCastleFilesUtils.TestExeName;
begin
  try
    {$warnings off} // knowingly using deprecated below, for test
    ExeName;
    {$warnings on}
  except
    on E: EExeNameNotAvailable do
    begin
//      Writeln('ExeName not available, allowed on non-Windows:' +nl+ ExceptMessage(E));
      { This is an error under Windows, where ExeName is guaranteed to be available }
      {$ifdef MSWINDOWS} raise; {$endif}
    end;
  end;
end;

procedure TTestCastleFilesUtils.TestTimer;
{var
  TimerStart: TTimerResult;}
begin
{  Writeln('Waiting 5 seconds...');

  ProcessTimerBegin;
  TimerStart := Timer;
  Sleep(5000);
  Writeln( Format(
    'Sleep(5 seconds) done,' +nl+
    'ProcessTimer reports that %f seconds elapsed,'+nl+
    'Timer reports that %f seconds elapsed.',
    [ ProcessTimerEnd, TimerSeconds(Timer, TimerStart) ])); }
end;

{$ifdef UNIX}
procedure TTestCastleFilesUtils.TestHomePath;
begin
{  Writeln('HomePath is ', HomePath);
  Writeln('  ~/expanded is ', ExpandHomePath('~/expanded')); }
  AssertTrue(ExpandHomePath('/bin/') = '/bin/');
  AssertTrue(ExpandHomePath('~') = ExclPathDelim(HomePath));
  AssertTrue(ExpandHomePath('~/') = InclPathDelim(HomePath));
end;
{$endif}

procedure TTestCastleFilesUtils.TestGetTempDir;
begin
//  Writeln('TempDir: ', GetTempDir);
  {$ifdef FPC}
  GetTempDir; // ignore result, just make sure it doesn't raise errors
  {$else}
  TPath.GetTempPath;
  {$endif}
end;

procedure TTestCastleFilesUtils.TestApplicationData;
begin
  AssertEquals('castle-data:/', ApplicationData(''));
  AssertEquals('castle-data:/xxx/yyy', ApplicationData('xxx/yyy'));
end;

procedure TTestCastleFilesUtils.TestCombinePaths;
begin
  {$ifdef UNIX}
  AssertEquals('/a/b', CombinePaths('/a', 'b'));
  AssertEquals('/a/b', CombinePaths('/a/', 'b'));
  AssertEquals('/b', CombinePaths('/a/', '/b'));

  AssertEquals('/a/b/c', CombinePaths('/a/b', 'c'));
  AssertEquals('/a/b/c', CombinePaths('/a', 'b/c'));
  AssertEquals('/a/b/c', CombinePaths('/a', './b/c'));

  // check ../ handling
  AssertEquals('/b/c', CombinePaths('/a', '../b/c'));
  {$endif}

  {$ifdef MSWINDOWS}
  AssertEquals('c:\a\b', CombinePaths('c:\a', 'b'));
  AssertEquals('c:\a\b', CombinePaths('c:\a\', 'b'));
  AssertEquals('c:\b', CombinePaths('c:\a\', 'c:\b'));

  AssertEquals('c:/a\b', CombinePaths('c:/a', 'b'));
  AssertEquals('c:/a/b', CombinePaths('c:/a/', 'b'));
  AssertEquals('c:/b', CombinePaths('c:/a/', 'c:/b'));
  {$endif}
end;

procedure TTestCastleFilesUtils.TestCombinePathsRelative;
begin
  {$ifdef UNIX}
  AssertEquals('a/b', CombinePaths('a', 'b'));
  AssertEquals('a/b/c', CombinePaths('a/b', 'c'));
  AssertEquals('c', CombinePaths('', 'c'));
  {$endif}

  {$ifdef MSWINDOWS}
  AssertEquals('a\b', CombinePaths('a', 'b'));
  AssertEquals('a/b\c', CombinePaths('a/b', 'c'));
  AssertEquals('c', CombinePaths('', 'c'));

  // check special case when RelPath is absolute on drive, but without drive letter
  AssertEquals('x:/foo', CombinePaths('x:/bar', '/foo'));
  AssertEquals('x:\foo', CombinePaths('x:\bar', '\foo'));
  {$endif}
end;

initialization
  RegisterTest(TTestCastleFilesUtils);
end.
