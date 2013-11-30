{
  Copyright 2007-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test some OS-specific utilities in castle_game_engine,
  mostly in CastleUtils unit.
  Usually the mere purpose of these utilities is to hide some OS-specific
  (UNIX-specific, Windows-specific) things from program. }

unit TestOSSpecific;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestOSSpecific = class(TTestCase)
  private
    SymlinkName, SymlinkFullName, SymlinkTarget: string;
  published
    procedure TestPathDelim;
    procedure TestChangeDir;
    {$ifdef UNIX} procedure TestIsSymLink; {$endif}
    procedure TestExeName;
    procedure TestTimer;
    {$ifdef UNIX} procedure TestHomePath; {$endif}
    procedure TestGetTempDir;
  end;

implementation

uses CastleUtils, CastleEnumerateFiles, CastleFilesUtils, CastleTimeUtils {$ifdef UNIX}, BaseUnix {$endif};

procedure TTestOSSpecific.TestPathDelim;
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

procedure TTestOSSpecific.TestChangeDir;
var
  PreviousDir: string;
const
  ExistingDir = {$ifdef UNIX} '/' {$endif}
                {$ifdef MSWINDOWS} 'c:\' {$endif};
  NotExistingDir = {$ifdef UNIX} '/not_existing_directory_castle_test' {$endif}
                   {$ifdef MSWINDOWS} 'c:\not_existing_directory_castle_test' {$endif};
begin
  PreviousDir := GetCurrentDir;
  try
    ChangeDir(ExistingDir);
    Assert(InclPathDelim(GetCurrentDir) = ExistingDir);

    try
      ChangeDir(NotExistingDir);
      Assert(false, 'ChangeDir to ' + NotExistingDir + ' didn''t raise an exception');
    except
      on E: EInOutError do ;
    end;
  finally
    { restoring dir is necessary for other tests to work, some load
      files from data/ subdir. }
    ChangeDir(PreviousDir);
  end;
end;

{$ifdef UNIX}
procedure TestIsSymLink_Proc(const FileInfo: TEnumeratedFileInfo; Data: Pointer);
begin
  with TTestOSSpecific(Data) do
  begin
    Assert(ExtractFileName(FileInfo.AbsoluteName) = SymlinkName);
    Assert(IsSymLink(FileInfo.AbsoluteName));
    Assert(CastleReadLink(FileInfo.AbsoluteName) = SymlinkTarget);
  end;
end;

{ Test IsSymLink, EnumFiles, CastleReadLink }
procedure TTestOSSpecific.TestIsSymLink;
begin
  SymlinkName :=  'castle_game_engine_test_symlink_' + IntToStr(Random(100000));
  SymlinkFullName := InclPathDelim(GetTempDir) + SymlinkName;
  // Writeln('Using ', SymlinkFullName);
  SymlinkTarget := InclPathDelim(GetCurrentDir) + 'data/symlink_target.txt';

  FpSymlink(PChar(SymlinkTarget), PChar(SymlinkFullName));

  EnumFiles(SymlinkFullName, faAnyFile, @TestIsSymLink_Proc, Self, [eoSymlinks]);

  if not DeleteFile(SymlinkFullName) then
    raise Exception.CreateFmt('Failed to remove symlink file %s', [SymlinkFullName]);

  try
    CastleReadLink('/non_existing_file');
    Assert(false, 'CastleReadLink must raise exception on non-existing file');
  except
    on E: EOSError do
      { OK, CastleReadLink for /non_existing_file should raise exception };
  end;
end;
{$endif}

procedure TTestOSSpecific.TestExeName;
begin
  try
//    Writeln('ExeName: '+ ExeName);
    ExeName;
  except
    on E: EExeNameNotAvailable do
    begin
//      Writeln('ExeName not available, allowed on non-Windows:' +nl+ ExceptMessage(E));
      { This is an error under Windows, where ExeName is guaranteed to be available }
      {$ifdef MSWINDOWS} raise; {$endif}
    end;
  end;
end;

procedure TTestOSSpecific.TestTimer;
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
    [ ProcessTimerEnd, (Timer - TimerStart) / TimerFrequency ])); }
end;

{$ifdef UNIX}
procedure TTestOSSpecific.TestHomePath;
begin
{  Writeln('HomePath is ', HomePath);
  Writeln('  ~/expanded is ', ExpandHomePath('~/expanded')); }
  Assert(ExpandHomePath('/bin/') = '/bin/');
  Assert(ExpandHomePath('~') = ExclPathDelim(HomePath));
  Assert(ExpandHomePath('~/') = InclPathDelim(HomePath));
end;
{$endif}

procedure TTestOSSpecific.TestGetTempDir;
begin
//  Writeln('TempDir: ', GetTempDir);
  GetTempDir; // ignore result, just make sure it doesn't raise errors
end;

initialization
  RegisterTest(TTestOSSpecific);
end.
