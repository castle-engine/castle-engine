{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Compiling with FPC. }
unit ToolCompile;

interface

uses ToolArchitectures;

type
  TCompilationMode = (cmRelease, cmDebug);

{ Compile with FPC and proper command-line option given file. }
procedure Compile(const OS: TOS; const CPU: TCPU; const Plugin: boolean;
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string);

function ModeToString(const M: TCompilationMode): string;
function StringToMode(const S: string): TCompilationMode;

implementation

uses SysUtils, Process,
  CastleUtils, CastleStringUtils, CastleWarnings, CastleFilesUtils,
  ToolUtils;

type
  TFPCVersion = object
    Major, Minor, Release: Integer;
  end;

function FPCVersion: TFPCVersion;
var
  FpcOutput, FpcExe, Token: string;
  FpcExitStatus, SeekPos: Integer;
begin
  Writeln('Querying FPC version...');
  FpcExe := FindExe('fpc');
  if FpcExe = '' then
    raise Exception.Create('Cannot find "fpc" program on $PATH. Make sure it is installed, and available on $PATH');
  MyRunCommandIndir(GetCurrentDir, FpcExe, ['-iV'], FpcOutput, FpcExitStatus);
  if FpcExitStatus <> 0 then
    raise Exception.Create('Failed to query FPC version');

  { parse output into 3 numbers }
  FpcOutput := Trim(FpcOutput);
  SeekPos := 1;

  Token := NextToken(FpcOutput, SeekPos, ['.', '-']);
  if Token = '' then
    raise Exception.CreateFmt('Failed to query FPC version: no major version in response "%s"', [FpcOutput]);
  Result.Major := StrToInt(Token);

  Token := NextToken(FpcOutput, SeekPos, ['.', '-']);
  if Token = '' then
    raise Exception.CreateFmt('Failed to query FPC version: no minor version in response "%s"', [FpcOutput]);
  Result.Minor := StrToInt(Token);

  Token := NextToken(FpcOutput, SeekPos, ['.', '-']);
  if Token = '' then
  begin
    OnWarning(wtMajor, 'FPC', Format('Failed to query FPC version: no release version in response "%s", assuming 0', [FpcOutput]));
    Result.Release := 0;
  end else
    Result.Release := StrToInt(Token);

  Writeln(Format('Recognized version %d.%d.%d', [Result.Major, Result.Minor, Result.Release]));
end;

procedure Compile(const OS: TOS; const CPU: TCPU; const Plugin: boolean;
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string);
var
  CastleEnginePath, CastleEngineSrc: string;
  FpcOptions: TCastleStringList;

  procedure AddEnginePath(Path: string);
  begin
    Path := CastleEngineSrc + Path;
    if not DirectoryExists(Path) then
      OnWarning(wtMajor, 'Path', Format('Path "%s" does not exist. Make sure that $CASTLE_ENGINE_PATH points to the directory containing Castle Game Engine sources (the castle_game_engine/ or castle-engine/ directory)', [Path]));
    FpcOptions.Add('-Fu' + Path);
    FpcOptions.Add('-Fi' + Path);
  end;

var
  FpcOutput, CastleEngineSrc1, CastleEngineSrc2, CastleEngineSrc3, FpcExe: string;
  FpcExitStatus: Integer;
begin
  FpcOptions := TCastleStringList.Create;
  try
    CastleEnginePath := GetEnvironmentVariable('CASTLE_ENGINE_PATH');
    if CastleEnginePath = '' then
    begin
      Writeln('CASTLE_ENGINE_PATH environment variable not defined, so we assume that engine unit paths are already specified within fpc.cfg file.');
    end else
    begin
      { Use $CASTLE_ENGINE_PATH environment variable as the directory
        containing castle_game_engine/ or castle-engine/ as subdirectory
        (or pointing straight to castle_game_engine/ or castle-engine/
        directory).
        Then add all -Fu and -Fi options for FPC to include Castle Game Engine
        sources.

        This way you can compile programs using Castle Game Engine
        from any directory. You just have to
        set $CASTLE_ENGINE_PATH environment variable
        (or make sure that units paths are in fpc.cfg). }

      { calculate CastleEngineSrc }
      CastleEngineSrc1 := InclPathDelim(CastleEnginePath) +
        'src' + PathDelim;
      CastleEngineSrc2 := InclPathDelim(CastleEnginePath) +
        'castle_game_engine' + PathDelim + 'src' + PathDelim;
      CastleEngineSrc3 := InclPathDelim(CastleEnginePath) +
        'castle-engine' + PathDelim + 'src' + PathDelim;
      if DirectoryExists(CastleEngineSrc1) then
        CastleEngineSrc := CastleEngineSrc1 else
      if DirectoryExists(CastleEngineSrc2) then
        CastleEngineSrc := CastleEngineSrc2 else
      if DirectoryExists(CastleEngineSrc3) then
        CastleEngineSrc := CastleEngineSrc3 else
      begin
        CastleEngineSrc := '';
        Writeln('CASTLE_ENGINE_PATH environment variable defined, but we cannot find Castle Game Engine sources inside (looking in "' + CastleEngineSrc1 + '" and "' + CastleEngineSrc2 + '" and "' + CastleEngineSrc3 + '").');
        Writeln('  We continue compilation, assuming that engine unit paths are already specified within fpc.cfg file.');
      end;

      if CastleEngineSrc <> '' then
      begin
        { Note that we add OS-specific paths (windows, android, unix)
          regardless of the target OS. There is no point in filtering them
          only for specific OS, since all file names must be different anyway,
          as Lazarus packages could not compile otherwise. }

        AddEnginePath('base');
        AddEnginePath('base/android');
        AddEnginePath('base/windows');
        AddEnginePath('base/unix');
        AddEnginePath('base/opengl');
        AddEnginePath('fonts');
        AddEnginePath('fonts/windows');
        AddEnginePath('fonts/opengl');
        AddEnginePath('window');
        AddEnginePath('window/gtk');
        AddEnginePath('window/windows');
        AddEnginePath('window/unix');
        AddEnginePath('images');
        AddEnginePath('images/opengl');
        AddEnginePath('images/opengl/glsl');
        AddEnginePath('3d');
        AddEnginePath('3d/opengl');
        AddEnginePath('x3d');
        AddEnginePath('x3d/opengl');
        AddEnginePath('x3d/opengl/glsl');
        AddEnginePath('audio');
        AddEnginePath('net');
        AddEnginePath('castlescript');
        AddEnginePath('castlescript/opengl');
        AddEnginePath('ui');
        AddEnginePath('ui/windows');
        AddEnginePath('ui/opengl');
        AddEnginePath('game');
        AddEnginePath('services');

        { Do not add castle-fpc.cfg.
          Instead, rely on code below duplicating castle-fpc.cfg logic
          (reasons: engine sources, with castle-fpc.cfg, may not be available
          where build-tool is called).

        FpcOptions.Add('-dCASTLE_ENGINE_PATHS_ALREADY_DEFINED');
        FpcOptions.Add('@' + InclPathDelim(CastleEnginePath) + 'castle_game_engine(or castle-engine)/castle-fpc.cfg');
        }
      end;
    end;

    { Specify the compilation options explicitly,
      duplicating logic from ../castle-fpc.cfg .
      (Engine sources may be possibly not available,
      so we also cannot depend on castle-fpc.cfg being available.) }
    FpcOptions.Add('-l');
    FpcOptions.Add('-vwn');
    FpcOptions.Add('-Ci');
    FpcOptions.Add('-Mobjfpc');
    FpcOptions.Add('-Sm');
    FpcOptions.Add('-Sc');
    FpcOptions.Add('-Sg');
    FpcOptions.Add('-Si');
    FpcOptions.Add('-Sh');
    FpcOptions.Add('-vm2045'); // do not show Warning: (2045) APPTYPE is not supported by the target OS
    FpcOptions.Add('-vm5024'); // do not show Hint: (5024) Parameter "..." not used
    if FPCVersion.Major >= 3 then
    begin
      { do not show
          Warning: Implicit string type conversion from "AnsiString" to "WideString"
          Warning: Implicit string type conversion from "AnsiString" to "UnicodeString"
        As we normally use AnsiString, and we deal with XML units
        (using WideString / UnicodeString), this is normal situation for us. }
      FpcOptions.Add('-vm4105'); // not available in FPC 2.6.4
      { do not show
          Warning: Implicit string type conversion with potential data loss from "WideString" to "AnsiString"
        As we normally use AnsiString, and we deal with XML units
        (using WideString / UnicodeString), this is normal situation for us. }
      FpcOptions.Add('-vm4104'); // not available in FPC 2.6.4
    end;
    FpcOptions.Add('-T' + OSToString(OS));
    FpcOptions.Add('-P' + CPUToString(CPU));

    case Mode of
      cmRelease:
        begin
          FpcOptions.Add('-O2');
          FpcOptions.Add('-Xs');
          FpcOptions.Add('-dRELEASE');
        end;
      cmDebug:
        begin
          FpcOptions.Add('-Cr');
          FpcOptions.Add('-Co');
          FpcOptions.Add('-Sa');
          FpcOptions.Add('-CR');
          FpcOptions.Add('-g');
          FpcOptions.Add('-gl');
          FpcOptions.Add('-dDEBUG');
        end;
      else raise EInternalError.Create('DoCompile: Mode?');
    end;

    case OS of
      Android:
        { See https://github.com/castle-engine/castle-engine/wiki/Android-Internal-Information#notes-about-compiling-with-hard-floats--cfvfpv3 }
        FpcOptions.Add('-CfVFPV3');
    end;

    if Plugin then
    begin
      FpcOptions.Add('-dCASTLE_ENGINE_PLUGIN');
      { We need to add -fPIC otherwise compiling a shared library fails with

        /usr/bin/ld: .../castlewindow.o: relocation R_X86_64_32S against
          `U_CASTLEWINDOW_MENUITEMS' can not be used when making
          a shared object; recompile with -fPIC

        That is because FPC RTL is compiled with -fPIC for this platform,
        it seems. See
        http://lists.freepascal.org/pipermail/fpc-pascal/2014-November/043155.html
        http://lists.freepascal.org/pipermail/fpc-pascal/2014-November/043159.html

        """
        fpcmake automatically adds -Cg/-fPIC for x86-64 platforms,
        exactly because of this issue.
        """

        And http://wiki.freepascal.org/PIC_information explains about PIC:
        """
        fpcmake automatically adds -Cg/-fPIC for x86-64 platforms:
        * for freebsd, openbsd, netbsd, linux, solaris
        * for darwin -Cg is enabled by the compiler (see below)
        """
      }
      if (CPU = X86_64) and (OS in [Linux,FreeBSD,NetBSD,OpenBSD,Solaris]) then
        FpcOptions.Add('-fPIC');
    end;

    FpcOptions.Add(CompileFile);

    Writeln('FPC executing...');
    FpcExe := FindExe('fpc');
    if FpcExe = '' then
      raise Exception.Create('Cannot find "fpc" program on $PATH. Make sure it is installed, and available on $PATH');
    RunCommandIndirPassthrough(WorkingDirectory, FpcExe, FpcOptions.ToArray, FpcOutput, FpcExitStatus);
    if FpcExitStatus <> 0 then
      raise Exception.Create('Failed to compile');
  finally FreeAndNil(FpcOptions) end;
end;

{ globals -------------------------------------------------------------------- }

const
  CompilationModeNames: array [TCompilationMode] of string =
  ('release', 'debug');

function ModeToString(const M: TCompilationMode): string;
begin
  Result := CompilationModeNames[M];
end;

function StringToMode(const S: string): TCompilationMode;
begin
  for Result in TCompilationMode do
    if AnsiSameText(CompilationModeNames[Result], S) then
      Exit;
  raise Exception.CreateFmt('Invalid compilation mode name "%s"', [S]);
end;

end.
