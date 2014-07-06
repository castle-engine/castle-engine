{
  Copyright 2014-2014 Michalis Kamburelis and FPC team.

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
procedure Compile(const OS: TOS; const CPU: TCPU; const Mode: TCompilationMode;
  const WorkingDirectory, CompileFile: string);

function ModeToString(const M: TCompilationMode): string;
function StringToMode(const S: string): TCompilationMode;

implementation

uses SysUtils, Process,
  CastleUtils, CastleStringUtils, CastleWarnings,
  ToolUtils;

procedure Compile(const OS: TOS; const CPU: TCPU; const Mode: TCompilationMode;
  const WorkingDirectory, CompileFile: string);
var
  CastleEnginePath, CastleEngineSrc: string;
  FpcOptions: TCastleStringList;

  procedure AddEnginePath(Path: string);
  begin
    Path := CastleEngineSrc + Path;
    if not DirectoryExists(Path) then
      OnWarning(wtMajor, 'Path', Format('Path "%s" does not exist. Make sure that $CASTLE_ENGINE_PATH points to the directory containing Castle Game Engine sources (in castle_game_engine subdirectory)', [Path]));
    FpcOptions.Add('-Fu' + Path);
    FpcOptions.Add('-Fi' + Path);
  end;

var
  FpcOutput: string;
  FpcExitStatus: Integer;
begin
  FpcOptions := TCastleStringList.Create;
  try
    CastleEnginePath := GetEnvironmentVariable('CASTLE_ENGINE_PATH');
    if CastleEnginePath = '' then
    begin
      Writeln('CASTLE_ENGINE_PATH environment variable not defined, so we assume that engine unit paths are already specified within fpc.cfg file');
    end else
    begin
      { Use  $CASTLE_ENGINE_PATH environment variable as the directory
        containing castle_game_engine as subdirectory.
        Then this script outputs all -Fu and -Fi options for FPC to include
        Castle Game Engine sources.

        We also output -dCASTLE_ENGINE_PATHS_ALREADY_DEFINED,
        and @.../castle-fpc.cfg, to add castle-fpc.cfg with the rest of
        proper Castle Game Engine compilation options.

        This script is useful to compile programs using Castle Game Engine
        from any directory. You just have to
        set $CASTLE_ENGINE_PATH environment variable before calling
        (or make sure that units paths are in fpc.cfg). }

      CastleEngineSrc := InclPathDelim(CastleEnginePath) +
        'castle_game_engine' + PathDelim + 'src' + PathDelim;

      { Note that we add OS-specific paths (windows, android, unix)
        regardless of the target OS. There is no point in filtering them
        only for specific OS, since all file names must be different anyway,
        as Lazarus packages could not compile otherwise. }

      AddEnginePath('base');
      AddEnginePath('base/android');
      AddEnginePath('base/windows');
      AddEnginePath('base/unix');
      AddEnginePath('fonts');
      AddEnginePath('fonts/windows');
      AddEnginePath('opengl');
      AddEnginePath('opengl/glsl');
      AddEnginePath('window');
      AddEnginePath('window/gtk');
      AddEnginePath('window/windows');
      AddEnginePath('window/unix');
      AddEnginePath('images');
      AddEnginePath('3d');
      AddEnginePath('x3d');
      AddEnginePath('x3d/opengl');
      AddEnginePath('x3d/opengl/glsl');
      AddEnginePath('audio');
      AddEnginePath('net');
      AddEnginePath('castlescript');
      AddEnginePath('ui');
      AddEnginePath('ui/windows');
      AddEnginePath('ui/opengl');
      AddEnginePath('game');

      { Do not add castle-fpc.cfg.
        Instead, rely on code below duplicating castle-fpc.cfg logic.
        This way, it's tested.
      FpcOptions.Add('-dCASTLE_ENGINE_PATHS_ALREADY_DEFINED');
      FpcOptions.Add('@' + InclPathDelim(CastleEnginePath) + 'castle_game_engine/castle-fpc.cfg');
      }
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
        { See https://sourceforge.net/p/castle-engine/wiki/Android%20development/#notes-about-compiling-with-hard-floats-cfvfpv3 }
        FpcOptions.Add('-CfVFPV3');
    end;

    FpcOptions.Add(CompileFile);

    if Verbose then
    begin
      Writeln('FPC compilation options:');
      Writeln(FpcOptions.Text);
    end;

    Writeln('FPC executing...');
    RunCommandIndirPassthrough(WorkingDirectory, 'fpc', FpcOptions.ToArray, FpcOutput, FpcExitStatus);
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
