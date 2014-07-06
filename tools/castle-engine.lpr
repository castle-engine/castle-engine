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

{ Build and package tool for Castle Game Engine programs.
  Call with --help for detailed usage instructions.
}

uses SysUtils,
  CastleUtils, CastleParameters, CastleFindFiles, CastleArchitectures,
  CastleProject, CastleWarnings;

var
  OS: TOS;
  CPU: TCPU;
  Mode: TCompilationMode = cmRelease;

const
  Version = '5.0.0'; //< When updating this, remember to also update version in ../fpmake.pp
  Options: array [0..5] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short: #0 ; Long: 'os'; Argument: oaRequired),
    (Short: #0 ; Long: 'cpu'; Argument: oaRequired),
    (Short: 'V'; Long: 'verbose'; Argument: oaNone),
    (Short: #0 ; Long: 'mode'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0:begin
        Writeln(
          'castle-engine: Build and package tool for Castle Game Engine programs.' +NL+
          NL+
          'Call with the current directory set to your project, like this:' +NL+
          '  castle-engine [OPTIONS]... COMMAND' +NL+
          NL+
          'Possible COMMANDs:' +NL+
          NL +
          '- "create-manifest" :' +NL+
          '  Creates simple CastleEngineManifest.xml with guessed values.' +NL+
          NL+
          '- "compile" :' +NL+
          '  Compile project.' +NL+
          NL+
          '- "package" :' +NL+
          '  Package the application into the best archive format for given' +NL+
          '  operating system (OS) / processor (CPU).' +NL+
          '  By default uses current OS / CPU (' + OSToString(DefaultOS) + ' / ' + CPUToString(DefaultCPU) + ').' +NL+
          '  You can also use --cpu or --os options to affect it.' +NL+
          NL +
          '- "clean" :' +NL+
          '  Clean leftover files from compilation and packaging.' +NL+
          '  Does not remove final packaging output.' +NL+
          NL+
          'Available options are:' +NL+
          HelpOptionHelp +NL+
          VersionOptionHelp +NL+
          '  -V / --verbose        Verbose mode, output contains e.g. list of packaged files.' +NL+
          '  --mode=debug|release  Compilation mode, used by "compile" command.' +NL+
          OSOptionHelp +
          CPUOptionHelp +
          NL+
          SCastleEngineProgramHelpSuffix(ApplicationName, Version, true));
          ProgramBreak;
      end;
    1:begin
        Writeln(Version);
        ProgramBreak;
      end;
    2:OS := StringToOS(Argument);
    3:CPU := StringToCPU(Argument);
    4:Verbose := true;
    5:Mode := StringToMode(Argument);
    else raise EInternalError.Create('OptionProc');
  end;
end;

procedure DoPackage;
begin
end;

var
  Command, S: string;
  Project: TCastleProject;
begin
  OnWarning := @OnWarningWrite;

  OS := DefaultOS;
  CPU := DefaultCPU;

  { parse parameters }
  Parameters.Parse(Options, @OptionProc, nil);
  if Parameters.High <> 1 then
    raise EInvalidParams.Create('Invalid command-line parameters, expected exactly one COMMAND to perform. Use --help to get usage information') else
  Command := Parameters[1];

  { check OS, CPU }
  if not OSCPUSupported[OS, CPU] then
  begin
    S := Format('The combination of operating system "%s" and processor "%s" is not possible (or not supported by FPC)',
      [OSToString(OS), CPUToString(CPU)]);
    if OS in AllWindowsOSes then
      S += '. Note: in case of Windows 64-bit, remember to specify both OS and CPU like this: "--os=win64 --cpu=x86_64"';
    raise EInvalidParams.Create(S);
  end;

  Project := TCastleProject.Create(GetCurrentDir);
  try
    if Command = 'create-manifest' then
      Project.DoCreateManifest else
    if Command = 'compile' then
      Project.DoCompile(OS, CPU, Mode) else
    if Command = 'package' then
      Project.DoPackage(OS, CPU) else
    if Command = 'clean' then
      Project.DoClean else
      raise EInvalidParams.CreateFmt('Invalid COMMAND to perform: "%s". Use --help to get usage information', [Command]);
  finally FreeAndNil(Project) end;
end.
