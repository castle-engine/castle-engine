{
  Copyright 2014-2018 Michalis Kamburelis.

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

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile".
  Comment this out if you don't compile using our "castle-engine" build tool. }
{$ifdef MSWINDOWS} {$R automatic-windows-resources.res} {$endif MSWINDOWS}

uses SysUtils,
  CastleUtils, CastleParameters, CastleFindFiles, CastleLog,
  CastleFilesUtils, CastleURIUtils, CastleStringUtils,
  CastleApplicationProperties,
  ToolArchitectures, ToolProject, ToolCompile, ToolUtils, ToolIOS, ToolAndroid;

var
  Target: TTarget;
  OS: TOS;
  CPU: TCPU;
  Plugin: boolean = false;
  Mode: TCompilationMode = cmRelease;
  AssumeCompiled: boolean = false;
  Fast: boolean = false;
  CompilerExtraOptions: TCastleStringList;

const
  Options: array [0..13] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short: #0 ; Long: 'target'; Argument: oaRequired),
    (Short: #0 ; Long: 'os'; Argument: oaRequired),
    (Short: #0 ; Long: 'cpu'; Argument: oaRequired),
    (Short: 'V'; Long: 'verbose'; Argument: oaNone),
    (Short: #0 ; Long: 'mode'; Argument: oaRequired),
    (Short: #0 ; Long: 'assume-compiled'; Argument: oaNone),
    (Short: #0 ; Long: 'fast'; Argument: oaNone),
    (Short: #0 ; Long: 'plugin'; Argument: oaNone),
    (Short: #0 ; Long: 'fpc-version-iphone-simulator'; Argument: oaRequired),
    (Short: #0 ; Long: 'compiler-option'; Argument: oaRequired),
    (Short: #0 ; Long: 'output'; Argument: oaRequired),
    (Short: #0 ; Long: 'project'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);

  { Call SetCurrentDir to open project within specified directory. }
  procedure ChangeProjectDir(const DirOrManifestFile: String);
  var
    Dir: String;
  begin
    if ExtractFileName(DirOrManifestFile) = 'CastleEngineManifest.xml' then
      Dir := ExtractFilePath(DirOrManifestFile)
    else
      Dir := DirOrManifestFile;
    if not SetCurrentDir(Dir) then
      raise EInvalidParams.CreateFmt('Cannot enter project directory "%s"', [Dir]);
  end;

begin
  case OptionNum of
    0 : begin
          Writeln(
            'castle-engine: Build and package Castle Game Engine programs.' +NL+
            NL+
            'Call with the current directory set to your project, like this:' +NL+
            '  castle-engine [OPTIONS]... COMMAND' +NL+
            NL+
            'Possible commands:' +NL+
            NL+
            'create-manifest:' +NL+
            '    Creates simple CastleEngineManifest.xml with guessed values.' +NL+
            NL+
            'compile:' +NL+
            '    Compile project.' +NL+
            '    By default compiles for the current OS / current CPU (' + OSToString(DefaultOS) + ' / ' + CPUToString(DefaultCPU) + ').' +NL+
            '    You can use --os / --cpu options to compile to some other OS / CPU.' +NL+
            '    You can use --target to compile for a collection of OS / CPU' +NL+
            '    combination (like "iOS" or "Android").' +NL+
            NL+
            'package:' +NL+
            '    Package the application into the best archive format for given' +NL+
            '    operating system (OS) / processor (CPU) / target.' +NL+
            '    The OS, CPU and "target" can be changed just like at "compile".' +NL+
            NL+
            'install:' +NL+
            '    Install the application created by previous "package" call.' +NL+
            '    Useful when OS is "android", it installs' +NL+
            '    the apk package created by previous "package" call' +NL+
            '    for Android. Useful for quick testing of your app on a device' +NL+
            '    connected through USB.' +NL+
            '    Useful also for installing compiled web browser plugin.' +NL+
            NL+
            'run:' +NL+
            '    Run the application. ' +NL+
            '    On some platforms, it requires installing the application first' +NL+
            '    (e.g. on Android, where we install and run on a device' +NL+
            '    connected through USB). So run the "install" command before.' +NL+
            '    On other platforms (e.g. standalone Windows, Linux, macOS...),' +NL+
            '    it simply runs the last compiled application.' +NL+
            '    So just "compile" the application first.' +NL+
            NL+
            'package-source:' +NL+
            '    Package the source code of the application.' +NL+
            NL +
            'clean:' +NL+
            '    Clean leftover files from compilation and packaging.' +NL+
            '    Does not remove final packaging output.' +NL+
            NL+
            'simple-compile:' +NL+
            '    Compile the Object Pascal file (unit/program/library) given' +NL+
            '    as a parameter. This does not handle the Castle Game Engine projects' +NL+
            '    defined by CastleEngineManifest.xml files.' +NL+
            '    It merely calls "fpc" with proper command-line options for' +NL+
            '    units/programs/libraries using our engine.' +NL+
            '    Use this instead of "compile" only if there''s some good reason' +NL+
            '    you don''t want to use CastleEngineManifest.xml to your project.' +NL+
            NL+
            'auto-generate-textures:' +NL+
            '    Create GPU-compressed versions of textures,' +NL+
            '    for the textures mentioned in <auto_compressed_textures>' +NL+
            '    inside the file data/material_properties.xml.' +NL+
            NL+
            'auto-generate-clean:' +NL+
            '    Clear "auto_compressed" subdirectories, that should contain only' +NL+
            '    the output created by "auto-generate-textures" target.' +NL+
            NL+
            'generate-program:' +NL+
            '    Generate lpr and lpi files to edit and run this project in Lazarus.' +NL+
            '    Depends on game_units being defined in the CastleEngineManifest.xml.' +NL+
            NL+
            'editor:' +NL+
            '    Run Castle Game Engine Editor within this project, with possible' +NL+
            '    project-specific components.' +NL+
            NL+
            'Available options are:' +NL+
            HelpOptionHelp +NL+
            VersionOptionHelp +NL+
            OptionDescription('-V / --verbose',
              'Verbose mode, output contains e.g. list of packaged files.') +NL+
            OptionDescription('--mode=debug|release',
              'Compilation mode, used by "compile" command. Also packaging mode for some platforms (right now, Android). By default "release".') +NL+
            OptionDescription('--assume-compiled',
              'Do not automatically do "clean" and "compile" before "package". Instead assume that compiled executable for given OS/CPU/mode is already present in the package directory.') +NL+
            OptionDescription('--fast',
              'Do not "clean" before "package". Recompile only what changed. This is faster for development, but cannot guarantee that everything is recompiled in a release mode.') +NL+
            OptionDescription('--plugin',
              'Compile/package/install a browser plugin.') +NL+
            OptionDescription('--fpc-version-iphone-simulator VERSION',
              'When compiling for iPhone Simulator, we pass -V<VERSION> to the "fpc" command-line. This is necessary if you use the official "FPC for iOS" package (see the "Getting Started - iOS.rtf" inside the "FPC for iOS" dmg for explanation). You can set this to "auto" (this is the default) to auto-detect this based on regular FPC version. Or you can set this to a particular version, like "3.0.5". Or you can set this to empty "" to avoid passing any -V<VERSION> (suitable for FPC 3.1.1).') +NL+
            OptionDescription('--compiler-option=PARAM',
              'Extra parameter for "fpc" command line. For example --compiler-option=-dUSE_MOUSE will add -dUSE_MOUSE. You can use this parameter multiple times.') +NL+
            OptionDescription('--output=DIR',
              'Where to place the output executables, packages, and the "castle-engine-output" directory with temporary generated files.') +NL+
            OptionDescription('--project=DIR',
              'Where to search for the project (CastleEngineManifest.xml file). By default we search in the current directory. The argument can either be a directory, or a filename of CastleEngineManifest.xml file.') +NL+
            TargetOptionHelp +
            OSOptionHelp +
            CPUOptionHelp +
            NL+
            'Full documentation on' + NL +
            'https://github.com/castle-engine/castle-engine/wiki/Build-Tool' + NL +
            NL+
            SCastleEngineProgramHelpSuffix(ApplicationName,
              ApplicationProperties.Version, true));
          Halt;
        end;
    1 : begin
          // include ApplicationName in version, good for help2man
          Writeln(ApplicationName + ' ' + ApplicationProperties.Version);
          Halt;
        end;
    2 : Target := StringToTarget(Argument);
    3 : OS := StringToOS(Argument);
    4 : CPU := StringToCPU(Argument);
    5 : Verbose := true;
    6 : Mode := StringToMode(Argument);
    7 : AssumeCompiled := true;
    8 : Fast := true;
    9 : Plugin := true;
    10: FPCVersionForIPhoneSimulator := Argument;
    11: CompilerExtraOptions.Add(Argument);
    12: OutputPathBase := Argument;
    13: ChangeProjectDir(Argument);
    else raise EInternalError.Create('OptionProc');
  end;
end;

{ For some operations (like creating an Android project), the tool uses
  ApplicationData files. So make sure that ApplicationData is correct.
  We can use $CASTLE_ENGINE_PATH environment variable for this. }
procedure AdjustApplicationData;
var
  CastleEnginePath, Data1, Data2, Data3, DataSuffix: string;
begin
  CastleEnginePath := GetEnvironmentVariable('CASTLE_ENGINE_PATH');
  if CastleEnginePath <> '' then
  begin
    DataSuffix := PathDelim + 'tools' + PathDelim + 'build-tool' + PathDelim + 'data' + PathDelim;
    Data1 := ExclPathDelim(CastleEnginePath) + DataSuffix;
    Data2 := InclPathDelim(CastleEnginePath) + 'castle_game_engine' + DataSuffix;
    Data3 := InclPathDelim(CastleEnginePath) + 'castle-engine' + DataSuffix;
    if DirectoryExists(Data1) then
      ApplicationDataOverride := FilenameToURISafe(Data1) else
    if DirectoryExists(Data2) then
      ApplicationDataOverride := FilenameToURISafe(Data2) else
    if DirectoryExists(Data3) then
      ApplicationDataOverride := FilenameToURISafe(Data3) else
      { We do not complain about missing or invalid $CASTLE_ENGINE_PATH
        otherwise, because for some operations ApplicationData is not used,
        and also sometimes the default ApplicationData (in case of system-wide
        installation in /usr/share/castle-engine/ ) will be Ok. }
      Exit;
    if Verbose then
      Writeln('Build tool found its data in ' + ApplicationDataOverride);
  end;
end;

procedure Run;
var
  Command, S, FileName: string;
  Project: TCastleProject;
  RestOfParameters: TCastleStringList;
begin
  ApplicationProperties.ApplicationName := 'castle-engine';
  ApplicationProperties.Version := CastleEngineVersion;
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  OS := DefaultOS;
  CPU := DefaultCPU;
  CompilerExtraOptions := TCastleStringList.Create();

  { parse parameters }
  Parameters.Parse(Options, @OptionProc, nil);
  if Parameters.High < 1 then
    raise EInvalidParams.Create('Not enough command-line parameters, expected a COMMAND to perform. Use --help to get usage information') else
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

  AdjustApplicationData;

  if Command = 'simple-compile' then
  begin
    Parameters.CheckHigh(2);
    FileName := Parameters[2];
    { use GetCurrentDir as WorkingDir,
      so calling "castle-engine simple-compile somesubdir/myunit.pas" works.
      Working dir for FPC must be equal to our own working dir. }
    case Target of
      targetCustom:  Compile(OS, CPU, Plugin, Mode, GetCurrentDir, FileName, nil, nil, CompilerExtraOptions);
      targetAndroid: CompileAndroid(nil, Mode, GetCurrentDir, FileName, nil, nil, CompilerExtraOptions);
      targetIOS:     CompileIOS(Mode, GetCurrentDir, FileName, nil, nil, CompilerExtraOptions);
      else raise EInternalError.Create('Operation not implemented for this target');
    end;
  end else
  begin
    if Command <> 'run' then
      Parameters.CheckHigh(1);
    Project := TCastleProject.Create;
    try
      if Command = 'create-manifest' then
        Project.DoCreateManifest
      else
      if Command = 'compile' then
        Project.DoCompile(Target, OS, CPU, Plugin, Mode, CompilerExtraOptions)
      else
      if Command = 'package' then
      begin
        if not AssumeCompiled then
        begin
          if not Fast then
            Project.DoClean;
          Project.DoCompile(Target, OS, CPU, Plugin, Mode, CompilerExtraOptions);
        end;
        Project.DoPackage(Target, OS, CPU, Plugin, Mode);
      end else
      if Command = 'install' then
        Project.DoInstall(Target, OS, CPU, Plugin) else
      if Command = 'run' then
      begin
        RestOfParameters := TCastleStringList.Create;
        try
          RestOfParameters.Text := Parameters.Text;
          RestOfParameters.Delete(0); // remove our own name
          RestOfParameters.Delete(0); // remove "run"
          Project.DoRun(Target, OS, CPU, Plugin, RestOfParameters);
        finally FreeAndNil(RestOfParameters) end;
      end else
      if Command = 'package-source' then
      begin
        Project.DoClean;
        Project.DoPackageSource;
      end else
      if Command = 'clean' then
        Project.DoClean
      else
      if Command = 'auto-generate-textures' then
        Project.DoAutoGenerateTextures
      else
      if Command = 'auto-generate-clean' then
        Project.DoAutoGenerateClean
      else
      if Command = 'generate-program' then
        Project.DoGenerateProgram
      else
      if Command = 'editor' then
        Project.DoEditor
      else
        raise EInvalidParams.CreateFmt('Invalid COMMAND to perform: "%s". Use --help to get usage information', [Command]);
    finally FreeAndNil(Project) end;
  end;
end;

begin
  try
    Run;
  except
    on E: TObject do
    begin
      { In case of exception, write nice message and exit with non-zero status,
        without dumping any stack trace (because it's normal for build tool to
        exit with exception in case of project/environment error, not a bug). }
      Writeln(ErrOutput, ExceptMessage(E));
      Halt(1);
    end;
  end;
end.
