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

{ Castle Game Engine project information. }
unit CastleProject;

interface

uses CastleFindFiles, CastleArchitectures, CastleStringUtils, CastleUtils;

type
  TDependency = (depFreetype, depZlib, depPng, depSound, depOggVorbis);
  TDependencies = set of TDependency;

  TCompilationMode = (cmRelease, cmDebug);

type
  TCastleProject = class
  private
    FDependencies: TDependencies;
    FName, FExecutableName: string;
    GatheringFiles: TCastleStringList; //< only for GatherFile
    ManifestFile, ProjectPath, DataPath: string;
    IncludePaths, ExcludePaths: TCastleStringList;
    IncludePathsRecursive: TBooleanList;
    FStandaloneSource, FAndroidSource: string;
    DeletedFiles: Cardinal; //< only for DeleteFoundFile
    FVersionExecutableOption: string;
    procedure GatherFile(const FileInfo: TFileInfo);
    procedure AddDependency(const Dependency: TDependency; const FileInfo: TFileInfo);
    procedure FoundTtf(const FileInfo: TFileInfo);
    procedure FoundGz(const FileInfo: TFileInfo);
    procedure FoundPng(const FileInfo: TFileInfo);
    procedure FoundWav(const FileInfo: TFileInfo);
    procedure FoundOgg(const FileInfo: TFileInfo);
    procedure DeleteFoundFile(const FileInfo: TFileInfo);
    function PackageName(const Version: string;
      const OS: TOS; const CPU: TCPU): string;
    { Output Android library resulting from compilation.
      Use only if AndroidSource <> ''.
      Relative to ProjectPath. }
    function AndroidLibraryFile: string;
  public
    constructor Create(const Path: string);
    destructor Destroy; override;

    property Dependencies: TDependencies read FDependencies;
    property Name: string read FName;
    property ExecutableName: string read FExecutableName;
    property StandaloneSource: string read FStandaloneSource;
    property AndroidSource: string read FAndroidSource;
    property VersionExecutableOption: string read FVersionExecutableOption;

    procedure DoCreateManifest;
    procedure DoCompile(const OS: TOS; const CPU: TCPU; const Mode: TCompilationMode);
    procedure DoPackage(const OS: TOS; const CPU: TCPU);
    procedure DoClean;
  end;

function DependencyToString(const D: TDependency): string;
function StringToDependency(const S: string): TDependency;

function ModeToString(const M: TCompilationMode): string;
function StringToMode(const S: string): TCompilationMode;

var
  Verbose: boolean;

implementation

uses SysUtils, StrUtils, DOM, Process, Classes,
  CastleURIUtils, CastleXMLUtils, CastleWarnings, CastleFilesUtils,
  CastleProjectUtils, CastlePackage;

{ TCastleProject ------------------------------------------------------------- }

constructor TCastleProject.Create(const Path: string);

  procedure ReadManifest;

    procedure AutoGuessManifest;
    begin
      Writeln('Manifest file not found: ' + ManifestFile);
      Writeln('Guessing project values. Use create-manifest command to write these guesses into new CastleEngineManifest.xml');
      FName := ExtractFileName(ExtractFileDir(ManifestFile));
    end;

  var
    Doc: TXMLDocument;
    ManifestURL: string;
    ChildElements: TDOMNodeList;
    Element, ChildElement: TDOMElement;
    I: Integer;
  begin
    ManifestFile := ProjectPath + 'CastleEngineManifest.xml';
    if not FileExists(ManifestFile) then
      AutoGuessManifest else
    begin
      if Verbose then
        Writeln('Manifest file found: ' + ManifestFile);
      ManifestURL := FilenameToURISafe(ManifestFile);

      try
        URLReadXML(Doc, ManifestURL);
        Check(Doc.DocumentElement.TagName = 'project',
          'Root node of CastleEngineManifest.xml must be <project>');
        FName := Doc.DocumentElement.AttributeString('name');
        FExecutableName := Doc.DocumentElement.AttributeStringDef('executable_name', FName);
        FStandaloneSource := Doc.DocumentElement.AttributeStringDef('standalone_source', '');
        FAndroidSource := Doc.DocumentElement.AttributeStringDef('android_source', '');

        Element := DOMGetChildElement(Doc.DocumentElement, 'version', false);
        if Element <> nil then
        begin
          FVersionExecutableOption := Element.AttributeString('executable_option');
        end;

        Element := DOMGetChildElement(Doc.DocumentElement, 'dependencies', false);
        if Element <> nil then
        begin
          ChildElements := Element.GetElementsByTagName('dependency');
          for I := 0 to ChildElements.Count - 1 do
          begin
            ChildElement := ChildElements[I] as TDOMElement;
            Include(FDependencies,
              StringToDependency(ChildElement.AttributeString('name')));
          end;
        end;

        Element := DOMGetChildElement(Doc.DocumentElement, 'package', false);
        if Element <> nil then
        begin
          ChildElements := Element.GetElementsByTagName('include');
          for I := 0 to ChildElements.Count - 1 do
          begin
            ChildElement := ChildElements[I] as TDOMElement;
            IncludePaths.Add(ChildElement.AttributeString('path'));
            IncludePathsRecursive.Add(ChildElement.AttributeBooleanDef('recursive', false));
          end;

          ChildElements := Element.GetElementsByTagName('exclude');
          for I := 0 to ChildElements.Count - 1 do
          begin
            ChildElement := ChildElements[I] as TDOMElement;
            ExcludePaths.Add(ChildElement.AttributeString('path'));
          end;
        end;
      finally FreeAndNil(Doc) end;
    end;
  end;

  procedure GuessDependencies;
  begin
    if DirectoryExists(DataPath) then
    begin
      if Verbose then
        Writeln('Found data in "' + DataPath + '"');
      FindFiles(DataPath, '*.ttf', false, @FoundTtf, [ffRecursive]);
      FindFiles(DataPath, '*.gz' , false, @FoundGz , [ffRecursive]);
      FindFiles(DataPath, '*.png', false, @FoundPng, [ffRecursive]);
      FindFiles(DataPath, '*.wav', false, @FoundWav, [ffRecursive]);
      FindFiles(DataPath, '*.ogg', false, @FoundOgg, [ffRecursive]);
    end else
      Writeln('Data directory not found (tried "' + DataPath + '")');
  end;

  procedure CloseDependencies;

    procedure DependenciesClosure(const Dep, DepRequirement: TDependency);
    begin
      if (Dep in Dependencies) and not (DepRequirement in Dependencies) then
      begin
        Writeln('Automatically adding "' + DependencyToString(DepRequirement) +
          '" to dependencies because it is a prerequisite of existing dependency "'
          + DependencyToString(Dep) + '"');
        Include(FDependencies, DepRequirement);
      end;
    end;

  begin
    DependenciesClosure(depPng, depZlib);
    DependenciesClosure(depOggVorbis, depSound);
  end;


const
  DataName = 'data';
begin
  inherited Create;

  { empty initial state }
  IncludePaths := TCastleStringList.Create;
  IncludePathsRecursive := TBooleanList.Create;
  ExcludePaths := TCastleStringList.Create;
  FDependencies := [];

  ProjectPath := InclPathDelim(Path);
  DataPath := InclPathDelim(ProjectPath + DataName);

  ReadManifest;
  GuessDependencies;
  CloseDependencies;
end;

destructor TCastleProject.Destroy;
begin
  FreeAndNil(IncludePaths);
  FreeAndNil(IncludePathsRecursive);
  FreeAndNil(ExcludePaths);
  inherited;
end;

procedure TCastleProject.AddDependency(const Dependency: TDependency; const FileInfo: TFileInfo);
begin
  if not (Dependency in Dependencies) then
  begin
    Writeln('Automatically adding "' + DependencyToString(Dependency) +
      '" to dependencies because data contains file: ' + FileInfo.URL);
    Include(FDependencies, Dependency);
  end;
end;

procedure TCastleProject.FoundTtf(const FileInfo: TFileInfo);
begin
  AddDependency(depFreetype, FileInfo);
end;

procedure TCastleProject.FoundGz(const FileInfo: TFileInfo);
begin
  AddDependency(depZlib, FileInfo);
end;

procedure TCastleProject.FoundPng(const FileInfo: TFileInfo);
begin
  AddDependency(depPng, FileInfo);
end;

procedure TCastleProject.FoundWav(const FileInfo: TFileInfo);
begin
  AddDependency(depSound, FileInfo);
end;

procedure TCastleProject.FoundOgg(const FileInfo: TFileInfo);
begin
  AddDependency(depOggVorbis, FileInfo);
end;

procedure TCastleProject.GatherFile(const FileInfo: TFileInfo);
begin
  GatheringFiles.Add(ExtractRelativePath(ProjectPath, FileInfo.AbsoluteName));
end;

procedure TCastleProject.DoCreateManifest;
var
  Contents: string;
begin
  if FileExists(ManifestFile) then
    raise Exception.CreateFmt('Manifest file "%s" already exists, refusing to overwrite it',
      [ManifestFile]);
  Contents := '<?xml version="1.0" encoding="utf-8"?>' +NL+
'<project name="' + Name + '">' +NL+
'</project>' + NL;
  StringToFile(ManifestFile, Contents);
  Writeln('Created manifest ' + ManifestFile);
end;

procedure TCastleProject.DoCompile(const OS: TOS; const CPU: TCPU; const Mode: TCompilationMode);
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
  FpcOutput, SourceExe, DestExe: string;
  FpcExitStatus: Integer;
begin
  Writeln(Format('Compiling project "%s" for OS "%s" and CPU "%s" in mode "%s".',
    [Name, OSToString(OS), CPUToString(CPU), ModeToString(Mode)]));

  FpcOptions := TCastleStringList.Create;
  try
    case OS of
      Android:
        begin
          if AndroidSource = '' then
            raise Exception.Create('android_source property for project not defined, cannot compile Android version');
        end;
      else
        begin
          if StandaloneSource = '' then
            raise Exception.Create('standalone_source property for project not defined, cannot compile standalone version');
        end;
    end;

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
        begin
          { See https://sourceforge.net/p/castle-engine/wiki/Android%20development/#notes-about-compiling-with-hard-floats-cfvfpv3 }
          FpcOptions.Add('-CfVFPV3');
          FpcOptions.Add(AndroidSource);
        end;
      else
        begin
          FpcOptions.Add(StandaloneSource);
        end;
    end;

    if Verbose then
    begin
      Writeln('FPC compilation options:');
      Writeln(FpcOptions.Text);
    end;

    Writeln('FPC executing...');
    RunCommandIndirPassthrough(ProjectPath, 'fpc', FpcOptions.ToArray, FpcOutput, FpcExitStatus);
    if FpcExitStatus <> 0 then
      raise Exception.Create('Failed to compile') else
    case OS of
      Android:
        begin
          Writeln('Compiled library for Android in ', AndroidLibraryFile);
        end;
      else
        begin
          SourceExe := ChangeFileExt(StandaloneSource, ExeExtensionOS(OS));
          DestExe := ChangeFileExt(ExecutableName, ExeExtensionOS(OS));
          if not AnsiSameText(SourceExe, DestExe) then
          begin
            { move exe to top-level (in case StandaloneSource is in subdirectory
              like code/) and eventually rename to follow ExecutableName }
            Writeln('Moving ', SourceExe, ' to ', DestExe);
            CheckRenameFile(ProjectPath + SourceExe, ProjectPath + DestExe);
          end;
        end;
    end;
  finally FreeAndNil(FpcOptions) end;
end;

procedure TCastleProject.DoPackage(const OS: TOS; const CPU: TCPU);
var
  Pack: TPackageDirectory;

  procedure AddExternalLibrary(const LibraryName: string);
  var
    CastleEnginePath, LibraryPath: string;
  begin
    CastleEnginePath := GetEnvironmentVariable('CASTLE_ENGINE_PATH');
    if CastleEnginePath = '' then
      raise Exception.Create('CASTLE_ENGINE_PATH environment variable not defined, we cannot find required library ' + LibraryName);
    LibraryPath := InclPathDelim(CastleEnginePath) +
      'external_libraries' + PathDelim +
      CPUToString(CPU) + '-' + OSToString(OS) + PathDelim + LibraryName;
    if not FileExists(LibraryPath) then
      raise Exception.Create('Dependency library not found in ' + LibraryPath);
    Pack.Add(LibraryPath, LibraryName);
  end;

  procedure Exclude(const PathMask: string; const Files: TCastleStringList);
  var
    I: Integer;
    PathMaskSlashes, ItemSlashes: string;
  begin
    { replace all backslashes with slashes, so that they are equal for comparison }
    PathMaskSlashes := StringReplace(PathMask, '\', '/', [rfReplaceAll]);
    I := 0;
    while I < Files.Count do
    begin
      ItemSlashes := StringReplace(Files[I], '\', '/', [rfReplaceAll]);
      if IsWild(ItemSlashes, PathMaskSlashes, true) then
        Files.Delete(I) else
        Inc(I);
    end;
  end;

  function GetVersionByRunning(const ExecutableNameExt: string;
    const VersionOption: string): string;
  var
    ProcessOutput: string;
    ProcessExitStatus: Integer;
  begin
    { get version by running ExecutableNameExt in main ProjectPath,
      not in temporary path (to avoid polluting temporary path for packaging
      with anything) }
    RunCommandIndir(ProjectPath, ExecutableNameExt, [VersionOption],
      ProcessOutput, ProcessExitStatus);
    if ProcessExitStatus <> 0 then
      raise Exception.CreateFmt('Process exited with error, status %d', [ProcessExitStatus]);
    Result := Trim(ProcessOutput);
    Writeln(Format('Version detected as "%s" (by running executable with "%s")',
      [Result, VersionOption]));
  end;

var
  Files: TCastleStringList;
  I: Integer;
  PackageFileName, ExecutableNameExt, Version: string;
  UnixPermissionsMatter: boolean;
  FindOptions: TFindFilesOptions;
begin
  Writeln(Format('Packaging project "%s" for OS "%s" and CPU "%s".',
    [Name, OSToString(OS), CPUToString(CPU)]));

  ExecutableNameExt := '';

  { packaging for OS where permissions matter }
  UnixPermissionsMatter := not (OS in AllWindowsOSes);

  Pack := TPackageDirectory.Create(Name);
  try
    Files := TCastleStringList.Create;
    try
      { executable is 1st on Files list, since it's the most likely file
        to not exist, so we'll fail earlier }
      if OS in [linux, go32v2, win32, os2, freebsd, beos, netbsd,
                amiga, atari, solaris, qnx, netware, openbsd, wdosx,
                palmos, macos, darwin, emx, watcom, morphos, netwlibc,
                win64, wince, gba,nds, embedded, symbian, haiku, {iphonesim,}
                aix, java, {android,} nativent, msdos, wii] then
      begin
        ExecutableNameExt := ExecutableName + ExeExtensionOS(OS);
        Files.Add(ExecutableNameExt);
      end;

      GatheringFiles := Files;
      FindFiles(DataPath, '*', false, @GatherFile, [ffRecursive]);
      for I := 0 to IncludePaths.Count - 1 do
      begin
        if IncludePathsRecursive[I] then
          FindOptions := [ffRecursive] else
          { not recursive, so that e.g. <include path="README.txt" />
            or <include path="docs/README.txt" />
            should not include *all* README.txt files inside. }
          FindOptions := [];
        FindFiles(ProjectPath + IncludePaths[I], false, @GatherFile, FindOptions);
      end;
      GatheringFiles := nil;

      Exclude('*.xcf', Files);
      Exclude('*.blend*', Files);
      Exclude('*~', Files);
      for I := 0 to ExcludePaths.Count - 1 do
        Exclude(ExcludePaths[I], Files);

      for I := 0 to Files.Count - 1 do
        Pack.Add(ProjectPath + Files[I], Files[I]);
    finally FreeAndNil(Files) end;

    { For OSes where chmod matters, make sure to set it before packing }
    if UnixPermissionsMatter then
      Pack.MakeExecutable(ExecutableNameExt);

    case OS of
      win32:
        begin
          if depFreetype in Dependencies then
            AddExternalLibrary('freetype-6.dll');
          if depZlib in Dependencies then
            AddExternalLibrary('zlib1.dll');
          if depPng in Dependencies then
            AddExternalLibrary('libpng12.dll');
          if depSound in Dependencies then
          begin
            AddExternalLibrary('OpenAL32.dll');
            AddExternalLibrary('wrap_oal.dll');
          end;
          if depOggVorbis in Dependencies then
          begin
            AddExternalLibrary('ogg.dll');
            AddExternalLibrary('vorbis.dll');
            AddExternalLibrary('vorbisenc.dll');
            AddExternalLibrary('vorbisfile.dll');
          end;
        end;

      win64:
        begin
          if depFreetype in Dependencies then
            OnWarning(wtMajor, 'Libraries', 'We do not know how to satisfy freetype dependency on win64');
          if depZlib in Dependencies then
            AddExternalLibrary('zlib1.dll');
          if depPng in Dependencies then
            AddExternalLibrary('libpng14-14.dll');
          if depSound in Dependencies then
            OnWarning(wtMajor, 'Libraries', 'We do not know how to satisfy OpenAL dependency on win64');
          if depOggVorbis in Dependencies then
            OnWarning(wtMajor, 'Libraries', 'We do not know how to satisfy OggVorbis dependency on win64');
        end;
    end;

    Version := '';
    if VersionExecutableOption <> '' then
    begin
      if ExecutableNameExt <> '' then
        Version := GetVersionByRunning(ExecutableNameExt, FVersionExecutableOption) else
        raise Exception.Create('Specified <version executable_option="..."/>, but the executable is not defined. Cannot detect version.');
    end;

    PackageFileName := PackageName(Version, OS, CPU);

    if OS in AllWindowsOSes then
      Pack.Make(ProjectPath, PackageFileName, ptZip) else
      Pack.Make(ProjectPath, PackageFileName, ptTarGz);
  finally FreeAndNil(Pack) end;
end;

function TCastleProject.PackageName(const Version: string;
  const OS: TOS; const CPU: TCPU): string;
begin
  Result := Name;
  if Version <> '' then
    Result += '-' + Version;
  Result += '-' + OSToString(OS) + '-' + CPUToString(CPU);
  if OS in AllWindowsOSes then
    Result += '.zip' else
    Result += '.tar.gz';
end;

procedure TCastleProject.DeleteFoundFile(const FileInfo: TFileInfo);
begin
  if Verbose then
    Writeln('Deleting ' + FileInfo.AbsoluteName);
  CheckDeleteFile(FileInfo.AbsoluteName);
  Inc(DeletedFiles);
end;

function TCastleProject.AndroidLibraryFile: string;
begin
  Result := ExtractFilePath(AndroidSource) + 'lib' +
    ChangeFileExt(ExtractFileName(AndroidSource), '.so');
end;

procedure TCastleProject.DoClean;

  procedure TryDeleteFile(FileName: string);
  begin
    FileName := ProjectPath + FileName;
    if FileExists(FileName) then
    begin
      if Verbose then
        Writeln('Deleting ' + FileName);
      CheckDeleteFile(FileName);
      Inc(DeletedFiles);
    end;
  end;

  procedure DeleteFilesRecursive(const Mask: string);
  begin
    FindFiles(ProjectPath, Mask, false, @DeleteFoundFile, [ffRecursive]);
  end;

begin
  if StandaloneSource <> '' then
  begin
    TryDeleteFile(ChangeFileExt(StandaloneSource, ''));
    TryDeleteFile(ChangeFileExt(StandaloneSource, '.exe'));
    { Make sure to also remove executable in main directory, in case
      source is in code/ subdirectory. It is paranoid to remove it from both
      places, but this way we know we're clean before packaging etc. }
    if ExtractFileName(StandaloneSource) <> StandaloneSource then
    begin
      TryDeleteFile(ChangeFileExt(ExtractFileName(StandaloneSource), ''));
      TryDeleteFile(ChangeFileExt(ExtractFileName(StandaloneSource), '.exe'));
    end;
  end;

  if AndroidSource <> '' then
    TryDeleteFile(AndroidLibraryFile);

  { packages created by DoPackage? Or not, it's safer to not remove them. }
  {
  for OS in TOS do
    for CPU in TCPU do
      if OSCPUSupported[OS, CPU] then
        TryDeleteFile(PackageName(OS, CPU));
  }

  { compilation and editor backups }
  DeleteFilesRecursive('*~'); // editor backup, e.g. Emacs
  DeleteFilesRecursive('*.ppu'); // compilation
  DeleteFilesRecursive('*.o'); // compilation
  DeleteFilesRecursive('*.compiled'); // Lazarus compilation
  DeleteFilesRecursive('*.rst'); // resource strings
  DeleteFilesRecursive('*.rsj'); // resource strings

  Writeln('Deleted ', DeletedFiles, ' files');
end;

{ globals -------------------------------------------------------------------- }

const
  DependencyNames: array [TDependency] of string =
  ('Freetype', 'Zlib', 'Png', 'Sound', 'OggVorbis');

function DependencyToString(const D: TDependency): string;
begin
  Result := DependencyNames[D];
end;

function StringToDependency(const S: string): TDependency;
begin
  for Result in TDependency do
    if AnsiSameText(DependencyNames[Result], S) then
      Exit;
  raise Exception.CreateFmt('Invalid dependency name "%s"', [S]);
end;

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
