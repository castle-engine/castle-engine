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

{ Project information (from CastleEngineManifest.xml) and operations. }
unit ToolProject;

interface

uses SysUtils,
  CastleFindFiles, CastleStringUtils, CastleUtils,
  ToolArchitectures, ToolCompile, ToolUtils;

type
  TDependency = (depFreetype, depZlib, depPng, depSound, depOggVorbis);
  TDependencies = set of TDependency;

  TScreenOrientation = (soAny, soLandscape, soPortrait);

  TCastleProject = class
  private
    FDependencies: TDependencies;
    FName, FExecutableName, FQualifiedName, FAuthor, FCaption: string;
    GatheringFilesVsData: boolean; //< only for GatherFile
    GatheringFiles: TCastleStringList; //< only for GatherFile
    ManifestFile, ProjectPath, DataPath: string;
    IncludePaths, ExcludePaths: TCastleStringList;
    Icons: TIconFileNames;
    IncludePathsRecursive: TBooleanList;
    FStandaloneSource, FAndroidSource, FPluginSource, FAndroidProject: string;
    DeletedFiles: Cardinal; //< only for DeleteFoundFile
    FVersion: string;
    FVersionCode: Cardinal;
    FScreenOrientation: TScreenOrientation;
    function PluginCompiledFile(const OS: TOS; const CPU: TCPU): string;
    procedure GatherFile(const FileInfo: TFileInfo);
    procedure AddDependency(const Dependency: TDependency; const FileInfo: TFileInfo);
    procedure FoundTtf(const FileInfo: TFileInfo);
    procedure FoundGz(const FileInfo: TFileInfo);
    procedure FoundPng(const FileInfo: TFileInfo);
    procedure FoundWav(const FileInfo: TFileInfo);
    procedure FoundOgg(const FileInfo: TFileInfo);
    procedure DeleteFoundFile(const FileInfo: TFileInfo);
    function PackageName(const OS: TOS; const CPU: TCPU): string;
    function SourcePackageName: string;
    { Output Android library resulting from compilation.
      Use only if AndroidSource <> ''.
      Relative to ProjectPath if Subdir = true, otherwise this is only
      a name without any directory part. }
    function AndroidLibraryFile(const Subdir: boolean): string;
    function ReplaceMacros(const Source: string): string;
    { Add platform-independent files that should be included in package,
      remove files that should be excluded.
      If OnlyData, then only takes stuff inside DataPath,
      and assumes that Files are (and will be) URLs relative to DataPath.
      Otherwise, takes more files,
      and assumes that Files are (and will be) URLs relative to ProjectPath. }
    procedure PackageFiles(const Files: TCastleStringList; const OnlyData: boolean);

    property Version: string read FVersion;
    property QualifiedName: string read FQualifiedName;
    property Dependencies: TDependencies read FDependencies;
    property Name: string read FName;
    property Caption: string read FCaption;
    property Author: string read FAuthor;
    property ExecutableName: string read FExecutableName;
    property StandaloneSource: string read FStandaloneSource;
    property AndroidSource: string read FAndroidSource;
    property PluginSource: string read FPluginSource;
    property AndroidProject: string read FAndroidProject;
    property ScreenOrientation: TScreenOrientation read FScreenOrientation;
  public
    constructor Create;
    constructor Create(const Path: string);
    destructor Destroy; override;

    procedure DoCreateManifest;
    procedure DoCompile(const OS: TOS; const CPU: TCPU; const Plugin: boolean; const Mode: TCompilationMode);
    procedure DoPackage(const OS: TOS; const CPU: TCPU; const Plugin: boolean; const Mode: TCompilationMode);
    procedure DoInstall(const OS: TOS; const CPU: TCPU; const Plugin: boolean);
    procedure DoPackageSource;
    procedure DoClean;
  end;

function DependencyToString(const D: TDependency): string;
function StringToDependency(const S: string): TDependency;

function ScreenOrientationToString(const O: TScreenOrientation): string;
function StringToScreenOrientation(const S: string): TScreenOrientation;

implementation

uses StrUtils, DOM, Process, Classes,
  CastleURIUtils, CastleXMLUtils, CastleWarnings, CastleFilesUtils,
  ToolPackage, ToolWindowsResources, ToolAndroidPackage, ToolWindowsRegistry;

{ TCastleProject ------------------------------------------------------------- }

const
  ManifestName = 'CastleEngineManifest.xml';

constructor TCastleProject.Create;
var
  { look for CastleEngineManifest.xml in this dir, or parents }
  Dir, ParentDir: string;
begin
  Dir := GetCurrentDir;
  while not FileExists(InclPathDelim(Dir) + ManifestName) do
  begin
    ParentDir := ExtractFileDir(ExclPathDelim(Dir));
    if (ParentDir = '') or (ParentDir = Dir) then
    begin
      { no parent directory, give up, assume auto-guessed values in current dir }
      Create(GetCurrentDir);
      Exit;
    end;
    {if Verbose then
      Writeln('Manifest not found, looking in parent directory: ', ParentDir);}
    Dir := ParentDir;
  end;
  Create(Dir);
end;

const
  DataName = 'data';

constructor TCastleProject.Create(const Path: string);

  procedure ReadManifest;
  const
    { Google Play requires version code to be >= 1 }
    DefautVersionCode = 1;

    procedure AutoGuessManifest;
    begin
      Writeln('Manifest file not found: ' + ManifestFile);
      Writeln('Guessing project values. Use create-manifest command to write these guesses into new CastleEngineManifest.xml');
      FName := ExtractFileName(ExtractFileDir(ManifestFile));
      FCaption := FName;
      FQualifiedName := 'unknown.' + FName;
      FExecutableName := FName;
      FStandaloneSource := FName + '.lpr';
      FVersionCode := DefautVersionCode;
      Icons.BaseUrl := FilenameToURISafe(InclPathDelim(GetCurrentDir));
    end;

    procedure CheckManifestCorrect;

      procedure CheckMatches(const Name, Value: string; const AllowedChars: TSetOfChars);
      var
        I: Integer;
      begin
        for I := 1 to Length(Value) do
          if not (Value[I] in AllowedChars) then
            raise Exception.CreateFmt('Project %s contains invalid characters: "%s", this character is not allowed: "%s"',
              [Name, Value, SReadableForm(Value[I])]);
      end;

    const
      ControlChars = [#0..Chr(Ord(' ')-1)];
      AlphaNum = ['a'..'z','A'..'Z','0'..'9'];
    begin
      CheckMatches('name', Name                     , AlphaNum + ['_','-']);
      CheckMatches('executable_name', ExecutableName, AlphaNum + ['_','-']);

      { non-filename stuff: allow also dots }
      CheckMatches('version', Version             , AlphaNum + ['_','-','.']);
      CheckMatches('qualified_name', QualifiedName, AlphaNum + ['_','-','.']);

      { more user-visible stuff, where we allow spaces, local characters and so on }
      CheckMatches('caption', Caption, AllChars - ControlChars);
      CheckMatches('author', Author  , AllChars - ControlChars);
    end;

  var
    Doc: TXMLDocument;
    ManifestURL: string;
    ChildElements: TDOMNodeList;
    Element, ChildElement: TDOMElement;
    I: Integer;
  begin
    ManifestFile := ProjectPath + ManifestName;
    if not FileExists(ManifestFile) then
      AutoGuessManifest else
    begin
      if Verbose then
        Writeln('Manifest file found: ' + ManifestFile);
      ManifestURL := FilenameToURISafe(ManifestFile);
      Icons.BaseUrl := ManifestURL;

      try
        URLReadXML(Doc, ManifestURL);
        Check(Doc.DocumentElement.TagName = 'project',
          'Root node of CastleEngineManifest.xml must be <project>');
        FName := Doc.DocumentElement.AttributeString('name');
        FCaption := Doc.DocumentElement.AttributeStringDef('caption', FName);
        FQualifiedName := Doc.DocumentElement.AttributeStringDef('qualified_name', 'unknown.' + FName);
        FExecutableName := Doc.DocumentElement.AttributeStringDef('executable_name', FName);
        FStandaloneSource := Doc.DocumentElement.AttributeStringDef('standalone_source', '');
        FAndroidSource := Doc.DocumentElement.AttributeStringDef('android_source', '');
        FPluginSource := Doc.DocumentElement.AttributeStringDef('plugin_source', '');
        FAndroidProject := Doc.DocumentElement.AttributeStringDef('android_project', '');
        FAuthor := Doc.DocumentElement.AttributeStringDef('author', '');
        FScreenOrientation := StringToScreenOrientation(
          Doc.DocumentElement.AttributeStringDef('screen_orientation', 'any'));

        Element := DOMGetChildElement(Doc.DocumentElement, 'version', false);
        FVersionCode := DefautVersionCode;
        if Element <> nil then
        begin
          FVersion := Element.AttributeString('value');
          FVersionCode := Element.AttributeCardinalDef('code', DefautVersionCode);
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

        Element := DOMGetChildElement(Doc.DocumentElement, 'icons', false);
        if Element <> nil then
        begin
          ChildElements := Element.GetElementsByTagName('icon');
          for I := 0 to ChildElements.Count - 1 do
          begin
            ChildElement := ChildElements[I] as TDOMElement;
            Icons.Add(ChildElement.AttributeString('path'));
          end;
        end;
      finally FreeAndNil(Doc) end;
    end;

    CheckManifestCorrect;
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
    DependenciesClosure(depFreetype, depZlib);
    DependenciesClosure(depOggVorbis, depSound);
  end;

begin
  inherited Create;

  { empty initial state }
  IncludePaths := TCastleStringList.Create;
  IncludePathsRecursive := TBooleanList.Create;
  ExcludePaths := TCastleStringList.Create;
  FDependencies := [];
  Icons := TIconFileNames.Create;

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
  FreeAndNil(Icons);
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
var
  RelativeVs: string;
begin
  if GatheringFilesVsData then
    RelativeVs := DataPath else
    RelativeVs := ProjectPath;
  GatheringFiles.Add(ExtractRelativePath(RelativeVs, FileInfo.AbsoluteName));
end;

procedure TCastleProject.DoCreateManifest;
var
  Contents: string;
begin
  if FileExists(ManifestFile) then
    raise Exception.CreateFmt('Manifest file "%s" already exists, refusing to overwrite it',
      [ManifestFile]);
  Contents := '<?xml version="1.0" encoding="utf-8"?>' +NL+
'<project name="' + Name + '" standalone_source="' + StandaloneSource + '">' +NL+
'</project>' + NL;
  StringToFile(ManifestFile, Contents);
  Writeln('Created manifest ' + ManifestFile);
end;

procedure TCastleProject.DoCompile(const OS: TOS; const CPU: TCPU; const Plugin: boolean; const Mode: TCompilationMode);

  function InsertLibPrefix(const S: string): string;
  begin
    Result := {$ifdef UNIX} ExtractFilePath(S) + 'lib' + ExtractFileName(S)
              {$else} S
              {$endif};
  end;

var
  SourceExe, DestExe, MainSource: string;
begin
  Writeln(Format('Compiling project "%s" for OS / CPU "%s / %s" in mode "%s"%s.',
    [Name, OSToString(OS), CPUToString(CPU), ModeToString(Mode),
     Iff(Plugin, ' (as a plugin)', '')]));

  case OS of
    Android:
      begin
        if AndroidSource = '' then
          raise Exception.Create('android_source property for project not defined, cannot compile Android version');
        Compile(OS, CPU, Plugin, Mode, ProjectPath, AndroidSource);
        Writeln('Compiled library for Android in ', AndroidLibraryFile(true));
      end;
    else
      begin
        if Plugin then
        begin
          MainSource := PluginSource;
          if MainSource = '' then
            raise Exception.Create('plugin_source property for project not defined, cannot compile plugin version');
        end else
        begin
          MainSource := StandaloneSource;
          if MainSource = '' then
            raise Exception.Create('standalone_source property for project not defined, cannot compile standalone version');
        end;

        if OS in AllWindowsOSes then
          GenerateWindowsResources(@ReplaceMacros, ProjectPath, Icons, CPU, Plugin);

        Compile(OS, CPU, Plugin, Mode, ProjectPath, MainSource);

        if Plugin then
        begin
          SourceExe := InsertLibPrefix(ChangeFileExt(PluginSource, LibraryExtensionOS(OS)));
          { "np" prefix is safest for plugin library files. }
          DestExe := PluginCompiledFile(OS, CPU);
        end else
        begin
          SourceExe := ChangeFileExt(StandaloneSource, ExeExtensionOS(OS));
          DestExe := ChangeFileExt(ExecutableName, ExeExtensionOS(OS));
        end;
        if not AnsiSameText(SourceExe, DestExe) then
        begin
          { move exe to top-level (in case StandaloneSource is in subdirectory
            like code/) and eventually rename to follow ExecutableName }
          Writeln('Moving ', SourceExe, ' to ', DestExe);
          CheckRenameFile(ProjectPath + SourceExe, ProjectPath + DestExe);
        end;
      end;
  end;
end;

function TCastleProject.PluginCompiledFile(const OS: TOS; const CPU: TCPU): string;
begin
  Result := ExtractFilePath(ExecutableName) + 'np' +
    DeleteFileExt(ExtractFileName(ExecutableName)) + '.' +
    OSToString(OS) + '-' + CPUToString(CPU) + LibraryExtensionOS(OS);
end;

procedure TCastleProject.PackageFiles(const Files: TCastleStringList; const OnlyData: boolean);

  procedure Exclude(const PathMask: string; const Files: TCastleStringList);
  const
    IgnoreCase = true;
  var
    I: Integer;
    PathMaskSlashes, ItemSlashes: string;
  begin
    { replace all backslashes with slashes, so that they are equal for comparison }
    PathMaskSlashes := StringReplace(PathMask, '\', '/', [rfReplaceAll]);
    { Files are relative to data/ in case of OnlyData.
      So make sure that PathMaskSlashes is also relative to data/,
      otherwise stuff like exclude="data/blahblah/*" would not work
      for things that se OnlyData=true, e.g. for Android packaging. }
    if OnlyData then
      PathMaskSlashes := PrefixRemove(DataName + '/', PathMaskSlashes, IgnoreCase);
    I := 0;
    while I < Files.Count do
    begin
      ItemSlashes := StringReplace(Files[I], '\', '/', [rfReplaceAll]);
      if IsWild(ItemSlashes, PathMaskSlashes, IgnoreCase) then
        Files.Delete(I) else
        Inc(I);
    end;
  end;

var
  I: Integer;
  FindOptions: TFindFilesOptions;
begin
  GatheringFiles := Files;
  GatheringFilesVsData := OnlyData;
  FindFiles(DataPath, '*', false, @GatherFile, [ffRecursive]);

  if not OnlyData then
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
end;

procedure TCastleProject.DoPackage(const OS: TOS; const CPU: TCPU; const Plugin: boolean;
  const Mode: TCompilationMode);
var
  Pack: TPackageDirectory;

  procedure AddExternalLibrary(const LibraryName: string);
  var
    CastleEnginePath, LibraryPath,
      ExternalLibrariesPath1, ExternalLibrariesPath2, ExternalLibrariesPath: string;
  begin
    CastleEnginePath := GetEnvironmentVariable('CASTLE_ENGINE_PATH');
    if CastleEnginePath = '' then
      raise Exception.Create('CASTLE_ENGINE_PATH environment variable not defined, we cannot find required library ' + LibraryName);

    ExternalLibrariesPath1 := InclPathDelim(CastleEnginePath) + 'external_libraries';
    ExternalLibrariesPath2 := InclPathDelim(CastleEnginePath) + 'external-libraries';
    if DirectoryExists(ExternalLibrariesPath1) then
      ExternalLibrariesPath := ExternalLibrariesPath1 else
    if DirectoryExists(ExternalLibrariesPath2) then
      ExternalLibrariesPath := ExternalLibrariesPath2 else
      raise Exception.Create('CASTLE_ENGINE_PATH environment variable defined, but we cannot find "external libraries" directory inside, searched in: "' + ExternalLibrariesPath1 + '" and "' + ExternalLibrariesPath2 + '"');

    LibraryPath := ExternalLibrariesPath + PathDelim +
      CPUToString(CPU) + '-' + OSToString(OS) + PathDelim + LibraryName;
    if not FileExists(LibraryPath) then
      raise Exception.Create('Dependency library not found in ' + LibraryPath);
    Pack.Add(LibraryPath, LibraryName);
  end;

var
  Files: TCastleStringList;
  I: Integer;
  PackageFileName, ExecutableNameExt, AndroidProjectPath: string;
  UnixPermissionsMatter: boolean;
begin
  Writeln(Format('Packaging project "%s" for OS / CPU "%s / %s"%s.',
    [Name, OSToString(OS), CPUToString(CPU),
     Iff(Plugin, ' (as a plugin)', '')]));

  if Plugin then
    raise Exception.Create('The "package" command is not useful to package plugins for now');

  { for Android, the packaging process is special }
  if OS = Android then
  begin
    if AndroidSource = '' then
      raise Exception.Create('Cannot create Android package, because Android library source (android_source) is not set in CastleEngineManifest.xml');
    Files := TCastleStringList.Create;
    try
      PackageFiles(Files, true);
      { use the AndroidProject value (just make it safer) for AndroidProjectPath,
        if set }
      if AndroidProject <> '' then
        AndroidProjectPath := InclPathDelim(
          StringReplace(AndroidProject, '\', PathDelim, [rfReplaceAll])) else
        AndroidProjectPath := '';
      CreateAndroidPackage(Mode, Name, @ReplaceMacros, Icons, DataPath, Files,
        ProjectPath + AndroidLibraryFile(true),
        AndroidLibraryFile(false), ProjectPath, AndroidProjectPath);
    finally FreeAndNil(Files) end;
    Exit;
  end;

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

      PackageFiles(Files, false);

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
          begin
            AddExternalLibrary('OpenAL32.dll');
            AddExternalLibrary('wrap_oal.dll');
          end;
          if depOggVorbis in Dependencies then
          begin
            AddExternalLibrary('libogg.dll');
            AddExternalLibrary('libvorbis.dll');
            { AddExternalLibrary('vorbisenc.dll'); not present? }
            AddExternalLibrary('vorbisfile.dll');
          end;
        end;
    end;

    PackageFileName := PackageName(OS, CPU);

    if OS in AllWindowsOSes then
      Pack.Make(ProjectPath, PackageFileName, ptZip) else
      Pack.Make(ProjectPath, PackageFileName, ptTarGz);
  finally FreeAndNil(Pack) end;
end;

procedure TCastleProject.DoInstall(const OS: TOS; const CPU: TCPU; const Plugin: boolean);

  procedure InstallUnixPlugin;
  const
    TargetPathSystemWide = '/usr/lib/mozilla/plugins/';
  var
    PluginFile, Source, Target: string;
  begin
    PluginFile := PluginCompiledFile(OS, CPU);
    Source := InclPathDelim(ProjectPath) + PluginFile;
    Target := TargetPathSystemWide + PluginFile;
    try
      SmartCopyFile(Source, Target);
      Writeln('Installed system-wide by copying the plugin to "' + Target + '".');
    except
      on E: Exception do
      begin
        Writeln('Failed to install system-wide (' + E.ClassName + ': ' + E.Message + ').');
        Target := HomePath + '.mozilla/plugins/' + PluginFile;
        SmartCopyFile(Source, Target);
        Writeln('Installed to "' + Target + '".');
      end;
    end;
  end;

begin
  Writeln(Format('Installing project "%s" for OS / CPU "%s / %s"%s.',
    [Name, OSToString(OS), CPUToString(CPU),
     Iff(Plugin, ' (as a plugin)', '')]));

  if OS = Android then
    InstallAndroidPackage(Name, QualifiedName) else
  if Plugin and (OS in AllWindowsOSes) then
    InstallWindowsPluginRegistry(Name, QualifiedName, ProjectPath,
      PluginCompiledFile(OS, CPU), Version, Author) else
  if Plugin and (OS in AllUnixOSes) then
    InstallUnixPlugin else
    raise Exception.Create('The "install" command is not useful for this OS / CPU right now. Install and run the application manually.');
end;

procedure TCastleProject.DoPackageSource;
var
  Pack: TPackageDirectory;
  Files: TCastleStringList;
  I: Integer;
  PackageFileName: string;
  IsPackageName: boolean;
  OS: TOS;
  CPU: TCPU;
begin
  Writeln(Format('Packaging source code of project "%s".', [Name]));

  Pack := TPackageDirectory.Create(Name);
  try
    Files := TCastleStringList.Create;
    try
      GatheringFiles := Files;
      FindFiles(ProjectPath, '*', false, @GatherFile, [ffRecursive]);
      GatheringFiles := nil;
      for I := 0 to Files.Count - 1 do
      begin
        { Do not pack packages (binary or source) into the source package.
          The packages are not cleaned by DoClean, so they could otherwise
          be packed by accident. }
        IsPackageName := false;
        for OS in TOS do
          for CPU in TCPU do
            if OSCPUSupported[OS, CPU] then
              if AnsiCompareFileName(Files[I], PackageName(OS, CPU)) = 0 then
                IsPackageName := true;
        if (AnsiCompareFileName(Files[I], SourcePackageName) = 0) or
           { avoid Android packages }
           (AnsiCompareFileName(Files[I], Name + '-debug.apk') = 0) or
           (AnsiCompareFileName(Files[I], Name + '-release.apk') = 0) then
          IsPackageName := true;

        if (not IsPackageName) and
           { do not pack AndroidAntProperties.txt with private stuff }
           (Files[I] <> 'AndroidAntProperties.txt') then
          Pack.Add(ProjectPath + Files[I], Files[I]);
      end;
    finally FreeAndNil(Files) end;

    PackageFileName := SourcePackageName;
    Pack.Make(ProjectPath, PackageFileName, ptTarGz);
  finally FreeAndNil(Pack) end;
end;

function TCastleProject.PackageName(
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

function TCastleProject.SourcePackageName: string;
begin
  Result := Name;
  if Version <> '' then
    Result += '-' + Version;
  Result += '-src';
  Result += '.tar.gz';
end;

procedure TCastleProject.DeleteFoundFile(const FileInfo: TFileInfo);
begin
  if Verbose then
    Writeln('Deleting ' + FileInfo.AbsoluteName);
  CheckDeleteFile(FileInfo.AbsoluteName);
  Inc(DeletedFiles);
end;

function TCastleProject.AndroidLibraryFile(const Subdir: boolean): string;
begin
  Result := '';
  if Subdir then
    Result += ExtractFilePath(AndroidSource);
  Result += 'lib' + ChangeFileExt(ExtractFileName(AndroidSource), '.so');
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

var
  OS: TOS;
  CPU: TCPU;
begin
  if StandaloneSource <> '' then
  begin
    TryDeleteFile(ChangeFileExt(ExecutableName, ''));
    TryDeleteFile(ChangeFileExt(ExecutableName, '.exe'));
  end;
  if AndroidSource <> '' then
    TryDeleteFile(AndroidLibraryFile(true));

  { packages created by DoPackage? Or not, it's safer to not remove them. }
  {
  for OS in TOS do
    for CPU in TCPU do
      if OSCPUSupported[OS, CPU] then
        TryDeleteFile(PackageName(OS, CPU));
  }

  { possible plugin outputs }
  if PluginSource <> '' then
    for OS in TOS do
      for CPU in TCPU do
        if OSCPUSupported[OS, CPU] then
          TryDeleteFile(PluginCompiledFile(OS, CPU));

  { compilation and editor backups }
  DeleteFilesRecursive('*~'); // editor backup, e.g. Emacs
  DeleteFilesRecursive('*.ppu'); // compilation
  DeleteFilesRecursive('*.o'); // compilation
  DeleteFilesRecursive('*.or'); // compilation
  DeleteFilesRecursive('*.compiled'); // Lazarus compilation
  DeleteFilesRecursive('*.rst'); // resource strings
  DeleteFilesRecursive('*.rsj'); // resource strings

  { our own trash. Note that we do not remove .res file, it can be committed,
    otherwise compilation without using castle-engine tool will not be easily possible. }
  TryDeleteFile('plugin-automatic-windows-resources.rc');
  TryDeleteFile('automatic-windows-resources.rc');
  TryDeleteFile('automatic-windows.manifest');

  Writeln('Deleted ', DeletedFiles, ' files');
end;

function TCastleProject.ReplaceMacros(const Source: string): string;

  { Make CamelCase with only safe characters (digits and letters). }
  function MakeCamelCase(S: string): string;
  var
    I: Integer;
  begin
    S := SReplaceChars(S, AllChars - ['a'..'z', 'A'..'Z', '0'..'9'], ' ');
    Result := '';
    for I := 1 to Length(S) do
      if S[I] <> ' ' then
        if (I > 1) and (S[I - 1] <> ' ') then
          Result += S[I] else
          Result += UpCase(S[I]);
  end;

const
  AndroidScreenOrientation: array [TScreenOrientation] of string =
  ('unspecified', 'sensorLandscape', 'sensorPortrait');
  AndroidScreenOrientationFeature: array [TScreenOrientation] of string =
  ('',
   '<uses-feature android:name="android.hardware.screen.landscape"/>',
   '<uses-feature android:name="android.hardware.screen.portrait"/>');

var
  Patterns, Values: TCastleStringList;
  I: Integer;
  P, NonEmptyAuthor: string;
  VersionComponents: array [0..3] of Cardinal;
  VersionComponentsString: TCastleStringList;
begin
  { calculate version as 4 numbers, Windows resource/manifest stuff expect this }
  VersionComponentsString := CreateTokens(Version, ['.']);
  try
    for I := 0 to High(VersionComponents) do
      if I < VersionComponentsString.Count then
        VersionComponents[I] := StrToIntDef(Trim(VersionComponentsString[I]), 0) else
        VersionComponents[I] := 0;
  finally FreeAndNil(VersionComponentsString) end;

  if Author = '' then
    NonEmptyAuthor := 'Unknown Author' else
    NonEmptyAuthor := Author;

  Patterns := nil;
  Values := nil;
  try
    Patterns := TCastleStringList.Create;
    Values := TCastleStringList.Create;
    Patterns.Add('VERSION_MAJOR');   Values.Add(IntToStr(VersionComponents[0]));
    Patterns.Add('VERSION_MINOR');   Values.Add(IntToStr(VersionComponents[1]));
    Patterns.Add('VERSION_RELEASE'); Values.Add(IntToStr(VersionComponents[2]));
    Patterns.Add('VERSION_BUILD');   Values.Add(IntToStr(VersionComponents[3]));
    Patterns.Add('VERSION');         Values.Add(Version);
    Patterns.Add('VERSION_CODE');    Values.Add(IntToStr(FVersionCode));
    Patterns.Add('NAME');            Values.Add(Name);
    Patterns.Add('QUALIFIED_NAME');  Values.Add(QualifiedName);
    Patterns.Add('CAPTION');         Values.Add(Caption);
    Patterns.Add('AUTHOR');          Values.Add(NonEmptyAuthor);
    Patterns.Add('EXECUTABLE_NAME'); Values.Add(ExecutableName);
    Patterns.Add('ANDROID_LIBRARY_NAME'); Values.Add(ChangeFileExt(ExtractFileName(AndroidSource), ''));
    Patterns.Add('ANDROID_SCREEN_ORIENTATION'); Values.Add(AndroidScreenOrientation[ScreenOrientation]);
    Patterns.Add('ANDROID_SCREEN_ORIENTATION_FEATURE'); Values.Add(AndroidScreenOrientationFeature[ScreenOrientation]);
    // add CamelCase() replacements, add ${} around
    for I := 0 to Patterns.Count - 1 do
    begin
      P := Patterns[I];
      Patterns[I] := '${' + P + '}';
      Patterns.Add('${CamelCase(' + P + ')}');
      Values.Add(MakeCamelCase(Values[I]));
    end;
    Result := SReplacePatterns(Source, Patterns, Values, []);
  finally
    FreeAndNil(Patterns);
    FreeAndNil(Values);
  end;
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
  ScreenOrientationNames: array [TScreenOrientation] of string =
  ('any', 'landscape', 'portrait');

function ScreenOrientationToString(const O: TScreenOrientation): string;
begin
  Result := ScreenOrientationNames[O];
end;

function StringToScreenOrientation(const S: string): TScreenOrientation;
begin
  for Result in TScreenOrientation do
    if AnsiSameText(ScreenOrientationNames[Result], S) then
      Exit;
  raise Exception.CreateFmt('Invalid orientation name "%s"', [S]);
end;

end.
