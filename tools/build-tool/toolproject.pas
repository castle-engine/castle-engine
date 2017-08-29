{
  Copyright 2014-2017 Michalis Kamburelis.

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

uses SysUtils, Classes,
  CastleFindFiles, CastleStringUtils, CastleUtils,
  ToolArchitectures, ToolCompile, ToolUtils, ToolAndroidServices;

type
  TDependency = (depFreetype, depZlib, depPng, depSound, depOggVorbis);
  TDependencies = set of TDependency;

  TScreenOrientation = (soAny, soLandscape, soPortrait);

  TAndroidProjectType = (apBase, apIntegrated);

  ECannotGuessManifest = class(Exception);

  TCastleProject = class
  private
    FDependencies: TDependencies;
    FName, FExecutableName, FQualifiedName, FAuthor, FCaption: string;
    GatheringFilesVsData: boolean; //< only for GatherFile
    GatheringFiles: TCastleStringList; //< only for GatherFile
    ManifestFile, FPath, FDataPath: string;
    IncludePaths, ExcludePaths: TCastleStringList;
    FIcons, FLaunchImages: TImageFileNames;
    FSearchPaths: TStringList;
    IncludePathsRecursive: TBooleanList;
    FStandaloneSource, FAndroidSource, FIOSSource, FPluginSource: string;
    FGameUnits: string;
    DeletedFiles: Cardinal; //< only for DeleteFoundFile
    FVersion: string;
    FVersionCode: Cardinal;
    FScreenOrientation: TScreenOrientation;
    FAndroidBuildToolsVersion: string;
    FAndroidCompileSdkVersion, FAndroidMinSdkVersion, FAndroidTargetSdkVersion: Cardinal;
    FAndroidProjectType: TAndroidProjectType;
    FAndroidServices: TAndroidServiceList;
    // Helpers only for ExtractTemplateFoundFile.
    ExtractTemplateDestinationPath, ExtractTemplateDir: string;
    procedure GatherFile(const FileInfo: TFileInfo; var StopSearch: boolean);
    procedure AddDependency(const Dependency: TDependency; const FileInfo: TFileInfo);
    procedure DeleteFoundFile(const FileInfo: TFileInfo; var StopSearch: boolean);
    function PackageName(const OS: TOS; const CPU: TCPU): string;
    function SourcePackageName: string;
    procedure ExtractTemplateFoundFile(const FileInfo: TFileInfo; var StopSearch: boolean);

    { Convert Name to a valid Pascal identifier. }
    function NamePascal: string;

    { Extract a single file using the template system.
      SourceFileName and DestinationFileName should be absolute filenames
      of source and destination files.
      DestinationRelativeFileName should be a relative version of DestinationFileName,
      relative to the template root.

      This is used internally by ExtractTemplateFoundFile, which is in turn used
      by ExtractTemplate that can extract a whole template directory.

      It can also be used directly to expand a single file. }
    procedure ExtractTemplateFile(
      const SourceFileName, DestinationFileName, DestinationRelativeFileName: string;
      const OverrideExisting: boolean);

    { Generate a program/library file from template. }
    procedure GeneratedSourceFile(
      const TemplateRelativeURL, TargetRelativePath, ErrorMessageMissingGameUnits: string;
      const CreateIfNecessary: boolean;
      out RelativeResult, AbsoluteResult: string);

    { Android source specified in CastleEngineManifest.xml.
      Most code should use AndroidSourceFile instead, that can optionally
      auto-create Android source file. }
    property AndroidSource: string read FAndroidSource;
    function AndroidSourceFile(const AbsolutePath, CreateIfNecessary: boolean): string;

    { iOS source specified in CastleEngineManifest.xml.
      Most code should use IOSSourceFile instead, that can optionally
      auto-create iOS source file. }
    property IOSSource: string read FIOSSource;
    function IOSSourceFile(const AbsolutePath, CreateIfNecessary: boolean): string;

    { Standalone source specified in CastleEngineManifest.xml.
      Most code should use StandaloneSourceFile instead, that can optionally
      auto-create the source file. }
    property StandaloneSource: string read FStandaloneSource;
    function StandaloneSourceFile(const AbsolutePath, CreateIfNecessary: boolean): string;

    { Plugin source specified in CastleEngineManifest.xml.
      Most code should use PluginSourceFile instead, that can optionally
      auto-create the source file. }
    property PluginSource: string read FPluginSource;
    function PluginSourceFile(const AbsolutePath, CreateIfNecessary: boolean): string;
    function PluginLibraryFile(const OS: TOS; const CPU: TCPU): string;
  public
    constructor Create;
    constructor Create(const APath: string);
    destructor Destroy; override;

    { Commands on a project, used by the main program code. }
    { }

    procedure DoCreateManifest;
    procedure DoCompile(const Target: TTarget; const OS: TOS; const CPU: TCPU; const Plugin: boolean; const Mode: TCompilationMode);
    procedure DoPackage(const Target: TTarget; const OS: TOS; const CPU: TCPU; const Plugin: boolean; const Mode: TCompilationMode);
    procedure DoInstall(const Target: TTarget; const OS: TOS; const CPU: TCPU; const Plugin: boolean);
    procedure DoRun(const Target: TTarget; const OS: TOS; const CPU: TCPU; const Plugin: boolean; const Params: TCastleStringList);
    procedure DoPackageSource;
    procedure DoClean;
    procedure DoAutoGenerateTextures;
    procedure DoAutoGenerateClean;
    procedure DoGenerateProgram;

    { Detailed information about the project, read-only and useful for
      various project operations. }
    { }

    property Version: string read FVersion;
    property QualifiedName: string read FQualifiedName;
    property Dependencies: TDependencies read FDependencies;
    property Name: string read FName;
    { Project path. Always ends with path delimiter, like a slash or backslash. }
    property Path: string read FPath;
    { Project data path. Always ends with path delimiter, like a slash or backslash. }
    property DataPath: string read FDataPath;
    property Caption: string read FCaption;
    property Author: string read FAuthor;
    property ExecutableName: string read FExecutableName;
    property ScreenOrientation: TScreenOrientation read FScreenOrientation;
    property AndroidCompileSdkVersion: Cardinal read FAndroidCompileSdkVersion;
    property AndroidBuildToolsVersion: string read FAndroidBuildToolsVersion;
    property AndroidMinSdkVersion: Cardinal read FAndroidMinSdkVersion;
    property AndroidTargetSdkVersion: Cardinal read FAndroidTargetSdkVersion;
    property AndroidProjectType: TAndroidProjectType read FAndroidProjectType;
    property Icons: TImageFileNames read FIcons;
    property LaunchImages: TImageFileNames read FLaunchImages;
    property SearchPaths: TStringList read FSearchPaths;
    property AndroidServices: TAndroidServiceList read FAndroidServices;

    { Path to the external library in data/external_libraries/ .
      Right now, these host various Windows-specific DLL files.
      This checks existence of appropriate files along the way,
      and raises exception in case of trouble. }
    function ExternalLibraryPath(const OS: TOS; const CPU: TCPU; const LibraryName: string): string;

    function ReplaceMacros(const Source: string): string;

    { Recursively copy a directory from TemplatePath (this is relative
      to the build tool data) to the DestinationPath (this should be an absolute
      existing directory name).

      Each file is processed by the ReplaceMacros method. }
    procedure ExtractTemplate(const TemplatePath, DestinationPath: string);

    { Output Android library resulting from compilation.
      Relative to @link(Path) if AbsolutePath = @false,
      otherwise a complete absolute path. }
    function AndroidLibraryFile(const AbsolutePath: boolean = true): string;

    { Add platform-independent files that should be included in package,
      remove files that should be excluded.
      If OnlyData, then only takes stuff inside DataPath,
      and assumes that Files are (and will be) URLs relative to DataPath.
      Otherwise, takes more files,
      and assumes that Files are (and will be) URLs relative to @link(Path). }
    procedure PackageFiles(const Files: TCastleStringList; const OnlyData: boolean);

    { Output iOS library resulting from compilation.
      Relative to @link(Path) if AbsolutePath = @false,
      otherwise a complete absolute path. }
    function IOSLibraryFile(const AbsolutePath: boolean = true): string;
  end;

function DependencyToString(const D: TDependency): string;
function StringToDependency(const S: string): TDependency;

function ScreenOrientationToString(const O: TScreenOrientation): string;
function StringToScreenOrientation(const S: string): TScreenOrientation;

implementation

uses StrUtils, DOM, Process,
  CastleURIUtils, CastleXMLUtils, CastleLog, CastleFilesUtils,
  ToolPackage, ToolWindowsResources, ToolAndroidPackage, ToolWindowsRegistry,
  ToolTextureGeneration, ToolIOS;

const
  SErrDataDir = 'Make sure you have installed the data files of the Castle Game Engine build tool. Usually it is easiest to set the $CASTLE_ENGINE_PATH environment variable to the location of castle_game_engine/ or castle-engine/ directory, the build tool will then find its data correctly. Or place the data in system-wide location /usr/share/castle-engine/ or /usr/local/share/castle-engine/.';

{ Insert 'lib' prefix at the beginning of file name. }
function InsertLibPrefix(const S: string): string;
begin
  Result := ExtractFilePath(S) + 'lib' + ExtractFileName(S);
end;

{ Compiled library name (.so, .dll etc.) from given source code filename. }
function CompiledLibraryFile(const S: string; const OS: TOS): string;
begin
  Result := ChangeFileExt(S, LibraryExtensionOS(OS));
  if OS in AllUnixOSes then
    Result := InsertLibPrefix(Result);
end;

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

constructor TCastleProject.Create(const APath: string);

  procedure ReadManifest;
  const
    { Google Play requires version code to be >= 1 }
    DefautVersionCode = 1;
    DefaultAndroidCompileSdkVersion = 23;
    DefaultAndroidBuildToolsVersion = '23.0.2';
    { We need OpenGL ES 2.0, which means Android 2.0 (API Level 5) and higher.
      We want also NativeActivity and EGL, which require API level 9 or higher. }
    ReallyMinSdkVersion = 9;
    DefaultAndroidMinSdkVersion = 9;
    DefaultAndroidTargetSdkVersion = 18;

    { character sets }
    ControlChars = [#0..Chr(Ord(' ')-1)];
    AlphaNum = ['a'..'z','A'..'Z','0'..'9'];

    { qualified_name is also a Java package name for Android, so it
      cannot contain dash character.

      As for underscore:
      On Android, using _ is allowed,
      but on iOS it is not (it fails at signing),
      possibly because _ inside URLs is (in general) not allowed:
      http://stackoverflow.com/questions/2180465/can-domain-name-subdomains-have-an-underscore-in-it }
    QualifiedNameAllowedChars = AlphaNum + ['.'];

    function DefaultQualifiedName: string;
    begin
      Result := 'unknown.' + SDeleteChars(FName, AllChars - QualifiedNameAllowedChars);
    end;

    procedure AutoGuessManifest;

      function GuessName: string;
      var
        FileInfo: TFileInfo;
      begin
        Result := ExtractFileName(ExtractFileDir(ManifestFile));
        if not FileExists(Result + '.lpr') then
        begin
          if FindFirstFile(GetCurrentDir, '*.lpr', false, [], FileInfo) then
            Result := DeleteFileExt(FileInfo.Name)
          else
          if FindFirstFile(GetCurrentDir, '*.dpr', false, [], FileInfo) then
            Result := DeleteFileExt(FileInfo.Name)
          else
            raise ECannotGuessManifest.Create('Cannot find any *.lpr or *.dpr file in this directory, cannot guess which file to compile.' + NL +
              'Please create a CastleEngineManifest.xml to instruct Castle Game Engine build tool how to build your project.');
        end;
      end;

    begin
      Writeln('Manifest file not found: ' + ManifestFile);
      Writeln('Guessing project values. Use create-manifest command to write these guesses into new CastleEngineManifest.xml');
      FName := GuessName;
      FCaption := FName;
      FQualifiedName := DefaultQualifiedName;
      FExecutableName := FName;
      FStandaloneSource := FName + '.lpr';
      FVersionCode := DefautVersionCode;
      Icons.BaseUrl := FilenameToURISafe(InclPathDelim(GetCurrentDir));
      LaunchImages.BaseUrl := FilenameToURISafe(InclPathDelim(GetCurrentDir));
      FAndroidCompileSdkVersion := DefaultAndroidCompileSdkVersion;
      FAndroidBuildToolsVersion := DefaultAndroidBuildToolsVersion;
      FAndroidMinSdkVersion := DefaultAndroidMinSdkVersion;
      FAndroidTargetSdkVersion := DefaultAndroidTargetSdkVersion;
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

      procedure CheckQualifiedNameServices(const QualifiedName: string);
      var
        Services: TStringList;
        I: Integer;
      begin
        if (QualifiedName <> '') and
           ((QualifiedName[1] = '.') or
            (QualifiedName[Length(QualifiedName)] = '.')) then
          raise Exception.CreateFmt('Project qualified_name cannot start or end with a dot: "%s"', [QualifiedName]);

        Services := SplitString(QualifiedName, '.');
        try
          for I := 0 to Services.Count - 1 do
          begin
            if Services[I] = '' then
              raise Exception.CreateFmt('qualified_name must contain a number of non-empty services separated with dots: "%s"', [QualifiedName]);
            if Services[I][1] in ['0'..'9'] then
              raise Exception.CreateFmt('qualified_name services must not start with a digit: "%s"', [QualifiedName]);
          end;
        finally FreeAndNil(Services) end;
      end;

    begin
      CheckMatches('name', Name                     , AlphaNum + ['_','-']);
      CheckMatches('executable_name', ExecutableName, AlphaNum + ['_','-']);

      { non-filename stuff: allow also dots }
      CheckMatches('version', Version             , AlphaNum + ['_','-','.']);
      CheckMatches('qualified_name', QualifiedName, QualifiedNameAllowedChars);
      CheckQualifiedNameServices(QualifiedName);

      { more user-visible stuff, where we allow spaces, local characters and so on }
      CheckMatches('caption', Caption, AllChars - ControlChars);
      CheckMatches('author', Author  , AllChars - ControlChars);

      if AndroidMinSdkVersion > AndroidTargetSdkVersion then
        raise Exception.CreateFmt('Android min_sdk_version %d is larger than target_sdk_version %d, this is incorrect',
          [AndroidMinSdkVersion, AndroidTargetSdkVersion]);

      if AndroidMinSdkVersion < ReallyMinSdkVersion then
        raise Exception.CreateFmt('Android min_sdk_version %d is too small. It must be >= %d for Castle Game Engine applications',
          [AndroidMinSdkVersion, ReallyMinSdkVersion]);
    end;

  var
    Doc: TXMLDocument;
    ManifestURL, AndroidProjectTypeStr: string;
    ChildElements: TXMLElementIterator;
    Element, ChildElement: TDOMElement;
  begin
    ManifestFile := Path + ManifestName;
    if not FileExists(ManifestFile) then
      AutoGuessManifest else
    begin
      if Verbose then
        Writeln('Manifest file found: ' + ManifestFile);
      ManifestURL := FilenameToURISafe(ManifestFile);
      Icons.BaseUrl := ManifestURL;
      LaunchImages.BaseUrl := ManifestURL;

      try
        URLReadXML(Doc, ManifestURL);
        Check(Doc.DocumentElement.TagName = 'project',
          'Root node of CastleEngineManifest.xml must be <project>');
        FName := Doc.DocumentElement.AttributeString('name');
        FCaption := Doc.DocumentElement.AttributeStringDef('caption', FName);
        FQualifiedName := Doc.DocumentElement.AttributeStringDef('qualified_name', DefaultQualifiedName);
        FExecutableName := Doc.DocumentElement.AttributeStringDef('executable_name', FName);
        FStandaloneSource := Doc.DocumentElement.AttributeStringDef('standalone_source', '');
        FAndroidSource := Doc.DocumentElement.AttributeStringDef('android_source', '');
        FIOSSource := Doc.DocumentElement.AttributeStringDef('ios_source', '');
        FPluginSource := Doc.DocumentElement.AttributeStringDef('plugin_source', '');
        FAuthor := Doc.DocumentElement.AttributeStringDef('author', '');
        FGameUnits := Doc.DocumentElement.AttributeStringDef('game_units', '');
        FScreenOrientation := StringToScreenOrientation(
          Doc.DocumentElement.AttributeStringDef('screen_orientation', 'any'));

        Element := Doc.DocumentElement.ChildElement('version', false);
        FVersionCode := DefautVersionCode;
        if Element <> nil then
        begin
          FVersion := Element.AttributeString('value');
          FVersionCode := Element.AttributeCardinalDef('code', DefautVersionCode);
        end;

        Element := Doc.DocumentElement.ChildElement('dependencies', false);
        if Element <> nil then
        begin
          ChildElements := Element.ChildrenIterator('dependency');
          try
            while ChildElements.GetNext do
            begin
              ChildElement := ChildElements.Current;
              Include(FDependencies,
                StringToDependency(ChildElement.AttributeString('name')));
            end;
          finally FreeAndNil(ChildElements) end;
        end;

        Element := Doc.DocumentElement.ChildElement('package', false);
        if Element <> nil then
        begin
          ChildElements := Element.ChildrenIterator('include');
          try
            while ChildElements.GetNext do
            begin
              ChildElement := ChildElements.Current;
              IncludePaths.Add(ChildElement.AttributeString('path'));
              IncludePathsRecursive.Add(ChildElement.AttributeBooleanDef('recursive', false));
            end;
          finally FreeAndNil(ChildElements) end;

          ChildElements := Element.ChildrenIterator('exclude');
          try
            while ChildElements.GetNext do
            begin
              ChildElement := ChildElements.Current;
              ExcludePaths.Add(ChildElement.AttributeString('path'));
            end;
          finally FreeAndNil(ChildElements) end;
        end;

        Element := Doc.DocumentElement.ChildElement('icons', false);
        if Element <> nil then
        begin
          ChildElements := Element.ChildrenIterator('icon');
          try
            while ChildElements.GetNext do
            begin
              ChildElement := ChildElements.Current;
              Icons.Add(ChildElement.AttributeString('path'));
            end;
          finally FreeAndNil(ChildElements) end;
        end;

        Element := Doc.DocumentElement.ChildElement('launch_images', false);
        if Element <> nil then
        begin
          ChildElements := Element.ChildrenIterator('image');
          try
            while ChildElements.GetNext do
            begin
              ChildElement := ChildElements.Current;
              LaunchImages.Add(ChildElement.AttributeString('path'));
            end;
          finally FreeAndNil(ChildElements) end;
        end;

        FAndroidCompileSdkVersion := DefaultAndroidCompileSdkVersion;
        FAndroidBuildToolsVersion := DefaultAndroidBuildToolsVersion;
        FAndroidMinSdkVersion := DefaultAndroidMinSdkVersion;
        FAndroidTargetSdkVersion := DefaultAndroidTargetSdkVersion;
        Element := Doc.DocumentElement.ChildElement('android', false);
        if Element <> nil then
        begin
          FAndroidCompileSdkVersion := Element.AttributeCardinalDef('compile_sdk_version', DefaultAndroidCompileSdkVersion);
          FAndroidBuildToolsVersion := Element.AttributeStringDef('build_tools_version', DefaultAndroidBuildToolsVersion);
          FAndroidMinSdkVersion := Element.AttributeCardinalDef('min_sdk_version', DefaultAndroidMinSdkVersion);
          FAndroidTargetSdkVersion := Element.AttributeCardinalDef('target_sdk_version', DefaultAndroidTargetSdkVersion);

          if Element.AttributeString('project_type', AndroidProjectTypeStr) then
          begin
            if AndroidProjectTypeStr = 'base' then
              FAndroidProjectType := apBase else
            if AndroidProjectTypeStr = 'integrated' then
              FAndroidProjectType := apIntegrated else
              raise Exception.CreateFmt('Invalid android project_type "%s"', [AndroidProjectTypeStr]);
          end;

          ChildElement := Element.ChildElement('components', false);
          if ChildElement <> nil then
          begin
            FAndroidServices.ReadCastleEngineManifest(ChildElement);
            WritelnWarning('Android', 'The name <components> is deprecated, use <services> now to refer to Android services');
          end;
          ChildElement := Element.ChildElement('services', false);
          if ChildElement <> nil then
            FAndroidServices.ReadCastleEngineManifest(ChildElement);
        end;

        Element := Doc.DocumentElement.ChildElement('compiler_options', false);
        if Element <> nil then
        begin
          Element := Element.ChildElement('search_paths', false);
          if Element <> nil then
          begin
            ChildElements := Element.ChildrenIterator('path');
            try
              while ChildElements.GetNext do
                FSearchPaths.Add(ChildElements.Current.AttributeString('value'));
            finally FreeAndNil(ChildElements) end;
          end;
        end;
      finally FreeAndNil(Doc) end;
    end;

    CheckManifestCorrect;
  end;

  procedure GuessDependencies;
  var
    FileInfo: TFileInfo;
  begin
    if DirectoryExists(DataPath) then
    begin
      if Verbose then
        Writeln('Found data in "' + DataPath + '"');
      if FindFirstFile(DataPath, '*.ttf', false, [ffRecursive], FileInfo) then
        AddDependency(depFreetype, FileInfo);
      if FindFirstFile(DataPath, '*.gz' , false, [ffRecursive], FileInfo) then
        AddDependency(depZlib, FileInfo);
      if FindFirstFile(DataPath, '*.png', false, [ffRecursive], FileInfo) then
        AddDependency(depPng, FileInfo);
      if FindFirstFile(DataPath, '*.wav', false, [ffRecursive], FileInfo) then
        AddDependency(depSound, FileInfo);
      if FindFirstFile(DataPath, '*.ogg', false, [ffRecursive], FileInfo) then
        AddDependency(depOggVorbis, FileInfo);
    end else
      Writeln('Data directory not found (tried "' + DataPath + '")');
  end;

  procedure CloseDependencies;

    procedure DependenciesClosure(const Dep, DepRequirement: TDependency);
    begin
      if (Dep in Dependencies) and not (DepRequirement in Dependencies) then
      begin
        if Verbose then
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

  function DependenciesToStr(const S: TDependencies): string;
  var
    D: TDependency;
  begin
    Result := '';
    for D in S do
    begin
      if Result <> '' then Result += ', ';
      Result += DependencyToString(D);
    end;
    Result := '[' + Result + ']';
  end;

begin
  inherited Create;

  { empty initial state }
  IncludePaths := TCastleStringList.Create;
  IncludePathsRecursive := TBooleanList.Create;
  ExcludePaths := TCastleStringList.Create;
  FDependencies := [];
  FIcons := TImageFileNames.Create;
  FLaunchImages := TImageFileNames.Create;
  FSearchPaths := TStringList.Create;
  FAndroidProjectType := apBase;
  FAndroidServices := TAndroidServiceList.Create(true);

  FPath := InclPathDelim(APath);
  FDataPath := InclPathDelim(Path + DataName);

  ReadManifest;
  GuessDependencies;
  CloseDependencies;
  if Verbose then
    Writeln('Project "' + Name + '" dependencies: ' + DependenciesToStr(Dependencies));
end;

destructor TCastleProject.Destroy;
begin
  FreeAndNil(IncludePaths);
  FreeAndNil(IncludePathsRecursive);
  FreeAndNil(ExcludePaths);
  FreeAndNil(FIcons);
  FreeAndNil(FLaunchImages);
  FreeAndNil(FSearchPaths);
  FreeAndNil(FAndroidServices);
  inherited;
end;

procedure TCastleProject.AddDependency(const Dependency: TDependency; const FileInfo: TFileInfo);
begin
  if not (Dependency in Dependencies) then
  begin
    if Verbose then
      Writeln('Automatically adding "' + DependencyToString(Dependency) +
        '" to dependencies because data contains file: ' + FileInfo.URL);
    Include(FDependencies, Dependency);
  end;
end;

procedure TCastleProject.GatherFile(const FileInfo: TFileInfo; var StopSearch: boolean);
var
  RelativeVs: string;
begin
  if GatheringFilesVsData then
    RelativeVs := DataPath else
    RelativeVs := Path;
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

procedure TCastleProject.DoCompile(const Target: TTarget;
  const OS: TOS; const CPU: TCPU; const Plugin: boolean; const Mode: TCompilationMode);
var
  SourceExe, DestExe, MainSource: string;
begin
  Writeln(Format('Compiling project "%s" for %s in mode "%s".',
    [Name, PlatformToString(Target, OS, CPU, Plugin), ModeToString(Mode)]));

  if Target = targetIOS then
  begin
    CompileIOS(Plugin, Mode, Path, IOSSourceFile(true, true), SearchPaths);
    LinkIOSLibrary(Path, IOSLibraryFile);
    Writeln('Compiled library for iOS in ', IOSLibraryFile(false));
    Exit;
  end;

  case OS of
    Android:
      begin
        Compile(OS, CPU, Plugin, Mode, Path, AndroidSourceFile(true, true), SearchPaths);
        Writeln('Compiled library for Android in ', AndroidLibraryFile(false));
      end;
    else
      begin
        if Plugin then
        begin
          MainSource := PluginSourceFile(false, true);
          if MainSource = '' then
            raise Exception.Create('plugin_source property for project not defined, cannot compile plugin version');
        end else
        begin
          MainSource := StandaloneSourceFile(false, true);
          if MainSource = '' then
            raise Exception.Create('standalone_source property for project not defined, cannot compile standalone version');
        end;

        if OS in AllWindowsOSes then
          GenerateWindowsResources(Self, Path + ExtractFilePath(MainSource), CPU, Plugin);

        Compile(OS, CPU, Plugin, Mode, Path, MainSource, SearchPaths);

        if Plugin then
        begin
          SourceExe := CompiledLibraryFile(MainSource, OS);
          DestExe := PluginLibraryFile(OS, CPU);
        end else
        begin
          SourceExe := ChangeFileExt(MainSource, ExeExtensionOS(OS));
          DestExe := ChangeFileExt(ExecutableName, ExeExtensionOS(OS));
        end;
        if not SameFileName(SourceExe, DestExe) then
        begin
          { move exe to top-level (in case MainSource is in subdirectory
            like code/) and eventually rename to follow ExecutableName }
          Writeln('Moving ', SourceExe, ' to ', DestExe);
          CheckRenameFile(Path + SourceExe, Path + DestExe);
        end;
      end;
  end;
end;

function TCastleProject.PluginLibraryFile(const OS: TOS; const CPU: TCPU): string;
begin
  { "np" prefix is safest for plugin library files. }
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
      FindFiles(Path + IncludePaths[I], false, @GatherFile, FindOptions);
    end;
  GatheringFiles := nil;

  Exclude('*.xcf', Files);
  Exclude('*.blend*', Files);
  Exclude('*~', Files);
  for I := 0 to ExcludePaths.Count - 1 do
    Exclude(ExcludePaths[I], Files);
end;

function TCastleProject.ExternalLibraryPath(const OS: TOS; const CPU: TCPU; const LibraryName: string): string;
var
  LibraryURL: string;
begin
  LibraryURL := ApplicationData('external_libraries/' + CPUToString(CPU) + '-' + OSToString(OS) + '/' + LibraryName);
  Result := URIToFilenameSafe(LibraryURL);
  if not FileExists(Result) then
    raise Exception.Create('Cannot find dependency library in "' + Result + '". ' + SErrDataDir);
end;

procedure TCastleProject.DoPackage(const Target: TTarget;
  const OS: TOS; const CPU: TCPU; const Plugin: boolean;
  const Mode: TCompilationMode);
var
  Pack: TPackageDirectory;

  procedure AddExternalLibrary(const LibraryName: string);
  begin
    Pack.Add(ExternalLibraryPath(OS, CPU, LibraryName), LibraryName);
  end;

var
  Files: TCastleStringList;
  I: Integer;
  PackageFileName, ExecutableNameExt: string;
  UnixPermissionsMatter: boolean;
begin
  Writeln(Format('Packaging project "%s" for %s.',
    [Name, PlatformToString(Target, OS, CPU, Plugin)]));

  if Plugin then
    raise Exception.Create('The "package" command is not useful to package plugins for now');

  { for iOS, the packaging process is special }
  if Target = targetIOS then
  begin
    PackageIOS(Self);
    Exit;
  end;

  { for Android, the packaging process is special }
  if OS = Android then
  begin
    Files := TCastleStringList.Create;
    try
      PackageFiles(Files, true);
      CreateAndroidPackage(Self, OS, CPU, Mode, Files);
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
        Pack.Add(Path + Files[I], Files[I]);
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
            WritelnWarning('Libraries', 'We do not know how to satisfy freetype dependency on win64');
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
      Pack.Make(Path, PackageFileName, ptZip) else
      Pack.Make(Path, PackageFileName, ptTarGz);
  finally FreeAndNil(Pack) end;
end;

procedure TCastleProject.DoInstall(const Target: TTarget;
  const OS: TOS; const CPU: TCPU; const Plugin: boolean);

  {$ifdef UNIX}
  procedure InstallUnixPlugin;
  const
    TargetPathSystemWide = '/usr/lib/mozilla/plugins/';
  var
    PluginFile, Source, Target: string;
  begin
    PluginFile := PluginLibraryFile(OS, CPU);
    Source := InclPathDelim(Path) + PluginFile;
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
  {$endif}

begin
  Writeln(Format('Installing project "%s" for %s.',
    [Name, PlatformToString(Target, OS, CPU, Plugin)]));

  if Target = targetIOS then
    InstallIOS(Self)
  else
  if OS = Android then
    InstallAndroidPackage(Name, QualifiedName)
  else
  if Plugin and (OS in AllWindowsOSes) then
    InstallWindowsPluginRegistry(Name, QualifiedName, Path,
      PluginLibraryFile(OS, CPU), Version, Author)
  else
  {$ifdef UNIX}
  if Plugin and (OS in AllUnixOSes) then
    InstallUnixPlugin
  else
  {$endif}
    raise Exception.Create('The "install" command is not useful for this target / OS / CPU right now. Install the application manually.');
end;

procedure TCastleProject.DoRun(const Target: TTarget;
  const OS: TOS; const CPU: TCPU; const Plugin: boolean;
  const Params: TCastleStringList);
var
  ExeName: string;
  ProcessStatus: Integer;
begin
  Writeln(Format('Running project "%s" for %s.',
    [Name, PlatformToString(Target, OS, CPU, Plugin)]));

  if Plugin then
    raise Exception.Create('The "run" command cannot be used for runninig "plugin" type application right now.');

  if Target = targetIOS then
    RunIOS(Self)
  else
  if OS = Android then
    RunAndroidPackage(Self)
  else
  begin
    ExeName := Path + ChangeFileExt(ExecutableName, ExeExtensionOS(OS));
    Writeln('Running ' + ExeName);
    { run through ExecuteProcess, because we don't want to capture output,
      we want to immediately pass it to user }
    SetCurrentDir(Path);
    ProcessStatus := ExecuteProcess(ExeName, Params.ToArray);
    // this will cause our own status be non-zero
    if ProcessStatus <> 0 then
      raise Exception.CreateFmt('Process returned non-zero (failure) status %d', [ProcessStatus]);
  end;
  //else
  // raise Exception.Create('The "run" command is not useful for this OS / CPU right now. Run the application manually.');
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
      FindFiles(Path, '*', false, @GatherFile, [ffRecursive]);
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
          Pack.Add(Path + Files[I], Files[I]);
      end;
    finally FreeAndNil(Files) end;

    PackageFileName := SourcePackageName;
    Pack.Make(Path, PackageFileName, ptTarGz);
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

procedure TCastleProject.DeleteFoundFile(const FileInfo: TFileInfo; var StopSearch: boolean);
begin
  if Verbose then
    Writeln('Deleting ' + FileInfo.AbsoluteName);
  CheckDeleteFile(FileInfo.AbsoluteName);
  Inc(DeletedFiles);
end;

function TCastleProject.NamePascal: string;
begin
  Result := SReplaceChars(Name, AllChars - ['a'..'z', 'A'..'Z', '0'..'9'], '_');
end;

procedure TCastleProject.GeneratedSourceFile(
  const TemplateRelativeURL, TargetRelativePath, ErrorMessageMissingGameUnits: string;
  const CreateIfNecessary: boolean;
  out RelativeResult, AbsoluteResult: string);
var
  TemplateFile: string;
begin
  AbsoluteResult := OutputPath(Path, CreateIfNecessary) + TargetRelativePath;
  if CreateIfNecessary then
  begin
    TemplateFile := URIToFilenameSafe(ApplicationData(TemplateRelativeURL));
    if FGameUnits = '' then
      raise Exception.Create(ErrorMessageMissingGameUnits);
    ExtractTemplateFile(TemplateFile, AbsoluteResult, TemplateRelativeURL, true);
  end;
  if not IsPrefix(Path, AbsoluteResult, true) then
    raise EInternalError.CreateFmt('Something is wrong with the temporary source location "%s", it is not within the project "%s"',
      [AbsoluteResult, Path]);
  RelativeResult := PrefixRemove(Path, AbsoluteResult, true);
end;

function TCastleProject.AndroidSourceFile(const AbsolutePath, CreateIfNecessary: boolean): string;

  procedure InvalidAndroidSource(const FinalAndroidSource: string);
  begin
    raise Exception.Create('The android source library in "' + FinalAndroidSource + '" must export the necessary JNI functions for our integration to work. See the examples in "castle-engine/tools/build-tool/data/android/library_template_xxx.lpr".' + NL + 'It''s simplest to fix this error by removing the "android_source" from CastleEngineManifest.xml, and using only the "game_units" attribute in CastleEngineManifest.xml. Then the correct Android code will be auto-generated for you.');
  end;

var
  AndroidSourceContents, RelativeResult, AbsoluteResult, TemplateRelativeURL: string;
begin
  { calculate RelativeResult, AbsoluteResult }
  if AndroidSource <> '' then
  begin
    RelativeResult := AndroidSource;
    AbsoluteResult := Path + RelativeResult;
  end else
  begin
    if AndroidProjectType = apIntegrated then
      TemplateRelativeURL := 'android/library_template_integrated.lpr'
    else
      TemplateRelativeURL := 'android/library_template_base.lpr';
    GeneratedSourceFile(TemplateRelativeURL,
      'android' + PathDelim + NamePascal + '_android.lpr',
      'You must specify game_units="..." in the CastleEngineManifest.xml to enable build tool to create an Android project. Alternatively, you can specify android_source="..." in the CastleEngineManifest.xml, to explicitly indicate the Android library source code.',
      CreateIfNecessary, RelativeResult, AbsoluteResult);
  end;

  // for speed, do not check correctness if CreateIfNecessary = false
  if CreateIfNecessary then
  begin
    { check Android lpr file correctness.
      For now, do it even if we generated Android project from our own template
      (when AndroidSource = ''), to check our own work. }
    AndroidSourceContents := FileToString(AbsoluteResult);
    if Pos('ANativeActivity_onCreate', AndroidSourceContents) = 0 then
      InvalidAndroidSource(AbsoluteResult);
    if (AndroidProjectType = apIntegrated) and
       (Pos('Java_net_sourceforge_castleengine_MainActivity_jniMessage', AndroidSourceContents) = 0) then
      InvalidAndroidSource(AbsoluteResult);
  end;

  if AbsolutePath then
    Result := AbsoluteResult
  else
    Result := RelativeResult;
end;

function TCastleProject.IOSSourceFile(const AbsolutePath, CreateIfNecessary: boolean): string;
var
  RelativeResult, AbsoluteResult: string;
begin
  if IOSSource <> '' then
  begin
    RelativeResult := IOSSource;
    AbsoluteResult := Path + RelativeResult;
  end else
  begin
    GeneratedSourceFile('ios/library_template.lpr',
      'ios' + PathDelim + NamePascal + '_ios.lpr',
      'You must specify game_units="..." in the CastleEngineManifest.xml to enable build tool to create an iOS project. Alternatively, you can specify ios_source="..." in the CastleEngineManifest.xml, to explicitly indicate the iOS library source code.',
      CreateIfNecessary, RelativeResult, AbsoluteResult);
  end;

  if AbsolutePath then
    Result := AbsoluteResult
  else
    Result := RelativeResult;
end;

function TCastleProject.StandaloneSourceFile(const AbsolutePath, CreateIfNecessary: boolean): string;
var
  RelativeResult, AbsoluteResult: string;
begin
  if StandaloneSource <> '' then
  begin
    RelativeResult := StandaloneSource;
    AbsoluteResult := Path + RelativeResult;
  end else
  begin
    GeneratedSourceFile('standalone/program_template.lpr',
      'standalone' + PathDelim + NamePascal + '_standalone.lpr',
      'You must specify game_units or standalone_source in the CastleEngineManifest.xml to compile for the standalone platform',
      CreateIfNecessary, RelativeResult, AbsoluteResult);
  end;

  if AbsolutePath then
    Result := AbsoluteResult
  else
    Result := RelativeResult;
end;

function TCastleProject.PluginSourceFile(const AbsolutePath, CreateIfNecessary: boolean): string;
var
  RelativeResult, AbsoluteResult: string;
begin
  if PluginSource <> '' then
  begin
    RelativeResult := PluginSource;
    AbsoluteResult := Path + RelativeResult;
  end else
  begin
    GeneratedSourceFile('plugin/library_template.lpr',
      'plugin' + PathDelim + NamePascal + '_plugin.lpr',
      'You must specify game_units or plugin_source in the CastleEngineManifest.xml to compile a plugin',
      CreateIfNecessary, RelativeResult, AbsoluteResult);
  end;

  if AbsolutePath then
    Result := AbsoluteResult
  else
    Result := RelativeResult;
end;

function TCastleProject.AndroidLibraryFile(const AbsolutePath: boolean = true): string;
begin
  Result := InsertLibPrefix(ChangeFileExt(AndroidSourceFile(AbsolutePath, false), '.so'));
end;

function TCastleProject.IOSLibraryFile(const AbsolutePath: boolean): string;
begin
  Result := InsertLibPrefix(ChangeFileExt(IOSSourceFile(AbsolutePath, false), '.a'));
end;

procedure TCastleProject.DoClean;

  { Delete a file, given as absolute FileName. }
  procedure TryDeleteAbsoluteFile(FileName: string);
  begin
    if FileExists(FileName) then
    begin
      if Verbose then
        Writeln('Deleting ' + FileName);
      CheckDeleteFile(FileName);
      Inc(DeletedFiles);
    end;
  end;

  { Delete a file, given as FileName relative to project root. }
  procedure TryDeleteFile(FileName: string);
  begin
    TryDeleteAbsoluteFile(Path + FileName);
  end;

  procedure DeleteFilesRecursive(const Mask: string);
  begin
    FindFiles(Path, Mask, false, @DeleteFoundFile, [ffRecursive]);
  end;

var
  OS: TOS;
  CPU: TCPU;
  OutputP: string;
begin
  { delete OutputPath first, this also removes many files
    (but RemoveNonEmptyDir does not count them) }
  OutputP := OutputPath(Path, false);
  if DirectoryExists(OutputP) then
  begin
    RemoveNonEmptyDir(OutputP);
    Writeln('Deleted ', OutputP);
  end;

  DeletedFiles := 0;

  TryDeleteFile(ChangeFileExt(ExecutableName, ''));
  TryDeleteFile(ChangeFileExt(ExecutableName, '.exe'));

  if AndroidSource <> '' then
    TryDeleteAbsoluteFile(AndroidLibraryFile);
  if IOSSource <> '' then
    TryDeleteAbsoluteFile(IOSLibraryFile);

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
          TryDeleteFile(PluginLibraryFile(OS, CPU));

  { compilation and editor backups }
  DeleteFilesRecursive('*~'); // editor backup, e.g. Emacs
  DeleteFilesRecursive('*.ppu'); // compilation
  DeleteFilesRecursive('*.o'); // compilation
  DeleteFilesRecursive('*.or'); // compilation
  DeleteFilesRecursive('*.compiled'); // Lazarus compilation
  DeleteFilesRecursive('*.rst'); // resource strings
  DeleteFilesRecursive('*.rsj'); // resource strings

  { Note that we do not remove
      automatic-windows-resources.res
      plugin-automatic-windows-resources.res
    They can be committed,
    otherwise compilation without using castle-engine tool
    will not be easily possible. }

  Writeln('Deleted ', DeletedFiles, ' files');
end;

procedure TCastleProject.DoAutoGenerateTextures;
begin
  AutoGenerateTextures(Self);
end;

procedure TCastleProject.DoAutoGenerateClean;
begin
  AutoGenerateClean(Self);
end;

procedure TCastleProject.DoGenerateProgram;

  procedure Generate(const Ext: string);
  var
    TemplateRelativePath, TemplateFile, TargetFile: string;
  begin
    TemplateRelativePath := 'standalone/program_template.' + Ext;
    TemplateFile := URIToFilenameSafe(ApplicationData(TemplateRelativePath));
    TargetFile := Path + NamePascal + '_standalone.' + Ext;
    ExtractTemplateFile(TemplateFile, TargetFile, TemplateRelativePath, true);
    if Verbose then
      Writeln('Generated ', TargetFile);
  end;

begin
  if FGameUnits = '' then
    raise Exception.Create('You must specify game_units="..." in the CastleEngineManifest.xml to enable build tool to create a standalone project');
  Generate('lpr');
  Generate('lpi');
end;

function TCastleProject.ReplaceMacros(const Source: string): string;

const
  AndroidScreenOrientation: array [TScreenOrientation] of string =
  ('unspecified', 'sensorLandscape', 'sensorPortrait');

  AndroidScreenOrientationFeature: array [TScreenOrientation] of string =
  ('',
   '<uses-feature android:name="android.hardware.screen.landscape"/>',
   '<uses-feature android:name="android.hardware.screen.portrait"/>');

  IOSScreenOrientation: array [TScreenOrientation] of string =
  (CharTab + CharTab + '<string>UIInterfaceOrientationPortrait</string>' + NL +
   CharTab + CharTab + '<string>UIInterfaceOrientationPortraitUpsideDown</string>' + NL +
   CharTab + CharTab + '<string>UIInterfaceOrientationLandscapeLeft</string>' + NL +
   CharTab + CharTab + '<string>UIInterfaceOrientationLandscapeRight</string>' + NL,

   CharTab + CharTab + '<string>UIInterfaceOrientationLandscapeLeft</string>' + NL +
   CharTab + CharTab + '<string>UIInterfaceOrientationLandscapeRight</string>' + NL,

   CharTab + CharTab + '<string>UIInterfaceOrientationPortrait</string>' + NL +
   CharTab + CharTab + '<string>UIInterfaceOrientationPortraitUpsideDown</string>' + NL
  );

  function AndroidActivityLoadLibraries: string;
  begin
    { some Android devices work without this clause, some don't }
    Result := '';
    if depSound in Dependencies then
      Result += 'safeLoadLibrary("openal");' + NL;
    if depOggVorbis in Dependencies then
      Result += 'safeLoadLibrary("tremolo");' + NL;
  end;

  function SearchPathsStr: string;
  var
    S: string;
  begin
    Result := '';
    for S in SearchPaths do
    begin
      if Result <> '' then
        Result := Result + ';';
      Result := Result + S;
    end;
  end;

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

var
  Macros: TStringStringMap;
  I: Integer;
  P, NonEmptyAuthor, AndroidLibraryName: string;
  VersionComponents: array [0..3] of Cardinal;
  VersionComponentsString: TCastleStringList;
  AndroidServiceParameterPair: TStringStringMap.TDictionaryPair;
  PreviousMacros: array of TStringStringMap.TDictionaryPair;
begin
  { calculate version as 4 numbers, Windows resource/manifest stuff expect this }
  VersionComponentsString := SplitString(Version, '.');
  try
    for I := 0 to High(VersionComponents) do
      if I < VersionComponentsString.Count then
        VersionComponents[I] := StrToIntDef(Trim(VersionComponentsString[I]), 0) else
        VersionComponents[I] := 0;
  finally FreeAndNil(VersionComponentsString) end;

  if Author = '' then
    NonEmptyAuthor := 'Unknown Author' else
    NonEmptyAuthor := Author;

  Macros := TStringStringMap.Create;
  try
    Macros.Add('DOLLAR'          , '$');
    Macros.Add('VERSION_MAJOR'   , IntToStr(VersionComponents[0]));
    Macros.Add('VERSION_MINOR'   , IntToStr(VersionComponents[1]));
    Macros.Add('VERSION_RELEASE' , IntToStr(VersionComponents[2]));
    Macros.Add('VERSION_BUILD'   , IntToStr(VersionComponents[3]));
    Macros.Add('VERSION'         , Version);
    Macros.Add('VERSION_CODE'    , IntToStr(FVersionCode));
    Macros.Add('NAME'            , Name);
    Macros.Add('NAME_PASCAL'     , NamePascal);
    Macros.Add('QUALIFIED_NAME'  , QualifiedName);
    Macros.Add('CAPTION'         , Caption);
    Macros.Add('AUTHOR'          , NonEmptyAuthor);
    Macros.Add('EXECUTABLE_NAME' , ExecutableName);
    Macros.Add('GAME_UNITS'      , FGameUnits);
    Macros.Add('SEARCH_PATHS'    , SearchPathsStr);

    // Android specific stuff
    AndroidLibraryName := ChangeFileExt(ExtractFileName(AndroidSourceFile(true, false)), '');
    Macros.Add('ANDROID_LIBRARY_NAME'                , AndroidLibraryName);
    Macros.Add('ANDROID_SCREEN_ORIENTATION'          , AndroidScreenOrientation[ScreenOrientation]);
    Macros.Add('ANDROID_SCREEN_ORIENTATION_FEATURE'  , AndroidScreenOrientationFeature[ScreenOrientation]);
    Macros.Add('ANDROID_ACTIVITY_LOAD_LIBRARIES'     , AndroidActivityLoadLibraries);
    Macros.Add('ANDROID_COMPILE_SDK_VERSION'         , IntToStr(AndroidCompileSdkVersion));
    Macros.Add('ANDROID_BUILD_TOOLS_VERSION'         , AndroidBuildToolsVersion);
    Macros.Add('ANDROID_MIN_SDK_VERSION'             , IntToStr(AndroidMinSdkVersion));
    Macros.Add('ANDROID_TARGET_SDK_VERSION'          , IntToStr(AndroidTargetSdkVersion));
    for I := 0 to AndroidServices.Count - 1 do
      for AndroidServiceParameterPair in AndroidServices[I].Parameters do
        Macros.Add('ANDROID.' +
          UpperCase(AndroidServices[I].Name) + '.' +
          UpperCase(AndroidServiceParameterPair.Key),
          AndroidServiceParameterPair.Value);

    // iOS specific stuff }
    Macros.Add('IOS_LIBRARY_BASE_NAME' , ExtractFileName(IOSLibraryFile));
    Macros.Add('IOS_SCREEN_ORIENTATION', IOSScreenOrientation[ScreenOrientation]);

    // add CamelCase() replacements, add ${} around
    PreviousMacros := Macros.ToArray;
    Macros.Clear;
    for I := 0 to Length(PreviousMacros) - 1 do
    begin
      P := PreviousMacros[I].Key;
      Macros.Add('${' + P + '}', PreviousMacros[I].Value);
      Macros.Add('${CamelCase(' + P + ')}', MakeCamelCase(PreviousMacros[I].Value));
    end;
    Result := SReplacePatterns(Source, Macros, true);
  finally FreeAndNil(Macros) end;
end;

procedure TCastleProject.ExtractTemplate(const TemplatePath, DestinationPath: string);
var
  TemplateFilesCount: Cardinal;
begin
  ExtractTemplateDestinationPath := InclPathDelim(DestinationPath);
  ExtractTemplateDir := ExclPathDelim(URIToFilenameSafe(ApplicationData(TemplatePath)));
  if not DirectoryExists(ExtractTemplateDir) then
    raise Exception.Create('Cannot find Android project template in "' + ExtractTemplateDir + '". ' + SErrDataDir);

  TemplateFilesCount := FindFiles(ExtractTemplateDir, '*', false,
    @ExtractTemplateFoundFile, [ffRecursive]);
  if Verbose then
    Writeln(Format('Copied template "%s" (%d files) to "%s"',
      [TemplatePath, TemplateFilesCount, DestinationPath]));
end;

procedure TCastleProject.ExtractTemplateFoundFile(const FileInfo: TFileInfo; var StopSearch: boolean);
var
  DestinationRelativeFileName, DestinationFileName: string;
begin
  DestinationRelativeFileName := PrefixRemove(InclPathDelim(ExtractTemplateDir),
    FileInfo.AbsoluteName, true);

  if IsWild(DestinationRelativeFileName, '*setup_sdk.sh', true) or
     IsWild(DestinationRelativeFileName, '*~', true) then
  begin
    // if Verbose then
    //   Writeln('Ignoring template file: ' + DestinationRelativeFileName);
    Exit;
  end;

  StringReplaceAllVar(DestinationRelativeFileName, 'cge_project_name', Name);

  DestinationFileName := ExtractTemplateDestinationPath + DestinationRelativeFileName;

  ExtractTemplateFile(FileInfo.AbsoluteName, DestinationFileName,
    DestinationRelativeFileName, false);
end;

procedure TCastleProject.ExtractTemplateFile(
  const SourceFileName, DestinationFileName, DestinationRelativeFileName: string;
  const OverrideExisting: boolean);
var
  DestinationRelativeFileNameSlashes, Contents, Ext: string;
  BinaryFile: boolean;
begin
  if (not OverrideExisting) and FileExists(DestinationFileName) then
  begin
    DestinationRelativeFileNameSlashes := StringReplace(
      DestinationRelativeFileName, '\', '/', [rfReplaceAll]);
    if SameText(DestinationRelativeFileNameSlashes, 'app/src/main/AndroidManifest.xml') then
      MergeAndroidManifest(SourceFileName, DestinationFileName, @ReplaceMacros) else
    if SameText(DestinationRelativeFileNameSlashes, 'app/src/main/java/net/sourceforge/castleengine/MainActivity.java') then
      MergeAndroidMainActivity(SourceFileName, DestinationFileName, @ReplaceMacros) else
    if SameText(DestinationRelativeFileNameSlashes, 'app/src/main/jni/Android.mk') or
       SameText(DestinationRelativeFileNameSlashes, 'app/src/main/custom-proguard-project.txt') then
      MergeAppend(SourceFileName, DestinationFileName, @ReplaceMacros) else
    if SameText(DestinationRelativeFileNameSlashes, 'app/build.gradle') then
      MergeBuildGradle(SourceFileName, DestinationFileName, @ReplaceMacros) else
    if SameText(DestinationRelativeFileNameSlashes, 'build.gradle') then
      MergeBuildGradle(SourceFileName, DestinationFileName, @ReplaceMacros) else
    if Verbose then
      Writeln('Not overwriting custom ' + DestinationRelativeFileName);
    Exit;
  end;

  Ext := ExtractFileExt(SourceFileName);
  BinaryFile := SameText(Ext, '.so') or SameText(Ext, '.jar');
  CheckForceDirectories(ExtractFilePath(DestinationFileName));
  if BinaryFile then
  begin
    CheckCopyFile(SourceFileName, DestinationFileName);
  end else
  begin
    Contents := FileToString(FilenameToURISafe(SourceFileName));
    Contents := ReplaceMacros(Contents);
    StringToFile(FilenameToURISafe(DestinationFileName), Contents);
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
