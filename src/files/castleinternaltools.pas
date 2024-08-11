{
  Copyright 2022-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Functionality useful for CGE tools and design-time operations.

  Practically, this is a shared code between

  @unorderedList(
    @item(CGE build tool, in tools/build-tool/)

    @item(CGE editor, in tools/castle-editor/)

    @item(Delphi design-time functionality, in castle_engine_design.dpk,
      in unit CastleInternalDelphiDesign.)
  )
}
unit CastleInternalTools;

{$I castleconf.inc}

interface

uses Classes, Dom,
  CastleFindFiles;

const
  { Paths with units and include files that are for all OSes and all compilers.

    Note:

    - We don't bother trying to have separate include dirs (.inc) and units (.pas).
      We just pass the same paths for both includes and units, this is simpler.

    - We pass all paths, even system-specific, regardless of the target
      OS/architecture.

      We tried smarter approach in the past (such that you could have e.g.
      "windows/castle_system_specific.inc" and "unix/castle_system_specific.inc",
      and compiler recognized what to do on [$I castle_system_specific.inc]
      based on include paths)...
      but it was not friendly to Lazarus or Delphi packages,
      where it's easier to define the same include path for all platforms.

      So it is simpler to just name all includes and units differently,
      even across system-specific dirs. }

  EnginePaths: array [0..43] of String = (
    'base',
    'common_includes',
    'base/android',
    'base/windows',
    'base/unix',
    'base_rendering',
    'base_rendering/dglopengl',
    'base_rendering/glsl/generated-pascal',
    'fonts',
    'window',
    'window/gtk',
    'window/windows',
    'window/unix',
    'window/deprecated_units',
    'images',
    'transform',
    'scene',
    'scene/glsl/generated-pascal',
    'scene/x3d',
    'scene/load',
    'scene/load/spine',
    'scene/load/md3',
    'scene/load/collada',
    'scene/load/pasgltf',
    'audio',
    'audio/fmod',
    'audio/openal',
    'audio/ogg_vorbis',
    'files',
    'files/indy',
    'castlescript',
    'ui',
    'ui/windows',
    'services',
    'physics',
    'physics/kraft',
    'deprecated_units',
    { Vampyre Imaging Library }
    'vampyre_imaginglib/src/Source',
    'vampyre_imaginglib/src/Source/JpegLib',
    'vampyre_imaginglib/src/Source/ZLib',
    'vampyre_imaginglib/src/Extras/Extensions',
    'vampyre_imaginglib/src/Extensions/J2KObjects',
    'vampyre_imaginglib/src/Extensions/LibTiff',
    'vampyre_imaginglib/src/Extensions'
  );

  { Additional include/units paths, only for Delphi. }
  EnginePathsDelphi: array [0..2] of String = (
    'delphi',
    'compatibility/delphi-only',
    'compatibility/delphi-only/fcl-json'
  );

  { Paths for library (object) files.
    For FPC these are passed using -Fl. }
  EngineLibraryPaths: array [0..1] of String = (
    'vampyre_imaginglib/src/Extensions/J2KObjects',
    'vampyre_imaginglib/src/Extensions/LibTiff/Compiled'
  );

type
  { CGE project dependencies. }
  TDependency = (depFreetype, depZlib, depPng, depSound, depOggVorbis, depHttps);
  TDependencies = set of TDependency;

  { Platforms where we make effort to copy extra files alongside the EXE
    ("deploy").
    This is done when building a CGE project
    using build tool ("castle-engine compile")
    or in Delphi IDE (with our CastleInternalDelphiDesign feature).

    TODO: In the future, we could use TOS and TCPU and make it general.
    For now, this is practically only used by Windows platforms that need
    some DLLs, on other platforms we're fine with just using system libraries. }
  TDeployFilesPlatform = (dpWin32, dpWin64);

  { Managing project dependencies: reading from manifest, guessing based on data etc. }
  TProjectDependencies = class
  strict private
    FDependencies: TDependencies;
    procedure AddDependencyFromFoundDataFile(
      const FileInfo: TFileInfo; var StopSearch: Boolean);
  public
    property Dependencies: TDependencies read FDependencies write FDependencies;

    { Add to Dependencies values from CastleEngineManifest.xml file. }
    procedure ReadFromManifest(const Doc: TXmlDocument);

    { Add to Dependencies values that are implied by other values.
      E.g. using libpng requires using also zlib. }
    procedure CloseDependencies;

    { Add to Dependencies values implied by some data files.
      E.g. if data has .png file, then we add depPng. }
    procedure GuessDependencies(const ProjectDataPath: String);

    { Add deploy files (.dll on Windows) to Files, based on given platform and project
      dependencies. The resulting filenames are relative to build tool's data. }
    procedure DeployFiles(const DeployPlatform: TDeployFilesPlatform;
      const Files: TStrings);
  end;

function DependencyToString(const D: TDependency): String;
function StringToDependency(const S: String): TDependency;

{ API reference (online or offline).
  EnginePath is the path to the engine root directory,
  or '' if not known. }
function ApiReferenceUrlCore(const EnginePath: String): String;

{ Make correct CGE project qualified name from any ProjectName. }
function MakeQualifiedName(ProjectName: String): String;

{ Make correct CGE project Pascal name from any ProjectName. }
function MakeProjectPascalName(ProjectName: String): String;

type
  { Options for project creation. }
  TProjectCreationOptions = record
    { Where to place the new project.
      A subdirectory ProjectName will be created there. }
    ParentDir: String;

    { Template name to use, must be one of the allowed templates
      in project_templates. }
    TemplateName: String;

    { New project parameters. }
    ProjectName: String;
    ProjectCaption: String;
    MainView: String;
  end;

{ Create directory with new CGE project, instantiating given template. }
procedure ProjectCreateFromTemplate(const EnginePath: String;
  const Options: TProjectCreationOptions;
  out ProjectDirUrl: String);

implementation

uses SysUtils, StrUtils,
  CastleUriUtils, CastleXmlUtils, CastleStringUtils, CastleLog, CastleUtils,
  CastleFilesUtils;

function ApiReferenceUrlCore(const EnginePath: String): String;
var
  LocalDocsPath: String;
begin
  if EnginePath <> '' then
  begin
    LocalDocsPath := EnginePath + 'doc' + PathDelim + 'reference' + PathDelim;
    if DirectoryExists(LocalDocsPath) then
      Exit(FilenameToUriSafe(LocalDocsPath));
  end;

  Result := 'https://castle-engine.io/apidoc/html/';
end;

const
  DependencyNames: array [TDependency] of string =
  ('Freetype', 'Zlib', 'Png', 'Sound', 'OggVorbis', 'Https');

function DependencyToString(const D: TDependency): String;
begin
  Result := DependencyNames[D];
end;

function StringToDependency(const S: String): TDependency;
begin
  for Result := Low(TDependency) to High(TDependency) do
    if AnsiSameText(DependencyNames[Result], S) then
      Exit;
  raise Exception.CreateFmt('Invalid dependency name "%s"', [S]);
end;

{ TProjectDependencies ------------------------------------------------------- }

procedure TProjectDependencies.ReadFromManifest(const Doc: TXmlDocument);
var
  Element, ChildElement: TDOMElement;
  ChildElements: TXMLElementIterator;
begin
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
end;

procedure TProjectDependencies.CloseDependencies;

  procedure DependenciesClosure(const Dep, DepRequirement: TDependency);
  begin
    if (Dep in Dependencies) and not (DepRequirement in Dependencies) then
    begin
      WritelnLog('Automatically adding "' + DependencyToString(DepRequirement) +
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

procedure TProjectDependencies.AddDependencyFromFoundDataFile(
  const FileInfo: TFileInfo; var StopSearch: Boolean);

  procedure AddDependency(const Dependency: TDependency; const FileInfo: TFileInfo);
  begin
    if not (Dependency in Dependencies) then
    begin
      WritelnLog('Automatically adding "' + DependencyToString(Dependency) +
        '" to dependencies because data contains file: ' + FileInfo.Url);
      Include(FDependencies, Dependency);
    end;
  end;

const
  { Ignore case on all platforms, to e.g. add freetype DLL when file FOO.TTF
    is present in data, even on case-sensitive filesystems. }
  IgnoreCase = true;
begin
  if IsWild(FileInfo.Name, '*.ttf', IgnoreCase) or
     IsWild(FileInfo.Name, '*.otf', IgnoreCase) then
    AddDependency(depFreetype, FileInfo);
  if IsWild(FileInfo.Name, '*.gz' , IgnoreCase) then
    AddDependency(depZlib, FileInfo);
  if IsWild(FileInfo.Name, '*.png', IgnoreCase) then
    AddDependency(depPng, FileInfo);
  if IsWild(FileInfo.Name, '*.wav', IgnoreCase) then
    AddDependency(depSound, FileInfo);
  if IsWild(FileInfo.Name, '*.ogg', IgnoreCase) then
    AddDependency(depOggVorbis, FileInfo);
end;

procedure TProjectDependencies.GuessDependencies(const ProjectDataPath: String);
begin
  { Note: Instead of one FindFiles call, this could also be implemented by a series
    of FindFirstFileIgnoreCase calls, like

      if FindFirstFileIgnoreCase(DataPath, '*.ttf' , false, [ffRecursive], FileInfo) or
      if FindFirstFileIgnoreCase(DataPath, '*.otf' , false, [ffRecursive], FileInfo) then
        AddDependency(depFreetype, FileInfo);
      if FindFirstFileIgnoreCase(DataPath, '*.gz' , false, [ffRecursive], FileInfo) then
        AddDependency(depZlib, FileInfo);

    But this would be inefficient. Each FindFirstFileIgnoreCase effectively again
    enumerates all files in data. }

  FindFiles(ProjectDataPath, '*', false,
    {$ifdef FPC}@{$endif} AddDependencyFromFoundDataFile, [ffRecursive]);
end;

procedure TProjectDependencies.DeployFiles(const DeployPlatform: TDeployFilesPlatform;
  const Files: TStrings);
var
  Prefix: String;
begin
  case DeployPlatform of
    dpWin32:
      begin
        { Matches CPUToString(CPU) + '-' + OSToString(OS) for win32. }
        Prefix := 'external_libraries/i386-win32/';

        if depFreetype in Dependencies then
        begin
          Files.Add(Prefix + 'freetype.dll');
          Files.Add(Prefix + 'vcruntime140.dll');
        end;
        if depZlib in Dependencies then
          Files.Add(Prefix + 'zlib1.dll');
        if depPng in Dependencies then
          Files.Add(Prefix + 'libpng12.dll');
        if depSound in Dependencies then
        begin
          Files.Add(Prefix + 'OpenAL32.dll');
          Files.Add(Prefix + 'wrap_oal.dll');
        end;
        if depOggVorbis in Dependencies then
        begin
          Files.Add(Prefix + 'ogg.dll');
          Files.Add(Prefix + 'vorbis.dll');
          Files.Add(Prefix + 'vorbisenc.dll');
          Files.Add(Prefix + 'vorbisfile.dll');
          Files.Add(Prefix + 'msvcr120.dll');
        end;
        if depHttps in Dependencies then
        begin
          Files.Add(Prefix + 'openssl/libeay32.dll');
          Files.Add(Prefix + 'openssl/ssleay32.dll');
        end;
      end;

    dpWin64:
      begin
        { Matches CPUToString(CPU) + '-' + OSToString(OS) for win32. }
        Prefix := 'external_libraries/x86_64-win64/';

        if depFreetype in Dependencies then
        begin
          Files.Add(Prefix + 'freetype.dll');
          Files.Add(Prefix + 'vcruntime140.dll');
        end;
        if depZlib in Dependencies then
          Files.Add(Prefix + 'zlib1.dll');
        if depPng in Dependencies then
          Files.Add(Prefix + 'libpng14-14.dll');
        if depSound in Dependencies then
        begin
          Files.Add(Prefix + 'OpenAL32.dll');
          Files.Add(Prefix + 'wrap_oal.dll');
        end;
        if depOggVorbis in Dependencies then
        begin
          Files.Add(Prefix + 'libogg.dll');
          Files.Add(Prefix + 'libvorbis.dll');
          { Files.Add(Prefix + 'vorbisenc.dll'); not present? }
          Files.Add(Prefix + 'vorbisfile.dll');
          Files.Add(Prefix + 'msvcr120.dll');
        end;
        if depHttps in Dependencies then
        begin
          Files.Add(Prefix + 'openssl/libeay32.dll');
          Files.Add(Prefix + 'openssl/ssleay32.dll');
        end;
      end;
  end;
end;

{ TTemplateCopyProcess ------------------------------------------------------------ }

type
  TTemplateCopyProcess = class
    TemplateUrl: String;
    ProjectDirUrl: String;
    MainView: String;
    Macros: TStringStringMap;
    procedure FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

procedure TTemplateCopyProcess.FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
var
  Contents, RelativeUrl, TargetUrl, TargetFileName, Mime: String;
begin
  { Ignore case at IsPrefix / PrefixRemove calls,
    in case it's not case-sensitive file-system, then the case in theory
    can differ. }
  if not IsPrefix(TemplateUrl, FileInfo.URL, true) then
    raise Exception.CreateFmt('Unexpected: %s is not a prefix of %s, report a bug',
      [TemplateUrl, FileInfo.URL]);
  RelativeUrl := PrefixRemove(TemplateUrl, FileInfo.URL, true);
  TargetUrl := CombineUri(ProjectDirUrl, RelativeUrl);
  { Rename target files that depend on MainView. }
  if ExtractUriName(TargetUrl) = 'gameviewmain.pas' then
    TargetUrl := ExtractUriPath(TargetUrl) + 'gameview' + LowerCase(MainView) + '.pas';
  if ExtractUriName(TargetUrl) = 'gameviewmain.castle-user-interface' then
    TargetUrl := ExtractUriPath(TargetUrl) + 'gameview' + LowerCase(MainView) + '.castle-user-interface';
  TargetFileName := UriToFilenameSafe(TargetUrl);

  if FileInfo.Directory then
  begin
    // create directory
    if not ForceDirectories(TargetFileName) then
      raise Exception.CreateFmt('Cannot create directory "%s"', [TargetFileName]);
  end else
  begin
    Mime := UriMimeType(FileInfo.URL);
    if (Mime = 'application/xml') or
       (Mime = 'text/plain') then
    begin
      // copy text file, replacing macros
      Contents := FileToString(FileInfo.URL);
      Contents := SReplacePatterns(Contents, Macros, false);
      StringToFile(TargetFileName, Contents);
    end else
    begin
      // simply copy other file types (e.g. sample png images in project templates)
      CheckCopyFile(UriToFilenameSafe(FileInfo.URL), TargetFileName);
    end;
  end;
end;

procedure ProjectCreateFromTemplate(const EnginePath: String;
  const Options: TProjectCreationOptions;
  out ProjectDirUrl: String);

  procedure AddMacroXmlQuote(const Macros: TStringStringMap; const MacroName: String);

    function XmlQuote(const S: String): String;
    begin
      Result := SReplacePatterns(S,
        ['&', '<', '>', '"'],
        ['&amp;', '&lt;', '&gt;', '&quot;'],
        false { IgnoreCase; can be false, it doesn't matter, as our patterns are not letters }
      );
    end;

  begin
    Macros.Add('${XmlQuote(' + MacroName + ')}', XmlQuote(Macros['${' + MacroName + '}']));
  end;

var
  ProjectDir, TemplateUrl, ProjectQualifiedName, ProjectPascalName: String;
  CopyProcess: TTemplateCopyProcess;
  Macros: TStringStringMap;
begin
  Assert(Options.ProjectName <> '');

  // Calculate and check TemplateUrl
  // (do it early, to fail early if template does not exist, without creating project dir)
  TemplateUrl := FilenameToUriSafe(InclPathDelim(EnginePath) +
    'tools/castle-editor/data/project_templates/' + Options.TemplateName + '/files/');
  if UriExists(TemplateUrl) <> ueDirectory then
    raise Exception.CreateFmt('Cannot find template directory %s. Make sure template name is valid and engine path is valid.',
      [TemplateUrl]);

  // Create project dir
  ProjectDir := InclPathDelim(Options.ParentDir) + Options.ProjectName;
  ProjectDirUrl := FilenameToUriSafe(InclPathDelim(ProjectDir));
  if DirectoryExists(ProjectDir) then
    raise Exception.CreateFmt('Directory "%s" already exists. Choose a different project name.', [ProjectDir]);
  if not ForceDirectories(ProjectDir) then
    raise Exception.CreateFmt('Cannot create directory "%s".', [ProjectDir]);

  ProjectQualifiedName := MakeQualifiedName(Options.ProjectName);
  ProjectPascalName := MakeProjectPascalName(Options.ProjectName);

  Macros := TStringStringMap.Create;
  try
    Macros.Add('${PROJECT_NAME}', Options.ProjectName);
    Macros.Add('${PROJECT_QUALIFIED_NAME}', ProjectQualifiedName);
    Macros.Add('${PROJECT_PASCAL_NAME}', ProjectPascalName);
    Macros.Add('${PROJECT_CAPTION}', Options.ProjectCaption);
    Macros.Add('${MAIN_VIEW}', Options.MainView);
    Macros.Add('${MAIN_VIEW_LOWERCASE}', LowerCase(Options.MainView));

    { Generate versions of some macros with xml_quote function. }
    AddMacroXmlQuote(Macros, 'PROJECT_NAME');
    AddMacroXmlQuote(Macros, 'PROJECT_QUALIFIED_NAME');
    AddMacroXmlQuote(Macros, 'PROJECT_PASCAL_NAME');
    AddMacroXmlQuote(Macros, 'PROJECT_CAPTION');

    CopyProcess := TTemplateCopyProcess.Create;
    try
      CopyProcess.TemplateUrl := TemplateUrl;
      CopyProcess.ProjectDirUrl := ProjectDirUrl;
      CopyProcess.Macros := Macros;
      CopyProcess.MainView := Options.MainView;
      FindFiles(TemplateUrl, '*', true, {$ifdef FPC}@{$endif} CopyProcess.FoundFile, [ffRecursive]);
    finally FreeAndNil(CopyProcess) end;
  finally FreeAndNil(Macros) end;
end;

const
  AlphaNum = ['a'..'z', 'A'..'Z', '0'..'9'];

function MakeQualifiedName(ProjectName: String): String;
const
  { See ToolProject constant in CGE build tool. }
  QualifiedNameAllowedChars = AlphaNum + ['.'];
  QualifiedNameAllowedCharsFirst = QualifiedNameAllowedChars - ['.', '0'..'9'];
begin
  ProjectName := SDeleteChars(ProjectName, AllChars - QualifiedNameAllowedChars);
  if (ProjectName <> '') and not (ProjectName[1] in QualifiedNameAllowedCharsFirst) then
    ProjectName := 'project' + ProjectName;
  if ProjectName = '' then
    ProjectName := 'project'; // if ProjectName is left empty after above deletions, set it to anything
  Result := 'com.mycompany.' + ProjectName;
end;

function MakeProjectPascalName(ProjectName: String): String;
const
  ValidProjectPascalNameChars = AlphaNum + ['_'];
  ValidProjectPascalNameCharsFirst = ValidProjectPascalNameChars - ['0'..'9'];
begin
  ProjectName := SReplaceChars(ProjectName, AllChars - ValidProjectPascalNameChars, '_');
  if (ProjectName <> '') and not (ProjectName[1] in ValidProjectPascalNameCharsFirst) then
    ProjectName := 'project' + ProjectName;
  if ProjectName = '' then
    ProjectName := 'project'; // if ProjectName is left empty after above deletions, set it to anything
  Result := ProjectName;
end;

end.