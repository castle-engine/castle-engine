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

{ Project information (from CastleEngineManifest.xml) and operations. }
unit ToolProject;

interface

uses SysUtils,
  CastleFindFiles, CastleStringUtils, CastleUtils,
  ToolArchitectures, ToolCompile;

type
  TDependency = (depFreetype, depZlib, depPng, depSound, depOggVorbis);
  TDependencies = set of TDependency;

  TCastleProject = class
  private
    FDependencies: TDependencies;
    FName, FExecutableName, FAuthor: string;
    GatheringFiles: TCastleStringList; //< only for GatherFile
    ManifestFile, ProjectPath, DataPath: string;
    IncludePaths, ExcludePaths, Icons: TCastleStringList;
    IncludePathsRecursive: TBooleanList;
    FStandaloneSource, FAndroidSource: string;
    DeletedFiles: Cardinal; //< only for DeleteFoundFile
    FVersion: string;
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
      Relative to ProjectPath. }
    function AndroidLibraryFile: string;
    property Version: string read FVersion;
    function ReplaceMacros(const Source: string): string;
  public
    constructor Create;
    constructor Create(const Path: string);
    destructor Destroy; override;

    property Dependencies: TDependencies read FDependencies;
    property Name: string read FName;
    property Author: string read FAuthor;
    property ExecutableName: string read FExecutableName;
    property StandaloneSource: string read FStandaloneSource;
    property AndroidSource: string read FAndroidSource;

    procedure DoCreateManifest;
    procedure DoCompile(const OS: TOS; const CPU: TCPU; const Mode: TCompilationMode);
    procedure DoPackage(const OS: TOS; const CPU: TCPU);
    procedure DoPackageSource;
    procedure DoClean;
  end;

function DependencyToString(const D: TDependency): string;
function StringToDependency(const S: string): TDependency;

implementation

uses StrUtils, DOM, Process, Classes,
  CastleURIUtils, CastleXMLUtils, CastleWarnings, CastleFilesUtils,
  ToolUtils, ToolPackage, ToolWindowsResources;

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

constructor TCastleProject.Create(const Path: string);

  procedure ReadManifest;

    procedure AutoGuessManifest;
    begin
      Writeln('Manifest file not found: ' + ManifestFile);
      Writeln('Guessing project values. Use create-manifest command to write these guesses into new CastleEngineManifest.xml');
      FName := ExtractFileName(ExtractFileDir(ManifestFile));
      FExecutableName := FName;
      FStandaloneSource := FName + '.lpr';
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

      try
        URLReadXML(Doc, ManifestURL);
        Check(Doc.DocumentElement.TagName = 'project',
          'Root node of CastleEngineManifest.xml must be <project>');
        FName := Doc.DocumentElement.AttributeString('name');
        FExecutableName := Doc.DocumentElement.AttributeStringDef('executable_name', FName);
        FStandaloneSource := Doc.DocumentElement.AttributeStringDef('standalone_source', '');
        FAndroidSource := Doc.DocumentElement.AttributeStringDef('android_source', '');
        FAuthor := Doc.DocumentElement.AttributeStringDef('author', '');

        Element := DOMGetChildElement(Doc.DocumentElement, 'version', false);
        if Element <> nil then
          FVersion := Element.AttributeString('name');

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


const
  DataName = 'data';
begin
  inherited Create;

  { empty initial state }
  IncludePaths := TCastleStringList.Create;
  IncludePathsRecursive := TBooleanList.Create;
  ExcludePaths := TCastleStringList.Create;
  FDependencies := [];
  Icons := TCastleStringList.Create;

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
'<project name="' + Name + '" standalone_source="' + StandaloneSource + '">' +NL+
'</project>' + NL;
  StringToFile(ManifestFile, Contents);
  Writeln('Created manifest ' + ManifestFile);
end;

procedure TCastleProject.DoCompile(const OS: TOS; const CPU: TCPU; const Mode: TCompilationMode);
var
  SourceExe, DestExe: string;
begin
  Writeln(Format('Compiling project "%s" for OS "%s" and CPU "%s" in mode "%s".',
    [Name, OSToString(OS), CPUToString(CPU), ModeToString(Mode)]));

  case OS of
    Android:
      begin
        if AndroidSource = '' then
          raise Exception.Create('android_source property for project not defined, cannot compile Android version');
        Compile(OS, CPU, Mode, ProjectPath, AndroidSource);
        Writeln('Compiled library for Android in ', AndroidLibraryFile);
      end;
    else
      begin
        if StandaloneSource = '' then
          raise Exception.Create('standalone_source property for project not defined, cannot compile standalone version');

        if OS in AllWindowsOSes then
          GenerateWindowsResources(@ReplaceMacros, ProjectPath, Icons, CPU);

        Compile(OS, CPU, Mode, ProjectPath, StandaloneSource);

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

var
  Files: TCastleStringList;
  I: Integer;
  PackageFileName, ExecutableNameExt: string;
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
        if AnsiCompareFileName(Files[I], SourcePackageName) = 0 then
          IsPackageName := true;

        if not IsPackageName then
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
  DeleteFilesRecursive('*.or'); // compilation
  DeleteFilesRecursive('*.compiled'); // Lazarus compilation
  DeleteFilesRecursive('*.rst'); // resource strings
  DeleteFilesRecursive('*.rsj'); // resource strings

  { our own trash. Note that we do not remove .res file, it can be committed,
    otherwise compilation without using castle-engine tool will not be easily possible. }
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
    Patterns.Add('NAME');            Values.Add(Name);
    Patterns.Add('AUTHOR');          Values.Add(NonEmptyAuthor);
    Patterns.Add('EXECUTABLE_NAME'); Values.Add(ExecutableName);
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

end.
