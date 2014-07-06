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

uses CastleFindFiles, CastleArchitectures, CastleStringUtils;

type
  TDependency = (depFreetype, depZlib, depPng, depSound, depOggVorbis);
  TDependencies = set of TDependency;

type
  TCastleProject = class
  private
    FDependencies: TDependencies;
    FName, FExecutableName: string;
    GatheringFiles: TCastleStringList; //< only for GatherFile
    ProjectPath, DataPath: string;
    IncludePaths, ExcludePaths: TCastleStringList;
    procedure GatherFile(const FileInfo: TFileInfo);
    procedure AddDependency(const Dependency: TDependency; const FileInfo: TFileInfo);
    procedure FoundTtf(const FileInfo: TFileInfo);
    procedure FoundGz(const FileInfo: TFileInfo);
    procedure FoundPng(const FileInfo: TFileInfo);
    procedure FoundWav(const FileInfo: TFileInfo);
    procedure FoundOgg(const FileInfo: TFileInfo);
  public
    constructor Create(const Path: string);
    destructor Destroy; override;
    property Dependencies: TDependencies read FDependencies;
    property Name: string read FName;
    property ExecutableName: string read FExecutableName;
    procedure DoPackage(const OS: TOS; const CPU: TCPU);
  end;

function DependencyToString(const D: TDependency): string;
function StringToDependency(const S: string): TDependency;

var
  Verbose: boolean;

implementation

uses SysUtils, StrUtils, DOM, Process, Classes,
  CastleUtils, CastleURIUtils, CastleXMLUtils, CastleWarnings,
  CastleFilesUtils;

{ Copy file, making sure the destination directory exists
  (eventually creating it), and checking result. }
procedure SmartCopyFile(const Source, Dest: string);
var
  SourceFile, DestFile: TFileStream;
begin
  CheckForceDirectories(ExtractFileDir(Dest));

  SourceFile := TFileStream.Create(Source, fmOpenRead);
  try
    DestFile := TFileStream.Create(Dest, fmCreate);
    try
      DestFile.CopyFrom(SourceFile, SourceFile.Size);
    finally FreeAndNil(SourceFile) end;
  finally FreeAndNil(DestFile) end;

{  if not CopyFile(Source, Dest) then
    raise Exception.CreateFmt('Cannot copy file from "%s" to "%s"', [Source, Dest]);}
end;

function FileSize(const FileName: string): Int64;
var
  SourceFile: TFileStream;
begin
  SourceFile := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := SourceFile.Size;
  finally FreeAndNil(SourceFile) end;
end;

{ TCastleProject ------------------------------------------------------------- }

constructor TCastleProject.Create(const Path: string);

  procedure ReadManifest;
  var
    ManifestFile: string;

    procedure AutoGuessManifest;
    var
      Contents, AutoName: string;
    begin
      Writeln('Manifest file not found, creating: ' + ManifestFile);
      AutoName := ExtractFileName(ExtractFileDir(ManifestFile));
      Contents := '<?xml version="1.0" encoding="utf-8"?>' +NL+
'<project name="' + AutoName + '">' +NL+
'</project>' + NL;
      StringToFile(ManifestFile, Contents);
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
      Writeln('Manifest file found: ' + ManifestFile);
    ManifestURL := FilenameToURISafe(ManifestFile);

    try
      URLReadXML(Doc, ManifestURL);
      Check(Doc.DocumentElement.TagName = 'project',
        'Root node of CastleEngineManifest.xml must be <project>');
      FName := Doc.DocumentElement.AttributeString('name');
      FExecutableName := Doc.DocumentElement.AttributeStringDef('executable_name', FName);

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

procedure TCastleProject.DoPackage(const OS: TOS; const CPU: TCPU);
var
  PackagePath: string;

  procedure AddExternalLibrary(const LibraryName: string);
  var
    CastleEnginePath, LibraryPath: string;
  begin
    CastleEnginePath := GetEnvironmentVariable('CASTLE_ENGINE_PATH');
    if CastleEnginePath = '' then
      OnWarning(wtMajor, 'Library', 'CASTLE_ENGINE_PATH environment library not defined, we cannot find required library ' + LibraryName);
    LibraryPath := InclPathDelim(CastleEnginePath) +
      'external_libraries' + PathDelim +
      CPUToString(CPU) + '-' + OSToString(OS) + PathDelim + LibraryName;
    if not FileExists(LibraryPath) then
      OnWarning(wtMajor, 'Library', 'Dependency library not found in ' + LibraryPath);
    SmartCopyFile(LibraryPath, PackagePath + LibraryName);
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
  PackageFileName, TemporaryDir, ProcessOutput, FullPackageFileName: string;
  ProcessExitStatus: Integer;
begin
  TemporaryDir := InclPathDelim(GetTempDir(false)) +
    ApplicationName + IntToStr(Random(1000000));
  CheckForceDirectories(TemporaryDir);
  if Verbose then
    Writeln('Created temporary dir for package: ' + TemporaryDir);

  PackagePath := InclPathDelim(TemporaryDir) + Name;
  CheckForceDirectories(PackagePath);
  PackagePath += PathDelim;

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
        Files.Add(ExecutableName + CastleArchitectures.ExeExtension(OS));

      GatheringFiles := Files;
      FindFiles(DataPath, '*', false, @GatherFile, [ffRecursive]);
      for I := 0 to IncludePaths.Count - 1 do
        FindFiles(ProjectPath + IncludePaths[I], false, @GatherFile, [ffRecursive]);
      GatheringFiles := nil;

      Exclude('*.xcf', Files);
      Exclude('*.blend*', Files);
      Exclude('*~', Files);
      for I := 0 to ExcludePaths.Count - 1 do
        Exclude(ExcludePaths[I], Files);

      for I := 0 to Files.Count - 1 do
      begin
        SmartCopyFile(ProjectPath + Files[I], PackagePath + Files[I]);
        if Verbose then
          Writeln('Package file: ' + Files[I]);
      end;
    finally FreeAndNil(Files) end;

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

    PackageFileName := Name + '-' + OSToString(OS) + '-' + CPUToString(CPU);
    if OS in AllWindowsOSes then
      PackageFileName += '.zip' else
      PackageFileName += '.tar.gz';

    if OS in AllWindowsOSes then
      RunCommandIndir(TemporaryDir, 'zip', ['-q', '-r', PackageFileName, Name], ProcessOutput, ProcessExitStatus) else
      RunCommandIndir(TemporaryDir, 'tar', ['czf', PackageFileName, Name], ProcessOutput, ProcessExitStatus);

    if Verbose then
    begin
      Writeln('Executed package process, output:');
      Writeln(ProcessOutput);
    end;

    if ProcessExitStatus <> 0 then
      raise Exception.CreateFmt('Package process exited with error, status %d', [ProcessExitStatus]);

    FullPackageFileName := ProjectPath + PackageFileName;
    DeleteFile(FullPackageFileName);
    CheckRenameFile(InclPathDelim(TemporaryDir) + PackageFileName, FullPackageFileName);
    Writeln('Created package ' + PackageFileName + ', size: ',
      (FileSize(FullPackageFileName) / (1024 * 1024)):0:2, ' MB');
  finally RemoveNonEmptyDir(TemporaryDir) end;
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