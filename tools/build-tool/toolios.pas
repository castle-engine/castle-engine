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

{ iOS specific utilities. }
unit ToolIOS;

interface

uses Classes,
  CastleUtils, CastleStringUtils,
  ToolUtils, ToolArchitectures, ToolCompile, ToolProject;

procedure CompileIOS(const Plugin: boolean;
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string;
  const SearchPaths: TStrings);

procedure LinkIOSLibrary(const CompilationWorkingDirectory, OutputFile: string);

procedure PackageIOS(const Project: TCastleProject);
procedure InstallIOS(const Project: TCastleProject);
procedure RunIOS(const Project: TCastleProject);

implementation

uses SysUtils,
  CastleImages, CastleURIUtils, CastleLog, CastleFilesUtils,
  ToolEmbeddedImages;

const
  IOSPartialLibraryName = 'lib_cge_project.a';

procedure CompileIOS(const Plugin: boolean;
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string;
  const SearchPaths: TStrings);

  procedure CompileLibrary(const OS: TOS; const CPU: TCPU);
  var
    CompilationOutput, LinkRes, OutputLibrary: string;
    LinkResContents, ObjectFiles: TCastleStringList;
    I: Integer;
  begin
    Compile(OS, CPU, Plugin, Mode, WorkingDirectory, CompileFile, SearchPaths);

    CompilationOutput := CompilationOutputPath(OS, CPU, WorkingDirectory);
    LinkRes := CompilationOutput + 'link.res';
    if not FileExists(LinkRes) then
    begin
      if Verbose then
        Writeln('link.res not found inside "', LinkRes, '", probably what we compiled was only a unit, not a library');
    end else
    begin
      OutputLibrary := CompilationOutput + IOSPartialLibraryName;

      { grep '\.o$' link.res > lib_cge_project_object_files.txt }
      LinkResContents := TCastleStringList.Create;
      try
        LinkResContents.LoadFromFile(LinkRes);

        ObjectFiles := TCastleStringList.Create;
        try
          for I := 0 to LinkResContents.Count - 1 do
            if IsSuffix('.o', LinkResContents[I]) then
              ObjectFiles.Add(LinkResContents[I]);
          ObjectFiles.SaveToFile(CompilationOutput + 'lib_cge_project_object_files.txt');
        finally FreeAndNil(ObjectFiles) end;
      finally FreeAndNil(LinkResContents) end;

      RunCommandSimple('libtool', ['-static', '-o', OutputLibrary, '-filelist',
        CompilationOutput + 'lib_cge_project_object_files.txt']);
      if not FileExists(OutputLibrary) then
        raise Exception.CreateFmt('Creating library "%s" failed', [OutputLibrary]);
    end;
  end;

begin
  CompileLibrary(iphonesim, i386);
  CompileLibrary(iphonesim, x86_64);
  CompileLibrary(darwin, arm);
  CompileLibrary(darwin, aarch64);
end;

procedure LinkIOSLibrary(const CompilationWorkingDirectory, OutputFile: string);
begin
  RunCommandSimple('libtool', ['-static', '-o', OutputFile,
    CompilationOutputPath(iphonesim, i386   , CompilationWorkingDirectory) + IOSPartialLibraryName,
    CompilationOutputPath(iphonesim, x86_64 , CompilationWorkingDirectory) + IOSPartialLibraryName,
    CompilationOutputPath(darwin   , arm    , CompilationWorkingDirectory) + IOSPartialLibraryName,
    CompilationOutputPath(darwin   , aarch64, CompilationWorkingDirectory) + IOSPartialLibraryName
  ]);
end;

procedure PackageIOS(const Project: TCastleProject);
var
  XCodeProject: string;

  { Generate files for Android project from templates. }
  procedure GenerateFromTemplates;
  begin
    Project.ExtractTemplate('ios/xcode_project/', XCodeProject);
  end;

  { Generate icons, in various sizes, from the base icon. }
  procedure GenerateIcons;
  var
    Icon: TCastleImage;

    procedure SaveResized(const Size: Integer);
    var
      OutputFile: string;
      R: TCastleImage;
    begin
      R := Icon.MakeResized(Size, Size, riLanczos);
      try
        OutputFile := Project.Name + PathDelim +
          'Images.xcassets' + PathDelim +
          'AppIcon.appiconset' + PathDelim +
          'icon-' + IntToStr(Size) + '.png';
        SaveImage(R, FilenameToURISafe(XCodeProject + OutputFile));
        if Verbose then
          Writeln('Packaging generated icon file: ' + OutputFile);
      finally FreeAndNil(R) end;
    end;

  begin
    Icon := Project.Icons.FindReadable;
    if Icon = nil then
    begin
      WritelnWarning('Icon', 'No icon in a format readable by our engine (for example, png or jpg) is specified in CastleEngineManifest.xml. Using default icon.');
      Icon := DefaultIcon;
    end;
    try
      SaveResized(57);
      SaveResized(72);
      SaveResized(76);
      SaveResized(114);
      SaveResized(120);
      SaveResized(144);
      SaveResized(152);
      SaveResized(167);
    finally
      if Icon = DefaultIcon then
        Icon := nil else
        FreeAndNil(Icon);
    end;
  end;

  { Copy project data into XCode project. }
  procedure GenerateData;
  var
    I: Integer;
    FileFrom, FileTo: string;
    Files: TCastleStringList;
  begin
    Files := TCastleStringList.Create;
    try
      Project.PackageFiles(Files, true);
      for I := 0 to Files.Count - 1 do
      begin
        FileFrom := Project.DataPath + Files[I];
        FileTo := XCodeProject + Project.Name + PathDelim +
          'data' + PathDelim + Files[I];
        SmartCopyFile(FileFrom, FileTo);
        if Verbose then
          Writeln('Packaging data file: ' + Files[I]);
      end;
    finally FreeAndNil(Files) end;
  end;

  { Copy compiled library into XCode project. }
  procedure GenerateLibrary;
  var
    OutputFile: string;
  begin
    OutputFile := ExtractFileName(Project.IOSLibraryFile);
    SmartCopyFile(Project.IOSLibraryFile, XCodeProject + OutputFile);
    if Verbose then
      Writeln('Packaging library file: ' + OutputFile);
  end;

begin
  XCodeProject := OutputPath(Project.Path) +
    'ios' + PathDelim + 'xcode_project' + PathDelim;
  if DirectoryExists(XCodeProject) then
    RemoveNonEmptyDir(XCodeProject);

  GenerateFromTemplates;
  GenerateIcons;
  GenerateData;
  GenerateLibrary;

  Writeln('XCode project has been created in:');
  Writeln('  ', XCodeProject);
  Writeln('You can open it now on Mac OS X with XCode and compile, run and publish.');
  Writeln('The generated project should compile and work out-of-the-box.');
end;

procedure InstallIOS(const Project: TCastleProject);
begin
  // TODO
  raise Exception.Create('The "install" command is not implemented for iOS right now');
end;

procedure RunIOS(const Project: TCastleProject);
begin
  // TODO
  raise Exception.Create('The "run" command is not implemented for iOS right now');
end;

end.
