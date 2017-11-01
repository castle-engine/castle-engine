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
  const SearchPaths, ExtraOptions: TStrings);

procedure LinkIOSLibrary(const CompilationWorkingDirectory, OutputFile: string);

procedure PackageIOS(const Project: TCastleProject);
procedure InstallIOS(const Project: TCastleProject);
procedure RunIOS(const Project: TCastleProject);

implementation

uses SysUtils,
  CastleImages, CastleURIUtils, CastleLog, CastleFilesUtils,
  ToolEmbeddedImages, ToolIosPbxGeneration, ToolServices;

const
  IOSPartialLibraryName = 'lib_cge_project.a';

procedure CompileIOS(const Plugin: boolean;
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string;
  const SearchPaths, ExtraOptions: TStrings);

  procedure CompileLibrary(const OS: TOS; const CPU: TCPU);
  var
    CompilationOutput, LinkRes, OutputLibrary: string;
    LinkResContents, ObjectFiles: TCastleStringList;
    I: Integer;
  begin
    Compile(OS, CPU, Plugin, Mode, WorkingDirectory, CompileFile, SearchPaths, ExtraOptions);

    { now use libtool to create a static library .a }

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

  { Generate files for iOS project from templates. }
  procedure GenerateFromTemplates;
  begin
    Project.ExtractTemplate('ios/xcode_project/', XCodeProject);
  end;

  procedure GenerateServicesFromTemplates;

    procedure ExtractService(const ServiceName: string);
    begin
      Project.ExtractTemplate('ios/services/' + ServiceName + '/', XCodeProject);
    end;

  var
    S: TService;
  begin
    for S in Project.IOSServices do
      ExtractService(S.Name);

    if (depOggVorbis in Project.Dependencies) and
       not Project.IOSServices.HasService('ogg_vorbis') then
      ExtractService('ogg_vorbis');
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
      { Use DefaultIconSquare, not DefaultIcon for iOS, since we cannot have
        transparency on iOS icon (it's replaced by an ugly blackness).
        See
        https://stackoverflow.com/questions/959864/is-is-possible-to-use-transparency-in-an-iphone-app-icon
        https://stackoverflow.com/questions/22858501/ios-app-icon-with-transparent-background-showing-black-background-on-device }
      Icon := DefaultIconSquare;
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
      SaveResized(1024);
    finally
      if Icon = DefaultIconSquare then
        Icon := nil else
        FreeAndNil(Icon);
    end;
  end;

  { Generate launch images, in various sizes,
    from the most suitable launch images specified. }
  procedure GenerateLaunchImages;
  var
    LaunchImages: TCastleImageList;

    function FindBestMatching(const Width, Height: Integer;
      const DefaultImage: TCastleImage): TCastleImage;
    var
      I: Integer;
      ResultIndex: Integer;
    begin
      ResultIndex := -1;
      Result := nil;

      for I := 0 to LaunchImages.Count - 1 do
        if (Result = nil) or
           (Abs(LaunchImages[I].Width / LaunchImages[I].Height - Width / Height) <
            Abs(         Result.Width /          Result.Height - Width / Height)) then
        begin
          ResultIndex := I;
          Result := LaunchImages[ResultIndex];
        end;

      if Result = nil then
      begin
        Result := DefaultImage;
        if Verbose then
          Writeln('iOS lauch image: No custom launch image, using the default for size ', Width, ' x ', Height);
      end else
      begin
        if Verbose then
          Writeln('iOS lauch image: Using the (resized) launch image "' + Project.LaunchImages[ResultIndex] + '" as it has the best aspect ratio for size ', Width, ' x ', Height);
      end;
    end;

    procedure SaveResized(const Width, Height: Integer;
      const DefaultImage: TCastleImage);
    var
      OutputFile: string;
      ImageSource, R: TCastleImage;
    begin
      ImageSource := FindBestMatching(Width, Height, DefaultImage);
      R := ImageSource.MakeResized(Width, Height, riLanczos);
      try
        OutputFile := Project.Name + PathDelim +
          'Images.xcassets' + PathDelim +
          'LaunchImage.launchimage' + PathDelim +
          'launch-image-' + IntToStr(Width) + 'x' + IntToStr(Height) + '.png';
        SaveImage(R, FilenameToURISafe(XCodeProject + OutputFile));
        // it's already reported by FindBestMatching
        // if Verbose then
        //   Writeln('Packaging generated launch icon file: ' + OutputFile);
      finally FreeAndNil(R) end;
    end;

  var
    I: Integer;
    Default640x1136, Default1536x2048, Default2048x1536: TCastleImage;
  begin
    Default640x1136 := nil;
    Default1536x2048 := nil;
    Default2048x1536 := nil;
    LaunchImages := TCastleImageList.Create(true);
    try
      Default640x1136 := LoadImage(ApplicationData('default_launch_images/DefaultLaunchImage640x1136.png'));
      Default1536x2048 := LoadImage(ApplicationData('default_launch_images/DefaultLaunchImage1536x2048.png'));
      Default2048x1536 := LoadImage(ApplicationData('default_launch_images/DefaultLaunchImage2048x1536.png'));

      for I := 0 to Project.LaunchImages.Count - 1 do
        LaunchImages.Add(LoadImage(Project.LaunchImages[I]));
      // iPhone Portrait
      SaveResized(640, 1136, Default640x1136);
      // iPad Landscape
      SaveResized(1024, 768, Default2048x1536);
      // iPad Landscape x2
      SaveResized(1024 * 2, 768 * 2, Default2048x1536);
      // iPad Portrait
      SaveResized(768, 1024, Default1536x2048);
      // iPad Portrait x2
      SaveResized(768 * 2, 1024 * 2, Default1536x2048);
    finally FreeAndNil(LaunchImages) end;
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

  (* Add a large auto-generated chunk into the pbx file, replacing a special macro
    ${PBX_CONTENTS_GENERATED} inside the pbx file. *)
  procedure FixPbxProjectFile;
  var
    PbxProject: TXCodeProject;
    PBXContentsGenerated, PBX, PBXFileUrl: string;
  begin
    PbxProject := TXCodeProject.Create;
    try
      PbxProject.AddTopLevelDir(XCodeProject, Project.Name);

      PbxProject.Frameworks.Add(TXCodeProjectFramework.Create('Foundation'));
      PbxProject.Frameworks.Add(TXCodeProjectFramework.Create('CoreGraphics'));
      PbxProject.Frameworks.Add(TXCodeProjectFramework.Create('UIKit'));
      PbxProject.Frameworks.Add(TXCodeProjectFramework.Create('OpenGLES'));
      PbxProject.Frameworks.Add(TXCodeProjectFramework.Create('GLKit'));
      PbxProject.Frameworks.Add(TXCodeProjectFramework.Create('OpenAL'));

      if Project.IOSServices.HasService('apple_game_center') then
        PbxProject.Frameworks.Add(TXCodeProjectFramework.Create('GameKit'));

      PBXContentsGenerated := PbxProject.PBXContents;
      // process macros inside PBXContentsGenerated, to replace ${NAME} etc. inside
      PBXContentsGenerated := Project.ReplaceMacros(PBXContentsGenerated);

      PBXFileUrl := FilenameToURISafe(
        XCodeProject + Project.Name + '.xcodeproj' + PathDelim + 'project.pbxproj');
      PBX := FileToString(PBXFileUrl);
      StringReplaceAllVar(PBX, '${PBX_CONTENTS_GENERATED}', PBXContentsGenerated, false);
      StringToFile(PBXFileUrl, PBX);
    finally FreeAndNil(PbxProject) end;
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
  GenerateServicesFromTemplates;
  FixPbxProjectFile; // must be done *after* all files for services are created
  GenerateIcons;
  GenerateLaunchImages;
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
