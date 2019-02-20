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

{ iOS specific utilities. }
unit ToolIOS;

interface

uses Classes,
  CastleUtils, CastleStringUtils,
  ToolUtils, ToolArchitectures, ToolCompile, ToolProject;

procedure CompileIOS(
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string;
  const SearchPaths, LibraryPaths, ExtraOptions: TStrings);

procedure LinkIOSLibrary(const CompilationWorkingDirectory, OutputFile: string);

procedure PackageIOS(const Project: TCastleProject);
procedure InstallIOS(const Project: TCastleProject);
procedure RunIOS(const Project: TCastleProject);

procedure MergeIOSAppDelegate(const Source, Destination: string;
  const ReplaceMacros: TReplaceMacros);
procedure MergeIOSPodfile(const Source, Destination: string;
  const ReplaceMacros: TReplaceMacros);
procedure MergeIOSInfoPlist(const Source, Destination: string;
  const ReplaceMacros: TReplaceMacros);

implementation

uses SysUtils, DOM,
  CastleImages, CastleURIUtils, CastleLog, CastleFilesUtils, CastleXMLUtils,
  ToolEmbeddedImages, ToolIosPbxGeneration, ToolServices;

const
  IOSPartialLibraryName = 'lib_cge_project.a';

procedure CompileIOS(
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string;
  const SearchPaths, LibraryPaths, ExtraOptions: TStrings);

  procedure CompileLibrary(const OS: TOS; const CPU: TCPU);
  var
    CompilationOutput, LinkRes, OutputLibrary: string;
    LinkResContents, ObjectFiles: TCastleStringList;
    I: Integer;
  begin
    Compile(OS, CPU, { Plugin } false, Mode, WorkingDirectory, CompileFile,
      SearchPaths, LibraryPaths, ExtraOptions);

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
  XcodeProject: string;
  UsesCocoaPods: boolean;

  { Generate files for iOS project from templates. }
  procedure GenerateFromTemplates;
  begin
    Project.ExtractTemplate('ios/xcode_project/', XcodeProject);
  end;

  procedure GenerateServicesFromTemplates;

    procedure ExtractService(const ServiceName: string);
    var
      TemplatePath: string;
    begin
      TemplatePath := 'ios/services/' + ServiceName + '/';
      Project.ExtractTemplate(TemplatePath, XcodeProject);

      if URIFileExists(ApplicationData(TemplatePath + 'Podfile')) then
      begin
        if Verbose then
          Writeln(Format('Service "%s" requires using CocoaPods. Make sure you have CocoaPods ( https://cocoapods.org/ ) installed.',
            [ServiceName]));
        UsesCocoaPods := true;
      end;
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
      R := Icon.MakeResized(Size, Size, BestInterpolation);
      try
        OutputFile := Project.Name + PathDelim +
          'Images.xcassets' + PathDelim +
          'AppIcon.appiconset' + PathDelim +
          'icon-' + IntToStr(Size) + '.png';
        SaveImage(R, FilenameToURISafe(XcodeProject + OutputFile));
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
      R := ImageSource.MakeResized(Width, Height, BestInterpolation);
      try
        OutputFile := Project.Name + PathDelim +
          'Images.xcassets' + PathDelim +
          'LaunchImage.launchimage' + PathDelim +
          'launch-image-' + IntToStr(Width) + 'x' + IntToStr(Height) + '.png';
        SaveImage(R, FilenameToURISafe(XcodeProject + OutputFile));
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

  { Copy project data into Xcode project. }
  procedure GenerateData;
  var
    I: Integer;
    OutputDataFolder: string;
    FileFrom, FileTo: string;
    Files: TCastleStringList;
  begin
    OutputDataFolder := XcodeProject + Project.Name + PathDelim + 'data';
    ForceDirectories(OutputDataFolder);    // create folder even if project does not contain any files (is referenced in Xcode project)
    Files := Project.PackageFiles(true);
    try
      for I := 0 to Files.Count - 1 do
      begin
        FileFrom := Project.DataPath + Files[I];
        FileTo := OutputDataFolder + PathDelim + Files[I];
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
    PbxProject: TXcodeProject;
    PBXContentsGenerated, PBX, PBXFileUrl: string;
  begin
    PbxProject := TXcodeProject.Create;
    try
      PbxProject.AddTopLevelDir(XcodeProject, Project.Name);

      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('Foundation'));
      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('CoreGraphics'));
      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('UIKit'));
      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('OpenGLES'));
      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('GLKit'));
      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('OpenAL'));

      if Project.IOSServices.HasService('apple_game_center') then
        PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('GameKit'));
      if Project.IOSServices.HasService('in_app_purchases') then
        PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('StoreKit'));

      PBXContentsGenerated := PbxProject.PBXContents;
      // process macros inside PBXContentsGenerated, to replace ${NAME} etc. inside
      PBXContentsGenerated := Project.ReplaceMacros(PBXContentsGenerated);

      PBXFileUrl := FilenameToURISafe(
        XcodeProject + Project.Name + '.xcodeproj' + PathDelim + 'project.pbxproj');
      PBX := FileToString(PBXFileUrl);
      StringReplaceAllVar(PBX, '${PBX_CONTENTS_GENERATED}', PBXContentsGenerated, false);
      StringToFile(PBXFileUrl, PBX);
    finally FreeAndNil(PbxProject) end;
  end;

  { Copy compiled library into Xcode project. }
  procedure GenerateLibrary;
  var
    OutputFile: string;
  begin
    OutputFile := ExtractFileName(Project.IOSLibraryFile);
    SmartCopyFile(Project.IOSLibraryFile, XcodeProject + OutputFile);
    if Verbose then
      Writeln('Packaging library file: ' + OutputFile);
  end;

  procedure GenerateCocoaPods;
  begin
    if UsesCocoaPods then
      RunCommandSimple(XcodeProject, 'pod', ['install']);
  end;

begin
  UsesCocoaPods := false;
  XcodeProject := TempOutputPath(Project.Path) +
    'ios' + PathDelim + 'xcode_project' + PathDelim;
  if DirectoryExists(XcodeProject) then
    RemoveNonEmptyDir(XcodeProject);

  GenerateFromTemplates;
  GenerateServicesFromTemplates;
  FixPbxProjectFile; // must be done *after* all files for services are created
  GenerateIcons;
  GenerateLaunchImages;
  GenerateData;
  GenerateLibrary;
  GenerateCocoaPods; // should be at the end, to allow CocoaPods to see our existing project

  Writeln('Xcode project has been created in:');
  Writeln('  ', XcodeProject);
  Writeln('You can open it now on macOS with Xcode and compile, run and publish.');
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

type
  ECannotMergeTemplate = class(Exception);

procedure MergeIOSAppDelegate(const Source, Destination: string;
  const ReplaceMacros: TReplaceMacros);
const
  MarkerImport = '/* IOS-SERVICES-IMPORT */';
  MarkerCreate = '/* IOS-SERVICES-CREATE */';
  CreateTemplate =
    '{' + NL +
    '%s* serviceInstance;' + NL +
    'serviceInstance = [[%0:s alloc] init];' + NL +
    'serviceInstance.mainController = viewController;' + NL +
    'serviceInstance.window = self.window;' + NL +
    '[services addObject: serviceInstance];' + NL +
    '}';

var
  DestinationContents: string;

  procedure InsertAtMarker(const Marker, Insertion: string);
  var
    MarkerPos: Integer;
  begin
    MarkerPos := Pos(Marker, DestinationContents);
    if MarkerPos = 0 then
      raise ECannotMergeTemplate.CreateFmt('Cannot find marker "%s" in AppDelegate.m', [Marker]);
    Insert(Trim(Insertion) + NL, DestinationContents, MarkerPos);
  end;

var
  SourceDocument: TXMLDocument;
  Import, CreateClass, CreateCode: string;
begin
  SourceDocument := URLReadXML(Source);
  try
    if SourceDocument.DocumentElement.TagName <> 'app_delegate_patch' then
      raise ECannotMergeTemplate.Create('The source file from which to merge AppDelegate.m must be XML with root <app_delegate_patch>');
    Import := SourceDocument.DocumentElement.ChildElement('import').TextData;
    CreateClass := SourceDocument.DocumentElement.ChildElement('class').TextData;
  finally FreeAndNil(SourceDocument) end;

  CreateCode := Format(CreateTemplate, [CreateClass]);

  DestinationContents := FileToString(FilenameToURISafe(Destination));
  InsertAtMarker(MarkerImport, Import);
  InsertAtMarker(MarkerCreate, CreateCode);
  StringToFile(Destination, DestinationContents);
end;

procedure MergeIOSPodfile(const Source, Destination: string;
  const ReplaceMacros: TReplaceMacros);
var
  DestinationContents: string;

  procedure InsertAtMarker(const Marker, Insertion: string);
  var
    MarkerPos: Integer;
  begin
    MarkerPos := Pos(Marker, DestinationContents);
    if MarkerPos = 0 then
      raise ECannotMergeTemplate.CreateFmt('Cannot find marker "%s" in Podfile', [Marker]);
    Insert(Insertion, DestinationContents, MarkerPos);
  end;

const
  Marker = '### SERVICES-PODFILES ###';
var
  SourceContents: string;
begin
  SourceContents := NL +
    '# ---- Inserted contents of ' + Source + NL +
    Trim(ReplaceMacros(FileToString(FilenameToURISafe(Source)))) + NL +
    '# ---- End of inserted contents of ' + Source + NL + NL;

  DestinationContents := FileToString(FilenameToURISafe(Destination));
  InsertAtMarker(Marker, SourceContents);
  StringToFile(Destination, DestinationContents);
end;

procedure MergeIOSInfoPlist(const Source, Destination: string;
  const ReplaceMacros: TReplaceMacros);
var
  DestinationContents: string;

  procedure InsertAtMarker(const Marker, Insertion: string);
  var
    MarkerPos: Integer;
  begin
    MarkerPos := Pos(Marker, DestinationContents);
    if MarkerPos = 0 then
      raise ECannotMergeTemplate.CreateFmt('Cannot find marker "%s" in xxx-Info.plist', [Marker]);
    Insert(Insertion, DestinationContents, MarkerPos);
  end;

const
  Marker = '<!-- IOS-SERVICES-PLIST -->';
var
  SourceContents: string;
begin
  SourceContents := NL +
    '<!-- Inserted contents of ' + Source + ' -->' + NL +
    Trim(ReplaceMacros(FileToString(FilenameToURISafe(Source)))) + NL +
    '<!-- End of inserted contents of ' + Source + ' -->' + NL + NL;

  DestinationContents := FileToString(FilenameToURISafe(Destination));
  InsertAtMarker(Marker, SourceContents);
  StringToFile(Destination, DestinationContents);
end;

end.
