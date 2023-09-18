{
  Copyright 2014-2022 Michalis Kamburelis.

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
  ToolUtils, ToolArchitectures, ToolCompile, ToolProject, ToolPackageFormat,
  ToolManifest;

var
  IosSimulatorSupport: Boolean = false;

type
  TIosArchiveType = (
    atDevelopment,
    atAdHoc,
    atAppStore
  );

{ Compile all IOS libraries (4 or 2 versions, depending on IosSimulatorSupport).

  CompilerOptions.OS andCompilerOptions.CPU are ignored by this routine.
  This routine may modify CompilerOptions contents. }
procedure CompileIOS(const Compiler: TCompiler;
  const WorkingDirectory, CompileFile: string;
  const CompilerOptions: TCompilerOptions);

procedure LinkIOSLibrary(const Compiler: TCompiler;
  const CompilationWorkingDirectory, OutputFile: string);

function PackageFormatWantsIOSArchive(const PackageFormat: TPackageFormatNoDefault;
  out ArchiveType: TIosArchiveType; out ExportMethod: String): Boolean;

procedure PackageIOS(const Project: TCastleProject;
  const UpdateOnlyCode: Boolean);
{ Call ArchiveIOS immediately after PackageIOS to perform build + archive using Xcode command-line. }
procedure ArchiveIOS(const Project: TCastleProject; const ArchiveType: TIosArchiveType);
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
  ToolEmbeddedImages, ToolIosPbxGeneration, ToolServices, ToolCommonUtils,
  ToolServicesOperations;

const
  IOSPartialLibraryName = 'lib_cge_project.a';

procedure CompileIOS(const Compiler: TCompiler;
  const WorkingDirectory, CompileFile: string;
  const CompilerOptions: TCompilerOptions);

  procedure CompileLibrary(const OS: TOS; const CPU: TCPU);
  var
    CompilationOutput, LinkRes, OutputLibrary: string;
    LinkResContents, ObjectFiles: TCastleStringList;
    I: Integer;
  begin
    CompilerOptions.OS := OS;
    CompilerOptions.CPU := CPU;
    Compile(Compiler, WorkingDirectory, CompileFile, CompilerOptions);

    { now use libtool to create a static library .a }

    CompilationOutput := CompilationOutputPath(Compiler, OS, CPU, WorkingDirectory);

    // will raise exception if not found, or ambiguous
    LinkRes := FindLinkRes(CompilationOutput);

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

    DeleteFile(LinkRes); // delete it, to allow later FindLinkRes to work

    RunCommandSimple('libtool', ['-static', '-o', OutputLibrary, '-filelist',
      CompilationOutput + 'lib_cge_project_object_files.txt']);
    if not RegularFileExists(OutputLibrary) then
      raise Exception.CreateFmt('Creating library "%s" failed', [OutputLibrary]);
  end;

begin
  { To compile CastleInternalVorbisFile properly.
    Later PackageIOS will actually add the static tremolo files to the project. }
  CompilerOptions.ExtraOptions.Add('-dCASTLE_TREMOLO_STATIC');

  if IosSimulatorSupport then
  begin
    { iPhoneSimulator for i386 is not useful, since
      - macOS on i386 is no longer supported
      - fpcupdeluxe seems to not even allow making FPC cross-compiler for iPhoneSimulator on i386 }
    //CompileLibrary(iphonesim, i386);
    CompileLibrary(iphonesim, x86_64);
  end;
  CompileLibrary(iOS, arm);
  CompileLibrary(iOS, aarch64);
end;

procedure LinkIOSLibrary(const Compiler: TCompiler; const CompilationWorkingDirectory, OutputFile: string);
var
  Options: TCastleStringList;
begin
  Options := TCastleStringList.Create;
  try
    Options.Add('-static');
    Options.Add('-o');
    Options.Add(OutputFile);
    if IosSimulatorSupport then
    begin
      { iPhoneSimulator for i386 is not useful, since
        - macOS on i386 is no longer supported
        - fpcupdeluxe seems to not even allow making FPC cross-compiler for iPhoneSimulator on i386 }
      //Options.Add(CompilationOutputPath(Compiler, iphonesim, i386   , CompilationWorkingDirectory) + IOSPartialLibraryName);
      Options.Add(CompilationOutputPath(Compiler, iphonesim, x86_64 , CompilationWorkingDirectory) + IOSPartialLibraryName);
    end;
    Options.Add(CompilationOutputPath(Compiler, iOS   , arm    , CompilationWorkingDirectory) + IOSPartialLibraryName);
    Options.Add(CompilationOutputPath(Compiler, iOS   , aarch64, CompilationWorkingDirectory) + IOSPartialLibraryName);
    RunCommandSimple('libtool', Options.ToArray);
  finally FreeAndNil(Options) end;
end;

procedure PackageIOS(const Project: TCastleProject;
  const UpdateOnlyCode: Boolean);
var
  XcodeProject: string;
  UsesCocoaPods: boolean;

  { Generate files for iOS project from templates. }
  procedure GenerateFromTemplates;
  begin
    Project.ExtractTemplate('ios/xcode_project/', XcodeProject);
  end;

  { Generate files for iOS project from templates, adding services. }
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

    // Since right now we always compile with CASTLE_TREMOLO_STATIC,
    // we just always add ogg_vorbis service, otherwise it would not link.
    if {(depOggVorbis in Project.Dependencies) and}
       not Project.IOSServices.HasService('ogg_vorbis') then
      ExtractService('ogg_vorbis');
    // Since right now we always compile with CASTLE_FREETYPE_STATIC,
    // we just always add freetype service, otherwise it would not link.
    if {(depFreeType in Project.Dependencies) and}
       not Project.IOSServices.HasService('freetype') then
      ExtractService('freetype');
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

  { Generate "Launch Screen.storyboard" and LaunchScreenImage.png it references. }
  procedure GenerateLaunchImageStoryboard;
  var
    Storyboard, ProjectOutputUrl, ImageUrl: String;
  begin
    if Project.IOSHasLaunchImageStoryboard then
    begin
      ProjectOutputUrl := FilenameToURISafe(XcodeProject + Project.Name + PathDelim);

      Storyboard := FileToString('castle-data:/ios/Launch%20Screen.storyboard');
      Storyboard := Project.ReplaceMacros(Storyboard);
      StringToFile(ProjectOutputUrl + 'Launch%20Screen.storyboard', Storyboard);

      ImageUrl := CombineURI(
        Project.LaunchImageStoryboard.BaseUrl,
        Project.LaunchImageStoryboard.Path);
      CheckCopyFile(
        URIToFilenameSafe(ImageUrl),
        URIToFilenameSafe(ProjectOutputUrl + 'LaunchScreenImage.png'));
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
  begin
    Project.CopyData(XcodeProject + Project.Name + PathDelim + 'data', cpIOS);
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

      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('Foundation.framework'));
      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('CoreGraphics.framework'));
      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('UIKit.framework'));
      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('OpenGLES.framework'));
      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('GLKit.framework'));
      PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('OpenAL.framework'));

      // TODO: These service-specific things should be defined in respective service CastleEngineService.xml
      if Project.IOSServices.HasService('apple_game_center') then
        PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('GameKit.framework'));
      if Project.IOSServices.HasService('in_app_purchases') then
        PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('StoreKit.framework'));
      if Project.IOSServices.HasService('fmod') then
      begin
        PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('libfmod_iphoneos.a'));
        PbxProject.Frameworks.Add(TXcodeProjectFramework.Create('libc++.tbd'));
      end;

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
    OutputFileBase: string;
  begin
    OutputFileBase := ExtractFileName(Project.IOSLibraryFile);
    SmartCopyFile(Project.IOSLibraryFile, XcodeProject + OutputFileBase);
    if Verbose then
      Writeln('Packaging library file: ' + OutputFileBase);
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

  if UpdateOnlyCode then
  begin
    if not DirectoryExists(XcodeProject) then
      raise Exception.CreateFmt('Project directory "%s" doesn''t exist. Run "package" without the --update-only-code option', [
        XcodeProject
      ]);
    GenerateLibrary;
  end else
  begin
    if DirectoryExists(XcodeProject) then
      RemoveNonEmptyDir(XcodeProject);

    GenerateFromTemplates;
    GenerateServicesFromTemplates;
    PackageServices(Project, Project.IOSServices,
      'castle-data:/ios/services/', XcodeProject);
    GenerateLaunchImageStoryboard;
    FixPbxProjectFile; // must be done *after* all files that have to be in PBX are in place, so after PackageServices and GenerateLaunchImageStoryboard
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
end;

function PackageFormatWantsIOSArchive(const PackageFormat: TPackageFormatNoDefault;
  out ArchiveType: TIosArchiveType; out ExportMethod: String): Boolean;
begin
  case PackageFormat of
    pfIosArchiveDevelopment:
      begin
        ArchiveType := atDevelopment;
        ExportMethod := 'development';
        Result := true;
      end;
    pfIosArchiveAdHoc:
      begin
        ArchiveType := atAdHoc;
        ExportMethod := 'ad-hoc';
        Result := true;
      end;
    pfIosArchiveAppStore:
      begin
        ArchiveType := atAppStore;
        ExportMethod := 'app-store';
        Result := true;
      end;
    else Result := false;
  end;
end;

procedure ArchiveIOS(const Project: TCastleProject; const ArchiveType: TIosArchiveType);
var
  XcodeProject: String;
  XcodeSelectExe, XcodeBuildExe: String;
  OutputString, ArchivePath, ExportPath: String;
  ExitStatus: Integer;
begin
  XcodeProject := TempOutputPath(Project.Path) +
    'ios' + PathDelim + 'xcode_project' + PathDelim; // same as in PackageIOS
  ArchivePath := TempOutputPath(Project.Path) +
    'ios' + PathDelim + 'App.xcarchive';
  ExportPath := TempOutputPath(Project.Path) +
    'ios' + PathDelim + 'build' + PathDelim;

  if not DirectoryExists(XcodeProject) then
    raise Exception.CreateFmt('Cannot read created Xcode project in "%s"', [XcodeProject]);

  XcodeSelectExe := FindExe('xcode-select');
  if XcodeSelectExe = '' then
    raise Exception.Create('Cannot find "xcode-select". Make sure that Xcode with command-line utilities is installed.');

  MyRunCommandIndir(XcodeProject, XcodeSelectExe, ['--print-path'], OutputString, ExitStatus);

  if ExitStatus <> 0 then
    raise Exception.CreateFmt('Running "xcode-select" failed, exit status %d, output "%s".', [
      ExitStatus,
      OutputString
    ]);

  if Trim(OutputString) = '/Library/Developer/CommandLineTools' then
    WritelnWarning('xcode-select points to only command-line utilities, but not Xcode path. In new Xcode versions, you should do something like "sudo xcode-select -switch /Applications/Xcode.app".');

  Writeln('Using Xcode from: ' + Trim(OutputString));

  { See
    https://developer.apple.com/library/archive/technotes/tn2339/_index.html
    https://stackoverflow.com/questions/25678372/xcodebuild-cant-build-when-no-physical-ios
    about these commands.

    xcodebuild -list -workspace castle-engine-output/ios/xcode_project/PROJ_NAME.xcworkspace
  }

  XcodeBuildExe := FindExe('xcodebuild');
  if XcodeBuildExe = '' then
    raise Exception.Create('Cannot find "xcodebuild". Make sure that Xcode with command-line utilities is installed.');

  if DirectoryExists(ArchivePath) then
    RemoveNonEmptyDir(ArchivePath);
  // do not create the archive path, it would be malformed archive to Xcode then

  if DirectoryExists(ExportPath) then
    RemoveNonEmptyDir(ExportPath);
  CheckForceDirectories(ExportPath);

  RunCommandSimple(XcodeProject, XcodeBuildExe, [
    '-allowProvisioningUpdates',
    '-workspace', Project.Name + '.xcworkspace',
    '-scheme', Project.Caption,
    '-destination', 'generic/platform=iOS',
    '-quiet',
    '-archivePath', ArchivePath,
    'archive'
  ]);
  RunCommandSimple(XcodeProject, XcodeBuildExe, [
    '-allowProvisioningUpdates',
    '-archivePath', ArchivePath,
    '-exportOptionsPlist', XcodeProject + 'export_options.plist',
    '-exportArchive',
    '-exportPath', ExportPath
  ]);

  // TODO: It could be a bit cleaner to keep export_options.plist outside of Xcode project dir,
  // and copy it here to the final place,
  // replacing IOS_EXPORT_METHOD by a special code in this unit.
  // This would allow to simplify PackageFormatWantsIOSArchive?
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
