{
  Copyright 2019-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes, Zipper, URIParser,
  CastleWindow, CastleScene, CastleControls, CastleLog, CastleUtils,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleApplicationProperties, CastleDownload, CastleStringUtils,
  CastleURIUtils, CastleViewport, CastleCameras;

var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  Status: TCastleLabel;
  ExampleImage: TCastleImageControl;
  ExampleScene: TCastleScene;

{ TPackedDataReader ---------------------------------------------------------- }

type
  TPackedDataReader = class
  public
    SourceZipFileName: String;
    TempDirectory: String;
    function ReadUrl(const Url: string; out MimeType: string): TStream;
    destructor Destroy; override;
  end;

var
  PackedDataReader: TPackedDataReader;

function TPackedDataReader.ReadUrl(const Url: string; out MimeType: string): TStream;
var
  U: TURI;
  FileInZip: String;
  Unzip: TUnZipper;
  FilesInZipList: TStringlist;
begin
  U := ParseURI(Url);
  FileInZip := PrefixRemove('/', U.Path + U.Document, false);

  { Unpack file to a temporary directory.
    TODO: TPackedDataReader.ReadUrl should be implemented
    without storing the temp file on disk. }
  Unzip := TUnZipper.Create;
  try
    Unzip.FileName := SourceZipFileName;
    Unzip.OutputPath := TempDirectory;
    FilesInZipList := TStringlist.Create;
    try
      FilesInZipList.Add(FileInZip);
      Unzip.UnZipFiles(FilesInZipList);
    finally FreeAndNil(FilesInZipList) end;
  finally FreeAndNil(Unzip) end;

  { Use Download with file:/ protocol to load filename to TStream }
  Result := Download(FilenameToURISafe(CombinePaths(TempDirectory, FileInZip)), [], MimeType);
end;

destructor TPackedDataReader.Destroy;
begin
  RemoveNonEmptyDir(TempDirectory, true);
  inherited;
end;

{ routines ------------------------------------------------------------------- }

procedure WindowUpdate(Container: TCastleContainer);
begin
  // ... do something every frame
  Status.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure WindowPress(Container: TCastleContainer; const Event: TInputPressRelease);
begin
  // ... react to press of key, mouse, touch
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { initialize PackedDataReader to read ZIP when we access my-packed-data:/ }
  PackedDataReader := TPackedDataReader.Create;
  PackedDataReader.SourceZipFileName := URIToFilenameSafe('castle-data:/packed_game_data.zip');
  PackedDataReader.TempDirectory := InclPathDelim(GetTempDir) + 'unpacked_data';
  ForceDirectories(PackedDataReader.TempDirectory);
  WritelnLog('Using temporary directory "%s"', [PackedDataReader.TempDirectory]);
  RegisterUrlProtocol('my-packed-data', @PackedDataReader.ReadUrl, nil);

  { make following calls to castle-data:/ also load data from ZIP }
  ApplicationDataOverride := 'my-packed-data:/';

  { Assign Window callbacks }
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;

  { For a scalable UI (adjusts to any window size in a smart way), use UIScaling }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.InsertBack(TCastleExamineNavigation.Create(Application));
  Window.Controls.InsertFront(Viewport);

  { Show a label with frames per second information }
  Status := TCastleLabel.Create(Application);
  Status.Anchor(vpTop, -10);
  Status.Anchor(hpRight, -10);
  Status.Color := Yellow; // you could also use "Vector4(1, 1, 0, 1)" instead of Yellow
  Window.Controls.InsertFront(Status);

  { Show 2D image }
  ExampleImage := TCastleImageControl.Create(Application);
  ExampleImage.URL := 'castle-data:/example_image.png';
  // This also works:
  // 'my-packed-data:/example_image.png';
  ExampleImage.Anchor(vpBottom, 100);
  ExampleImage.Anchor(hpLeft, 100);
  Window.Controls.InsertFront(ExampleImage);

  { Show a 3D object (TCastleScene) inside a Viewport
    (which acts as a full-screen viewport by default). }
  ExampleScene := TCastleScene.Create(Application);
  ExampleScene.Load('castle-data:/example_scene.x3dv');
  ExampleScene.PreciseCollisions := true;

  Viewport.Items.Add(ExampleScene);

  // headlight
  Viewport.Camera.Add(TCastleDirectionalLight.Create(Application));

  // nice initial camera position
  Viewport.AssignDefaultCamera;
end;

initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  { Optionally, adjust window fullscreen state and size at this point.
    Examples:

    Run fullscreen:

      Window.FullScreen := true;

    Run in a 600x400 window:

      Window.FullScreen := false; // default
      Window.Width := 600;
      Window.Height := 400;

    Run in a window taking 2/3 of screen (width and height):

      Window.FullScreen := false; // default
      Window.Width := Application.ScreenWidth * 2 div 3;
      Window.Height := Application.ScreenHeight * 2 div 3;

    Note that some platforms (like mobile) ignore these window sizes.
  }

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
finalization
  FreeAndNil(PackedDataReader);
end.
