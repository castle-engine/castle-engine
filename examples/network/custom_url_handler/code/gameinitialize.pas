{
  Copyright 2019-2022 Michalis Kamburelis.

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
  Viewport.AutoCamera := true;
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
  ExampleImage.Bottom := 100;
  ExampleImage.Left := 100;
  Window.Controls.InsertFront(ExampleImage);

  { Show a 3D object (TCastleScene) inside a Viewport
    (which acts as a full-screen viewport by default). }
  ExampleScene := TCastleScene.Create(Application);
  ExampleScene.Load('castle-data:/example_scene.x3dv');
  ExampleScene.Spatial := [ssRendering, ssDynamicCollisions];
  ExampleScene.ProcessEvents := true;

  Viewport.Items.Add(ExampleScene);
  Viewport.Items.MainScene := ExampleScene;
end;

initialization
  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Window.ParseParameters; // allows to control window size / fullscreen on the command-line
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (in case of non-desktop platforms, some necessary may not be prepared yet). }
finalization
  FreeAndNil(PackedDataReader);
end.
