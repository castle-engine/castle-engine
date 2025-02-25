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

uses SysUtils, Classes, URIParser,
  CastleWindow, CastleScene, CastleControls, CastleLog, CastleUtils,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleApplicationProperties, CastleDownload, CastleStringUtils,
  CastleUriUtils, CastleViewport, CastleCameras, CastleZip;

var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  Status: TCastleLabel;
  ExampleImage: TCastleImageControl;
  ExampleScene: TCastleScene;

{ When using TCastleZip, we install the custom URL handler easier,
  using the TCastleZip.RegisterUrlProtocol method.
  This is simpler and is more efficient (the ZIP file is only opened once).

  Undefine this to show a more generic way of registering custom URL handler,
  using the RegisterUrlProtocol routine from the CastleDownload unit.
  It doens't really make sense to use it for ZIP reading (using TCastleZip
  is both simpler and more efficient) but it illustrates how to register
  custom URL handler in a more generic way, for any purpose. }
{$define USE_ZIP_URL_HANDLER}
{$ifdef USE_ZIP_URL_HANDLER}
var
  PackedDataZip: TCastleZip;
{$else USE_ZIP_URL_HANDLER}

{ TPackedDataReader ---------------------------------------------------------- }

type
  TPackedDataReader = class
  public
    SourceZipFileName: String;
    function ReadUrl(const Url: String; out MimeType: string): TStream;
    destructor Destroy; override;
  end;

var
  PackedDataReader: TPackedDataReader;

function TPackedDataReader.ReadUrl(const Url: String; out MimeType: string): TStream;

  { Unpack single file from zip, to a TStream.
    FileInZip should be relative path within the zip archive. }
  function UnzipFile(const ZipFileName, FileInZip: String): TStream;
  var
    Zip: TCastleZip;
  begin
    Zip := TCastleZip.Create;
    try
      Zip.Open(ZipFileName);
      Result := Zip.Read(FileInZip);
    finally FreeAndNil(Zip) end;
  end;

var
  U: TURI;
  FileInZip: String;
begin
  U := ParseURI(Url);
  FileInZip := PrefixRemove('/', U.Path + U.Document, false);
  Result := UnzipFile(SourceZipFileName, FileInZip);

  { Determine mime type from Url, which practically means:
    determine content type from filename extension. }
  MimeType := UriMimeType(Url);
end;

destructor TPackedDataReader.Destroy;
begin
  inherited;
end;

{$endif USE_ZIP_URL_HANDLER}

{ View ----------------------------------------------------------------------- }

type
  { View to contain whole UI and to handle events, like key press. }
  TMyView = class(TCastleView)
  end;

var
  MyView: TMyView;

{ routines ------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  {$ifdef USE_ZIP_URL_HANDLER}
  PackedDataZip := TCastleZip.Create;
  PackedDataZip.Open('castle-data:/packed_game_data.zip');
  PackedDataZip.RegisterUrlProtocol('my-packed-data');
  {$else}
  { initialize PackedDataReader to read ZIP when we access my-packed-data:/ }
  PackedDataReader := TPackedDataReader.Create;
  PackedDataReader.SourceZipFileName := UriToFilenameSafe('castle-data:/packed_game_data.zip');
  RegisterUrlProtocol('my-packed-data',
    {$ifdef FPC}@{$endif} PackedDataReader.ReadUrl, nil);
  {$endif}

  { make following calls to castle-data:/ also load data from ZIP }
  ApplicationDataOverride := 'my-packed-data:/';

  { For a scalable UI (adjusts to any window size in a smart way), use UIScaling }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  MyView := TMyView.Create(Application);
  Window.Container.View := MyView;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.InsertBack(TCastleExamineNavigation.Create(Application));
  MyView.InsertFront(Viewport);

  { Show a label with frames per second information }
  Status := TCastleLabel.Create(Application);
  Status.Anchor(vpTop, -10);
  Status.Anchor(hpRight, -10);
  Status.Color := Yellow; // you could also use "Vector4(1, 1, 0, 1)" instead of Yellow
  MyView.InsertFront(Status);

  { Show 2D image }
  ExampleImage := TCastleImageControl.Create(Application);
  ExampleImage.URL := 'castle-data:/example_image.png';
  // This also works:
  // 'my-packed-data:/example_image.png';
  ExampleImage.Anchor(vpBottom, 100);
  ExampleImage.Anchor(hpLeft, 100);
  MyView.InsertFront(ExampleImage);

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
  {$ifdef USE_ZIP_URL_HANDLER}
  FreeAndNil(PackedDataZip);
  {$else}
  FreeAndNil(PackedDataReader);
  {$endif}
end.
