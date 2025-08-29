{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the files COPYING*,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleScene, CastleTimeUtils,
  CastleUIControls, CastleControls, CastleKeysMouse,
  GameUnsplash;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonNewImage: TCastleButton;
    ImageUiFromUnsplash: TCastleImageControl;
    BoxTextureFromUnsplash: TCastleBox;
    LabelUnsplashDownloadStatus: TCastleLabel;
  private
    LifeTime: TFloatTime;
    UnsplashDownload: TUnsplashDownload;
    procedure ClickNewImage(Sender: TObject);
    procedure UnsplashDownloadFinish(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleDownload, CastleClassUtils, CastleLog;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  UnsplashDownload := TUnsplashDownload.Create;
  UnsplashDownload.OnFinish := {$ifdef FPC}@{$endif} UnsplashDownloadFinish;

  ButtonNewImage.OnClick := {$ifdef FPC}@{$endif} ClickNewImage;
  ClickNewImage(nil);
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(UnsplashDownload);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
const
  UnsplashDownloadStatusToStr: array [TUnsplashDownloadStatus] of String = (
    'Not started',
    'Working: Random image query',
    'Working: Downloading image contents',
    'Error',
    'Success'
  );
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  LabelUnsplashDownloadStatus.Caption := 'Unsplash Download Status: ' +
    UnsplashDownloadStatusToStr[UnsplashDownload.Status];

  LifeTime := LifeTime + SecondsPassed;
  // rotate BoxTextureFromUnsplash, just to keep things interesting looking
  BoxTextureFromUnsplash.Rotation := Vector4(0, 1, 0, LifeTime * 2);
end;

procedure TViewMain.ClickNewImage(Sender: TObject);
begin
  UnsplashDownload.SearchQuery := 'cute fox';
  UnsplashDownload.Collections := ''; //'8343314', // https://unsplash.com/collections/8343314/adorable-animals
  UnsplashDownload.Start;

  // TODO: test WaitForFinish
end;

procedure TViewMain.UnsplashDownloadFinish(Sender: TObject);

  procedure SaveStreamToFile(const Stream: TStream; const FileName: String);
  var
    S: TStream;
  begin
    S := UrlSaveStream(FileName);
    try
      ReadGrowingStream(Stream, S, true);
    finally FreeAndNil(S) end;
  end;

var
  ImageUrl: String;
begin
  if UnsplashDownload.Status = usSuccess then
  begin
    ImageUrl := 'castle-config:/' + UnsplashDownload.UnsplashImageId + '.jpg';
    SaveStreamToFile(UnsplashDownload.ImageStream, ImageUrl);
    ImageUiFromUnsplash.Url := ImageUrl;
    BoxTextureFromUnsplash.Texture := ImageUrl;
  end else
    WritelnWarning('Failed to download image from Unsplash');
end;

end.
