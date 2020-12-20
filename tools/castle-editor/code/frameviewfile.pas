{
  Copyright 2018-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Quick file viewer for various file types supported by CGE. }
unit FrameViewFile;

interface

uses
  Classes, SysUtils, Forms, Controls,
  CastleControl, CastleControls, CastleViewport, CastleScene,
  CastleUIControls, CastleSoundEngine;

type
  TViewFileFrame = class(TFrame)
    Control: TCastleControlBase;
  private
    FURL, FSuccessMessage, FErrorMessage: String;
    LabelURL, LabelInformation: TCastleLabel;
    PreviewLayer: TCastleUserInterface;
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    Image: TCastleImageControl;
    SoundSource: TSound;
    SoundBuffer: TSoundBuffer;
    SoundButton: TCastleButton;
    procedure ClickSoundButton(Sender: TObject);
    procedure SoundSourceRelease(Sound: TSound);
    procedure FinishLoading(const AURL: String);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Load preview of various file types. }
    procedure LoadScene(const AURL: String);
    procedure LoadImage(const AURL: String);
    procedure LoadSound(const AURL: String);
    procedure ClearLoaded;
  end;

implementation

uses CastleColors, CastleUtils, CastleSoundBase, CastleVectors, CastleCameras,
  CastleURIUtils;

{$R *.lfm}

{ TViewFileFrame ----------------------------------------------------------- }

procedure TViewFileFrame.ClearLoaded;
var
  OldInternalCastleDesignInvalidate: Boolean;
begin
  OldInternalCastleDesignInvalidate := InternalCastleDesignInvalidate;

  FURL := '';
  FSuccessMessage := '';
  FErrorMessage := '';

  FreeAndNil(Viewport);
  FreeAndNil(Scene);
  FreeAndNil(Image);
  FreeAndNil(SoundButton);
  if SoundSource <> nil then
  begin
    SoundSource.OnRelease := nil;
    SoundSource.Release;
    SoundSource := nil;
  end;
  if SoundBuffer <> nil then
  begin
    SoundEngine.FreeBuffer(SoundBuffer);
    SoundBuffer := nil;
  end;

  { Save and restore InternalCastleDesignInvalidate here,
    for the same reason as in LoadXxx.
    Testcase: without this save+restore,
    selecting in CGE editor a non-scene from scene in "Files"
    would deselect your current selection in "Hierarchy". }
  InternalCastleDesignInvalidate := OldInternalCastleDesignInvalidate;
end;

procedure TViewFileFrame.ClickSoundButton(Sender: TObject);
begin
  if SoundBuffer = nil then Exit;

  if SoundSource <> nil then
  begin
    SoundSource.Release; // will call SoundSourceRelease which will nil SoundSource
    SoundButton.Caption := 'PLAY';
  end else
  begin
    SoundSource := SoundEngine.PlaySound(SoundBuffer);
    if SoundSource <> nil then
    begin
      SoundSource.OnRelease := @SoundSourceRelease;
      SoundButton.Caption := 'STOP';
    end;
  end;
end;

procedure TViewFileFrame.SoundSourceRelease(Sound: TSound);
begin
  Sound.OnRelease := nil;
  if SoundButton <> nil then
    SoundButton.Caption := 'PLAY';
  SoundSource := nil;
end;

procedure TViewFileFrame.FinishLoading(const AURL: String);
begin
  FURL := AURL;
  LabelURL.Caption := URIDisplay(AURL, true);
  if FErrorMessage <> '' then
  begin
    LabelInformation.Caption := FErrorMessage;
    LabelInformation.Color := HexToColor('ff7e50'); // Red
  end else
  begin
    LabelInformation.Caption := FSuccessMessage;
    LabelInformation.Color := HexToColor('58ff43'); // Green
  end;
end;

constructor TViewFileFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TViewFileFrame.Destroy;
begin
  ClearLoaded; // makes sure to stop the sound
  inherited;
end;

procedure TViewFileFrame.Loaded;
begin
  inherited;

  PreviewLayer := TCastleUserInterface.Create(Self);
  PreviewLayer.FullSize := true;
  Control.Controls.InsertFront(PreviewLayer);

  LabelURL := TCastleLabel.Create(Self);
  LabelURL.Color := White;
  LabelURL.Anchor(hpLeft, 5);
  LabelURL.Anchor(vpTop, -5);
  Control.Controls.InsertFront(LabelURL);

  LabelInformation := TCastleLabel.Create(Self);
  LabelInformation.Color := White;
  LabelInformation.Anchor(hpLeft, 5);
  LabelInformation.Anchor(vpTop, - (5 + 5 + LabelURL.Font.RowHeight));
  Control.Controls.InsertFront(LabelInformation);
end;

procedure TViewFileFrame.LoadScene(const AURL: String);
var
  Pos, Dir, Up, GravityUp: TVector3;
  OldInternalCastleDesignInvalidate: Boolean;
begin
  OldInternalCastleDesignInvalidate := InternalCastleDesignInvalidate;
  ClearLoaded;

  Viewport := TCastleViewport.Create(Self);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.AutoNavigation := true;
  PreviewLayer.InsertFront(Viewport);

  Scene := TCastleScene.Create(Self);
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  try
    Scene.URL := AURL;
    FSuccessMessage := Format(
      'Vertexes: %d' + NL +
      'Triangles: %d' + NL +
      'Bounding Box: %s', [
      Scene.VerticesCount(true),
      Scene.TrianglesCount(true),
      Scene.BoundingBox.ToString
    ]);

    CameraViewpointForWholeScene(Viewport.Items.BoundingBox,
      2, 1, false, true, Pos, Dir, Up, GravityUp);
    Viewport.NavigationType := ntExamine;
    Viewport.Camera.SetView(Pos, Dir, Up, GravityUp);
  except
    on E: Exception do
    begin
      FErrorMessage := ExceptMessage(E);
      // continue the rest, to show URL and error message
    end;
  end;

  FinishLoading(AURL);
  { Save and restore InternalCastleDesignInvalidate in all LoadXxx methods,
    because changes done while initializing the preview frame
    (even creating UI / transform components) don't really matter for the editor's
    displayed hierarchy.
    Testcase: without this save+restore, selecting in CGE editor a scene in "Files"
    would deselect your current selection in "Hierarchy". }
  InternalCastleDesignInvalidate := OldInternalCastleDesignInvalidate;
end;

procedure TViewFileFrame.LoadImage(const AURL: String);
var
  OldInternalCastleDesignInvalidate: Boolean;
begin
  OldInternalCastleDesignInvalidate := InternalCastleDesignInvalidate;
  ClearLoaded;

  Image := TCastleImageControl.Create(Self);
  Image.WidthFraction := 1;
  Image.HeightFraction := 1;
  Image.Stretch := true;
  Image.ProportionalScaling := psFit;
  Image.Anchor(hpMiddle);
  Image.Anchor(vpMiddle);
  PreviewLayer.InsertFront(Image);

  try
    Image.URL := AURL;
    FSuccessMessage := Format(
      'Width: %d' + NL +
      'Height: %d' + NL +
      'Depth: %d', [
      Image.Content.Image.Width,
      Image.Content.Image.Height,
      Image.Content.Image.Depth
    ]);
  except
    on E: Exception do
    begin
      FErrorMessage := ExceptMessage(E);
      // continue the rest, to show URL and error message
    end;
  end;

  FinishLoading(AURL);
  InternalCastleDesignInvalidate := OldInternalCastleDesignInvalidate;
end;

procedure TViewFileFrame.LoadSound(const AURL: String);
var
  OldInternalCastleDesignInvalidate: Boolean;
begin
  OldInternalCastleDesignInvalidate := InternalCastleDesignInvalidate;
  ClearLoaded;

  SoundButton := TCastleButton.Create(Self);
  SoundButton.Caption := 'PLAY';
  SoundButton.OnClick := @ClickSoundButton;
  SoundButton.Anchor(hpMiddle);
  SoundButton.Anchor(vpMiddle);
  SoundButton.Enabled := false;
  SoundButton.MinWidth := 100;
  SoundButton.MinHeight := 100;
  PreviewLayer.InsertFront(SoundButton);

  try
    SoundBuffer := SoundEngine.LoadBuffer(AURL);

    // without this check, LoadBuffer fails silently e.g. when OpenAL dll not found
    if not SoundEngine.IsContextOpenSuccess then
      raise Exception.Create('Sound engine backend cannot be initialized.' + NL +
        SoundEngine.Information);

    FSuccessMessage := Format(
      'Duration: %f' + NL +
      'Format: %s' + NL +
      'Frequency: %d', [
      SoundBuffer.Duration,
      DataFormatToStr(SoundBuffer.DataFormat),
      SoundBuffer.Frequency
    ]);
    SoundButton.Enabled := true;
  except
    on E: Exception do
    begin
      FErrorMessage := ExceptMessage(E);
      // continue the rest, to show URL and error message
    end;
  end;

  FinishLoading(AURL);
  InternalCastleDesignInvalidate := OldInternalCastleDesignInvalidate;
end;

end.
