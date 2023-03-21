{
  Copyright 2018-2023 Michalis Kamburelis.

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

{$if defined(LCLGTK3) or defined(LCLQt) or defined(LCLQt5)}
  {$error Do not use LCL Qt or GTK3 widgetsets to build CGE editor, as LCL TOpenGLControl doesn't support OpenGL context sharing in this case. Use LCL default widgetset instead, like GTK2.}
  // Didn't fit in message above:
  // For your own applications, if you only use one TCastleControl in the application, it's not a problem.
{$endif}

interface

uses
  Classes, SysUtils, Forms, Controls,
  CastleControl, CastleControls, CastleViewport, CastleScene,
  CastleUIControls, CastleSoundEngine;

type
  TViewFileFrame = class(TFrame)
    Control: TCastleControl;
  private
    FURL, FSuccessMessage, FErrorMessage: String;
    LabelURL, LabelInformation: TCastleLabel;
    PreviewLayer: TCastleUserInterface;
    {$warnings off} // using TCastleAutoNavigationViewport that should be internal
    Viewport: TCastleAutoNavigationViewport;
    {$warnings on}
    Scene: TCastleScene;
    Image: TCastleImageControl;
    Sound: TCastleSound;
    PlayingSound: TCastlePlayingSound;
    SoundButton: TCastleButton;
    LabelVolume: TCastleLabel;
    procedure ClickSoundButton(Sender: TObject);
    procedure SoundStop(Sender: TObject);
    procedure FinishLoading(const AURL: String);
    procedure UpdateLabelVolume(const Sender: TCastleUserInterface;
      const SecondsPassed: Single; var HandleInput: Boolean);
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
  CastleTransform, CastleURIUtils,
  EditorUtils;

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
  FreeAndNil(LabelVolume);
  FreeAndNil(PlayingSound);
  FreeAndNil(Sound);

  { Save and restore InternalCastleDesignInvalidate here,
    for the same reason as in LoadXxx.
    Testcase: without this save+restore,
    selecting in CGE editor a non-scene from scene in "Files"
    would deselect your current selection in "Hierarchy". }
  InternalCastleDesignInvalidate := OldInternalCastleDesignInvalidate;
end;

procedure TViewFileFrame.ClickSoundButton(Sender: TObject);
begin
  if PlayingSound <> nil then
  begin
    PlayingSound.Stop;// will call SoundStop which will set PlayingSound := nil
    Assert(PlayingSound = nil);
    SoundButton.Caption := 'PLAY';
  end else
  begin
    SoundButton.Caption := 'STOP';
    PlayingSound := TCastlePlayingSound.Create(Self);
    PlayingSound.Sound := Sound;
    PlayingSound.FreeOnStop := true;
    PlayingSound.OnStop := @SoundStop;
    { Note: In special cases, if we don't have enough audio sources, the Play immediately
      stops the sound (calling OnStop (changing SoundButton.Caption) and applying FreeOnStop). }
    SoundEngine.Play(PlayingSound);
  end;
end;

procedure TViewFileFrame.SoundStop(Sender: TObject);
begin
  if SoundButton <> nil then
    SoundButton.Caption := 'PLAY';
  PlayingSound := nil;
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
  LabelInformation.Anchor(vpTop, - (5 + 5 + LabelURL.Font.Height));
  Control.Controls.InsertFront(LabelInformation);
end;

procedure TViewFileFrame.LoadScene(const AURL: String);
var
  Pos, Dir, Up, GravityUp: TVector3;
  OldInternalCastleDesignInvalidate: Boolean;
begin
  OldInternalCastleDesignInvalidate := InternalCastleDesignInvalidate;
  ClearLoaded;

  {$warnings off} // using TCastleAutoNavigationViewport that should be internal
  Viewport := TCastleAutoNavigationViewport.InternalCreateNonDesign(Self);
  {$warnings on}
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
      Scene.VerticesCount,
      Scene.TrianglesCount,
      Scene.BoundingBox.ToString
    ]);

    CameraViewpointForWholeScene(Viewport.Items.BoundingBox,
      2, 1, false, true, Pos, Dir, Up, GravityUp);
    Viewport.NavigationType := ntExamine;
    Viewport.Camera.SetWorldView(Pos, Dir, Up);
    Viewport.Camera.GravityUp := GravityUp;
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

  LabelVolume := TCastleLabel.Create(Self);
  LabelVolume.FontSize := 20;
  LabelVolume.Color := Gray;
  LabelVolume.Anchor(hpMiddle);
  LabelVolume.Anchor(vpTop, vpBottom, -5);
  LabelVolume.OnUpdate := @UpdateLabelVolume;
  SoundButton.InsertFront(LabelVolume);

  Sound := TCastleSound.Create(Self);
  try
    Sound.URL := AURL;

    // without this check, loading fails silently e.g. when OpenAL dll not found
    if not SoundEngine.IsContextOpenSuccess then
      raise Exception.Create('Sound engine backend cannot be initialized.' + NL +
        SoundEngine.Information);

    FSuccessMessage := FormatDot(
      'Duration: %f' + NL +
      'Format: %s' + NL +
      'Frequency: %d', [
      Sound.Duration,
      DataFormatToStr(Sound.DataFormat),
      Sound.Frequency
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

procedure TViewFileFrame.UpdateLabelVolume(const Sender: TCastleUserInterface;
  const SecondsPassed: Single; var HandleInput: Boolean);
var
  S: String;
begin
  if SoundEngine.Volume = 0 then
  begin
    S := 'Volume: mute';
    if RunningApplication and MuteOnRun then
      S := S + ' (application running)';
  end else
    S := FormatDot('Volume: %f', [SoundEngine.Volume]);
  LabelVolume.Caption := S;
end;

end.
