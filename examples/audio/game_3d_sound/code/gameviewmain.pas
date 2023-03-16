{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes, Contnrs,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleTransform, CastleSoundEngine, CastleScene, CastleCameras,
  CastleViewport, CastleVectors, CastleBehaviors;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    SoundKaboom, SoundRatSqueak: TCastleSound;
    HelpMessage: TCastleLabel;
    MuteImage: TCastleImageControl;
    CrosshairForMouseLook: TCastleCrosshair;
    TimerSpawnTnts: TCastleTimer;
    Viewport: TCastleViewport;
    Navigation: TCastleWalkNavigation;
    Rat: TCastleScene;
    SoundSourceRat: TCastleSoundSource;
    SceneLevel: TCastleScene;
  private
    const
      { Max number of TNT items. }
      MaxTntsCount = 40;
      InitialTntsCount = MaxTntsCount;

    var
      RatAngle: Single;
      TntTemplate: TSerializedComponent;
      Tnts: TComponentList;

    procedure NewTnt(const Y: Single);
    { Update Rat.Translation based on RatAngle }
    procedure UpdateRatPosition;
    procedure DoTimerSpawnTnts(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils, CastleBoxes, CastleWindow, CastleSceneCore;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { initialize Rat }
  UpdateRatPosition;

  { initialize Tnt }
  Tnts := TComponentList.Create(false);
  TntTemplate := TSerializedComponent.Create('castle-data:/extra_objects/tnt_final.castle-transform');
  while Tnts.Count < InitialTntsCount do
    NewTnt(0.0);

  TimerSpawnTnts.OnTimer := {$ifdef FPC}@{$endif}DoTimerSpawnTnts;
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(TntTemplate);
  FreeAndNil(Tnts);
  inherited;
end;

procedure TViewMain.NewTnt(const Y: Single);
var
  TntExtent: Single;
  Tnt: TCastleTransform;
  LevelBox: TBox3D;
begin
  Tnt := TntTemplate.TransformLoad(FreeAtStop);
  TntExtent := Tnt.BoundingBox.MaxSize / 2;
  LevelBox := SceneLevel.BoundingBox;
  Tnt.Translation := Vector3(
    RandomFloatRange(LevelBox.Data[0].X + TntExtent, LevelBox.Data[1].X - TntExtent),
    Y + TntExtent,
    RandomFloatRange(LevelBox.Data[0].Z + TntExtent, LevelBox.Data[1].Z - TntExtent));
  Viewport.Items.Add(Tnt);
  Tnts.Add(Tnt);
end;

procedure TViewMain.UpdateRatPosition;
const
  RatCircleMiddle: TVector3 = (X: 0; Y: 0; Z: 0);
  RatCircleRadius = 3;
var
  T: TVector3;
begin
  T := RatCircleMiddle;
  T.X := T.X + (Cos(RatAngle) * RatCircleRadius);
  T.Z := T.Z + (Sin(RatAngle) * RatCircleRadius);
  Rat.Translation := T;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  function CylinderContains(const P: TVector3;
    const MiddleX, MiddleZ, Radius, MinY, MaxY: Single): boolean;
  begin
    Result :=
      (Sqr(P.X-MiddleX) + Sqr(P.Z-MiddleZ) <= Sqr(Radius)) and
      (MinY <= P.Y) and (P.Y <= MaxY);
  end;

var
  InMuteArea: boolean;
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { update rat }
  RatAngle := RatAngle + 0.5 * SecondsPassed;
  UpdateRatPosition;

  { update "mute area" }
  InMuteArea := CylinderContains(Viewport.Camera.WorldTranslation, 2, 0, 0.76, 0, 1.045640);
  if MuteImage <> nil then
    MuteImage.Exists := InMuteArea;
  if InMuteArea then
    SoundEngine.Volume := 0
  else
    SoundEngine.Volume := 1;
end;

procedure TViewMain.DoTimerSpawnTnts(Sender: TObject);
begin
  while Tnts.Count < MaxTntsCount do
    NewTnt(3.0);
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;

  procedure TntHit(TntTransform: TCastleTransform);
  var
    TntSoundSource: TCastleSoundSource;
    SoundKaboomPlaying: TCastlePlayingSoundSource;
  begin
    TntSoundSource := TntTransform.FindBehavior(TCastleSoundSource) as TCastleSoundSource;

    { Create TCastlePlayingSoundSource instance, because we want to adjust Follow
      from default true to false.
      This way sound will continue playing regardless if the TCastleSoundSource
      will be destroyed. }
    SoundKaboomPlaying := TCastlePlayingSoundSource.Create(FreeAtStop);
    SoundKaboomPlaying.FreeOnStop := true;
    SoundKaboomPlaying.Follow := false;
    SoundKaboomPlaying.Sound := SoundKaboom;
    TntSoundSource.Play(SoundKaboomPlaying);

    if PointsDistanceSqr(TntTransform.Translation, Rat.Translation) < 1.0 then
      SoundSourceRat.Play(SoundRatSqueak);

    FreeAndNil(TntTransform); // will automatically remove itself from Tnts list
  end;

begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonLeft) then
  begin
    { Detect clicks on TNT scene.
      We look at TransformUnderMouse.Parent, because that is how tnt_final.castle-transform
      was designed: we will detect clicks on SceneTntBox, which is the child of the root
      of tnt_final.castle-transform . }
    if (Viewport.TransformUnderMouse <> nil) and
       (Viewport.TransformUnderMouse.Parent <> nil) and
       (Tnts.IndexOf(Viewport.TransformUnderMouse.Parent) <> -1) then
      TntHit(Viewport.TransformUnderMouse.Parent);
    Exit(true);
  end;

  if Event.IsMouseButton(buttonRight) then
  begin
    Navigation.MouseLook := not Navigation.MouseLook;
    // crosshair makes sense only with mouse look
    CrosshairForMouseLook.Exists := Navigation.MouseLook;
    Exit(true);
  end;

  if Event.IsKey(keyF1) then
  begin
    HelpMessage.Exists := not HelpMessage.Exists;
    Exit(true);
  end;

  if Event.IsKey(keyF5) then
  begin
    Container.SaveScreenToDefaultFile;
    Exit(true);
  end;

  if Event.IsKey(keyEscape) then
  begin
    Application.Terminate;
    Exit(true);
  end;
end;

end.
