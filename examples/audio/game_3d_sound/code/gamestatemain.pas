{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes, Contnrs,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleTransform, CastleSoundEngine, CastleScene, CastleCameras,
  CastleViewport, CastleVectors, CastleBehaviors;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    const
      { Max number of TNT items. }
      MaxTntsCount = 40;
      InitialTntsCount = MaxTntsCount;

    var
      { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
      LabelFps: TCastleLabel;
      SoundKaboom, SoundRatSqueak: TCastleSound;
      HelpMessage: TCastleLabel;
      MuteImage: TCastleImageControl;
      CrosshairForMouseLook: TCastleCrosshair;
      TimerSpawnTnts: TCastleTimer;
      Viewport: TCastleViewport;
      Navigation: TCastleWalkNavigation;
      Rat: TCastleScene;
      RatSoundSource: TCastleSoundSource;
      SceneLevel: TCastleScene;

      { Other }
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
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleUtils, CastleBoxes, CastleWindow, CastleSceneCore;

{ utils ---------------------------------------------------------------------- }

{ Setup TRigidBody and TMeshCollider on the Scene.
  TODO: This will be possible to be created in CGE editor soon. }
procedure SetupPhysicsStaticMesh(const Scene: TCastleScene);
var
  RigidBody: TRigidBody;
  Collider: TMeshCollider;
begin
  RigidBody := TRigidBody.Create(Scene);
  RigidBody.Dynamic := false;

  Collider := TMeshCollider.Create(RigidBody);
  Collider.Scene := Scene;
  Collider.Restitution := 0.3;

  Scene.RigidBody := RigidBody;
end;

{ Setup TRigidBody and TMeshCollider on the Scene.
  TODO: This will be possible to be created in CGE editor soon. }
procedure SetupPhysicsStaticPlane(const Scene: TCastleScene);
var
  RigidBody: TRigidBody;
  Collider: TPlaneCollider;
begin
  RigidBody := TRigidBody.Create(Scene);
  RigidBody.Dynamic := false;

  Collider := TPlaneCollider.Create(RigidBody);
  Collider.Normal := Vector3(0, 1, 0);
  Collider.Distance := 0;
  Collider.Restitution := 0.3;

  Scene.RigidBody := RigidBody;
end;

{ Setup TRigidBody and TBoxCollider on the Scene.
  TODO: This will be possible to be created in CGE editor soon. }
procedure SetupPhysicsDynamicBox(const Transform: TCastleTransform);
var
  RigidBody: TRigidBody;
  Collider: TBoxCollider;
begin
  RigidBody := TRigidBody.Create(Transform);

  Collider := TBoxCollider.Create(RigidBody);
  Collider.Size := Transform.BoundingBox.Size * 0.9;

  Transform.RigidBody := RigidBody;
end;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  SoundKaboom := DesignedComponent('SoundKaboom') as TCastleSound;
  SoundRatSqueak := DesignedComponent('SoundRatSqueak') as TCastleSound;
  MuteImage := DesignedComponent('MuteImage') as TCastleImageControl;
  CrosshairForMouseLook := DesignedComponent('CrosshairForMouseLook') as TCastleCrosshair;
  HelpMessage := DesignedComponent('HelpMessage') as TCastleLabel;
  TimerSpawnTnts := DesignedComponent('TimerSpawnTnts') as TCastleTimer;
  Viewport := DesignedComponent('Viewport') as TCastleViewport;
  Navigation := DesignedComponent('Navigation') as TCastleWalkNavigation;
  Rat := DesignedComponent('Rat') as TCastleScene;
  RatSoundSource := Rat.FindBehavior(TCastleSoundSource) as TCastleSoundSource;
  SceneLevel := DesignedComponent('SceneLevel') as TCastleScene;

  { initialize Rat }
  UpdateRatPosition;

  { initialize Tnt }
  Tnts := TComponentList.Create(false);
  TntTemplate := TSerializedComponent.Create('castle-data:/extra_objects/tnt_final.castle-transform');
  while Tnts.Count < InitialTntsCount do
    NewTnt(0.0);

  TimerSpawnTnts.OnTimer := {$ifdef FPC}@{$endif}DoTimerSpawnTnts;

  SetupPhysicsStaticPlane(SceneLevel);
  //SetupPhysicsStaticMesh(SceneLevel); // mesh collider not reliable on this
end;

procedure TStateMain.Stop;
begin
  FreeAndNil(TntTemplate);
  FreeAndNil(Tnts);
  inherited;
end;

procedure TStateMain.NewTnt(const Y: Single);
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
  SetupPhysicsDynamicBox(Tnt);
  Viewport.Items.Add(Tnt);
  Tnts.Add(Tnt);
end;

procedure TStateMain.UpdateRatPosition;
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

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

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
  InMuteArea := CylinderContains(Viewport.Camera.Position, 2, 0, 0.76, 0, 1.045640);
  if MuteImage <> nil then
    MuteImage.Exists := InMuteArea;
  if InMuteArea then
    SoundEngine.Volume := 0
  else
    SoundEngine.Volume := 1;
end;

procedure TStateMain.DoTimerSpawnTnts(Sender: TObject);
begin
  while Tnts.Count < MaxTntsCount do
    NewTnt(3.0);
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;

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
      RatSoundSource.Play(SoundRatSqueak);

    FreeAndNil(TntTransform); // will automatically remove itself from Tnts list
  end;

begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.EventType = itKey then
    case Event.Key of
      keyF1: HelpMessage.Exists := not HelpMessage.Exists;
      keyF4:
        begin
          Navigation.MouseLook := not Navigation.MouseLook;
          // crosshair makes sense only with mouse look
          CrosshairForMouseLook.Exists := Navigation.MouseLook;
        end;
      keyF5: Container.SaveScreenToDefaultFile;
      keyEscape: Application.Terminate;
    end;

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
  end;
end;

end.
