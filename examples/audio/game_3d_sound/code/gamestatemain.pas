{
  Copyright 2003-2021 Michalis Kamburelis.

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

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleTransform, CastleSoundEngine, CastleScene, CastleCameras,
  CastleViewport, CastleVectors;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    type
      TTnt = class(TCastleTransform)
      private
        ToRemove: boolean;
      public
        State: TStateMain;
        function PointingDevicePress(const Pick: TRayCollisionNode;
          const Distance: Single): Boolean; override;
        procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
      end;

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
      SceneLevel: TCastleScene;

      { Other }
      RatAngle: Single;
      TntScene: TCastleScene;
      TntsCount: Integer;

    procedure NewTnt(const Y: Single);
    { Update Rat.Translation based on RatAngle }
    procedure UpdateRatPosition;
    procedure DoTimerSpawnTnts(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleUtils, CastleBoxes, CastleWindow;

{ TStateMain.TTnt ------------------------------------------------------------------------ }

{ TODO: TNT should use physics, and use .castle-transform }

function TStateMain.TTnt.PointingDevicePress(const Pick: TRayCollisionNode;
  const Distance: Single): Boolean;
begin
  inherited;

  if ToRemove then
    Exit(false);

  // TODO: make it 3D at tnt, or at rat:
  SoundEngine.Play(State.SoundKaboom);
  if PointsDistanceSqr(Translation, State.Rat.Translation) < 1.0 then
    SoundEngine.Play(State.SoundRatSqueak);

  Result := true;
  ToRemove := true;
  Dec(State.TntsCount);
end;

procedure TStateMain.TTnt.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  T: TVector3;
begin
  inherited;

  { make gravity }
  T := Translation;
  if T.Y > 0 then
  begin
    T.Y := T.Y - 5 * SecondsPassed;
    if T.Y < 0 then
      T.Y := 0;
    Translation := T;
  end;

  if ToRemove then
    RemoveMe := rtRemoveAndFree;
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
  SceneLevel := DesignedComponent('SceneLevel') as TCastleScene;

  { initialize Rat }
  UpdateRatPosition;

  { initialize Tnt }
  TntScene := TCastleScene.Create(FreeAtStop);
  TntScene.Load('castle-data:/extra_objects/tnt.gltf');
  while TntsCount < InitialTntsCount do
    NewTnt(0.0);

  TimerSpawnTnts.OnTimer := @DoTimerSpawnTnts;
end;

procedure TStateMain.NewTnt(const Y: Single);
var
  TntSize: Single;
  Tnt: TTnt;
  Box: TBox3D;
begin
  TntSize := TntScene.BoundingBox.MaxSize;
  Tnt := TTnt.Create(FreeAtStop);
  Tnt.State := Self;
  Tnt.Add(TntScene);
  Box := SceneLevel.BoundingBox;
  Tnt.Translation := Vector3(
    RandomFloatRange(Box.Data[0].X, Box.Data[1].X - TntSize),
    Y,
    RandomFloatRange(Box.Data[0].Z, Box.Data[1].Z - TntSize));
  Viewport.Items.Add(Tnt);
  Inc(TntsCount);
end;

procedure TStateMain.UpdateRatPosition;
const
  RatCircleMiddle: TVector3 = (Data: (0, 0, 0));
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
  while TntsCount < MaxTntsCount do NewTnt(3.0);
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
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
end;

end.
