{
  Copyright 2022-2022 Michalis Kamburelis.

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
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTerrain,
  CastleCameras, CastleScene, CastleViewport, CastleTransform, CastleSoundEngine;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    WalkNavigation1: TCastleWalkNavigation;
    Viewport1: TCastleViewport;
    SceneDetonate: TCastleScene;
    SoundDetonate: TCastleSound;
    Boxes: TCastleTransform;
    SceneExplosionCenter: TCastleTransform;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils, Math;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
const
  ExplosionStrength = 10;
  UpwardExplosion = 2; // makes sure we have some force upward, this just looks more impressive
var
  Box: TCastleTransform;
  RBody: TCastleRigidBody;
  ExplosionCenter, ForceDir: TVector3;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonLeft) then
  begin
    if Viewport1.TransformUnderMouse = SceneDetonate then
    begin
      SceneDetonate.PlayAnimation('detonate', false);
      SoundEngine.Play(SoundDetonate);
      ExplosionCenter := SceneExplosionCenter.WorldTranslation;
      for Box in Boxes do
      begin
        RBody := Box.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
        if RBody <> nil then
        begin
          ForceDir := Box.WorldTranslation - ExplosionCenter;
          ForceDir.Y := Max(ForceDir.Y, UpwardExplosion);
          RBody.ApplyImpulse(
            ForceDir.AdjustToLength(ExplosionStrength),
            ExplosionCenter);
        end;
      end;

      Exit(true);
    end;
  end;

  if Event.IsMouseButton(buttonRight) then
  begin
    WalkNavigation1.MouseLook := not WalkNavigation1.MouseLook;
    Exit(true);
  end;
end;

end.
