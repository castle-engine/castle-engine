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

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTerrain,
  CastleCameras, CastleScene, CastleViewport, CastleTransform, CastleSoundEngine;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    WalkNavigation1: TCastleWalkNavigation;
    Viewport1: TCastleViewport;
    SceneDetonate, SceneDropMore: TCastleScene;
    SoundDetonate, SoundClick: TCastleSound;
    Boxes: TCastleTransform;
    SceneExplosionCenter: TCastleTransform;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
const
  ExplosionStrength = 10;
  UpwardExplosion = 2; // makes sure we have some force upward, this just looks more impressive
var
  NewBoxes: TCastleTransform;
  RBody: TCastleRigidBody;
  ExplosionCenter, ForceDir: TVector3;
  AllRigidBodies: TCastleBehaviorList;
  RBodyBehavior: TCastleBehavior;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonLeft) then
  begin
    if Viewport1.TransformUnderMouse = SceneDetonate then
    begin
      SceneDetonate.PlayAnimation('detonate', false);
      SoundEngine.Play(SoundDetonate);

      { for all rigid bodies in Boxes, apply impulse }
      ExplosionCenter := SceneExplosionCenter.WorldTranslation;
      AllRigidBodies := Boxes.FindAllBehaviors(TCastleRigidBody);
      try
        for RBodyBehavior in AllRigidBodies do
        begin
          RBody := RBodyBehavior as TCastleRigidBody;
          ForceDir := RBody.Parent.WorldTranslation - ExplosionCenter;
          ForceDir.Y := Max(ForceDir.Y, UpwardExplosion);
          RBody.ApplyImpulse(
            ForceDir.AdjustToLength(ExplosionStrength),
            ExplosionCenter);
        end;
      finally FreeAndNil(AllRigidBodies) end;

      Exit(true);
    end;

    if Viewport1.TransformUnderMouse = SceneDropMore then
    begin
      SceneDropMore.PlayAnimation('click', false);
      SoundEngine.Play(SoundClick);

      { instantiate boxes.castle-transform }
      NewBoxes := TransformLoad('castle-data:/boxes.castle-transform', FreeAtStop);
      Boxes.Add(NewBoxes);

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
