{
  Copyright 2022-2022 Michalis Kamburelis, Andrzej Kilijański.

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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    SpherePlayer: TCastleTransform;
    SphereDynamic: TCastleTransform;
    ButtonMoveByTranslate: TCastleButton;
    ButtonMoveByVelocity: TCastleButton;
    ButtonMoveByAnimateTranslation: TCastleButton;
    CheckboxContinuousCollisionDetection: TCastleCheckbox;
    CheckboxContinuousCollisionDetectionSphereDynamic: TCastleCheckbox;
    ButtonBodyAnimated: TCastleButton;
    ButtonBodyDynamic: TCastleButton;
  private
    procedure ClickMoveByTranslate(Sender: TObject);
    procedure ClickMoveByVelocity(Sender: TObject);
    procedure ClickMoveByAnimateTranslation(Sender: TObject);

    procedure CheckboxContinuousCollisionDetectionChange(Sender: TObject);
    procedure CheckboxContinuousCollisionDetectionSphereDynamicChange(Sender: TObject);

    procedure ClickBodyDynamic(Sender: TObject);
    procedure ClickBodyAnimated(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils, CastleLog;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  ButtonMoveByTranslate.OnClick := {$ifdef FPC}@{$endif}ClickMoveByTranslate;
  ButtonMoveByVelocity.OnClick := {$ifdef FPC}@{$endif}ClickMoveByVelocity;
  ButtonMoveByAnimateTranslation.OnClick := {$ifdef FPC}@{$endif}ClickMoveByAnimateTranslation;

  CheckboxContinuousCollisionDetection.OnChange := {$ifdef FPC}@{$endif}CheckboxContinuousCollisionDetectionChange;
  CheckboxContinuousCollisionDetectionSphereDynamic.OnChange := {$ifdef FPC}@{$endif}CheckboxContinuousCollisionDetectionSphereDynamicChange;

  ButtonBodyDynamic.OnClick := {$ifdef FPC}@{$endif}ClickBodyDynamic;
  ButtonBodyAnimated.OnClick := {$ifdef FPC}@{$endif}ClickBodyAnimated;

  // initialize UI and state consistently
  ClickBodyAnimated(nil);
  ClickMoveByTranslate(nil);
end;

procedure TStateMain.ClickMoveByTranslate(Sender: TObject);
begin
  ButtonMoveByTranslate.Pressed := true;
  ButtonMoveByAnimateTranslation.Pressed := false;
  ButtonMoveByVelocity.Pressed := false;
end;

procedure TStateMain.ClickMoveByVelocity(Sender: TObject);
begin
  ButtonMoveByVelocity.Pressed := true;
  ButtonMoveByAnimateTranslation.Pressed := false;
  ButtonMoveByTranslate.Pressed := false;
end;

procedure TStateMain.ClickMoveByAnimateTranslation(Sender: TObject);
begin
  ButtonMoveByAnimateTranslation.Pressed := true;
  ButtonMoveByTranslate.Pressed := false;
  ButtonMoveByVelocity.Pressed := false;
end;

procedure TStateMain.CheckboxContinuousCollisionDetectionChange(Sender: TObject);
begin
  if CheckboxContinuousCollisionDetection.Checked then
    SpherePlayer.RigidBody.CollisionDetection := cdContinuous
  else
    SpherePlayer.RigidBody.CollisionDetection := cdDiscrete;
end;

procedure TStateMain.CheckboxContinuousCollisionDetectionSphereDynamicChange(Sender: TObject);
begin
  if CheckboxContinuousCollisionDetectionSphereDynamic.Checked then
    SphereDynamic.RigidBody.CollisionDetection := cdContinuous
  else
    SphereDynamic.RigidBody.CollisionDetection := cdDiscrete;
end;

procedure TStateMain.ClickBodyDynamic(Sender: TObject);
begin
  ButtonBodyAnimated.Pressed := false;
  ButtonBodyDynamic.Pressed := true;

  SpherePlayer.RigidBody.Animated := false;
  SpherePlayer.RigidBody.Dynamic := true;
end;

procedure TStateMain.ClickBodyAnimated(Sender: TObject);
begin
  ButtonBodyAnimated.Pressed := true;
  ButtonBodyDynamic.Pressed := false;

  SpherePlayer.RigidBody.Animated := true;
  SpherePlayer.RigidBody.Dynamic := false;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  procedure MovePlayer(const X, Y: Single);
  const
    PlayerSpeed = 500;
  var
    V: TVector2;
  begin
    V := Vector2(X, Y) * PlayerSpeed * SecondsPassed;
    if ButtonMoveByTranslate.Pressed then
      SpherePlayer.Translate(Vector3(V, 0))
    else
    if ButtonMoveByVelocity.Pressed then
      SpherePlayer.RigidBody.LinearVelocity := SpherePlayer.RigidBody.LinearVelocity + Vector3(V, 0)
    //else
    //if CheckboxMoveByAnimateTranslation.Checked then
      //
  end;

begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if Container.Pressed.Items[keyA] or Container.Pressed.Items[keyArrowLeft] then
    MovePlayer(-1, 0)
  else
  if Container.Pressed.Items[keyD] or Container.Pressed.Items[keyArrowRight] then
    MovePlayer(1, 0)
  else
  if Container.Pressed.Items[keyW] or Container.Pressed.Items[keyArrowUp] then
    MovePlayer(0, 1)
  else
  if Container.Pressed.Items[keyS] or Container.Pressed.Items[keyArrowDown] then
    MovePlayer(0, -1)
  else
    SpherePlayer.RigidBody.LinearVelocity := Vector3(0, 0, 0);
end;

end.
