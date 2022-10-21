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
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    SpherePlayer: TCastleTransform;
    SphereDynamic: TCastleTransform;
    CheckboxMoveByTranslate: TCastleCheckbox;
    CheckboxMoveByVelocity: TCastleCheckbox;
    CheckboxMoveByAnimateTranslation: TCastleCheckbox;
    CheckboxContinuousCollisionDetection: TCastleCheckbox;
    CheckboxContinuousCollisionDetectionSphereDynamic: TCastleCheckbox;
    ButtonChangeToAnimated: TCastleButton;
    ButtonChangeToDynamic: TCastleButton;

    procedure CheckboxMoveByTranslateChange(Sender: TObject);
    procedure CheckboxMoveByVelocityChange(Sender: TObject);
    procedure CheckboxMoveByAnimateTranslationChange(Sender: TObject);

    procedure CheckboxContinuousCollisionDetectionChange(Sender: TObject);
    procedure CheckboxContinuousCollisionDetectionSphereDynamicChange(Sender: TObject);

    procedure MakeDynamic(Sender: TObject);
    procedure MakeAnimated(Sender: TObject);

    function InputLeft: Boolean;
    function InputRight: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils, CastleLog;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.CheckboxMoveByTranslateChange(Sender: TObject);
begin
  CheckboxMoveByTranslate.Checked := true;
  CheckboxMoveByAnimateTranslation.Checked := false;
  CheckboxMoveByVelocity.Checked := false;
end;

procedure TStateMain.CheckboxMoveByVelocityChange(Sender: TObject);
begin
  CheckboxMoveByVelocity.Checked := true;
  CheckboxMoveByAnimateTranslation.Checked := false;
  CheckboxMoveByTranslate.Checked := false;
end;

procedure TStateMain.CheckboxMoveByAnimateTranslationChange(Sender: TObject);
begin
  CheckboxMoveByAnimateTranslation.Checked := true;
  CheckboxMoveByTranslate.Checked := false;
  CheckboxMoveByVelocity.Checked := false;
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

procedure TStateMain.MakeDynamic(Sender: TObject);
begin
  SpherePlayer.RigidBody.Dynamic := true;
  SpherePlayer.RigidBody.Animated := false;
end;

procedure TStateMain.MakeAnimated(Sender: TObject);
begin
  SpherePlayer.RigidBody.Animated := true;
  SpherePlayer.RigidBody.Dynamic := false;
end;

function TStateMain.InputLeft: Boolean;
begin
  Result :=
    Container.Pressed.Items[keyA] or
    Container.Pressed.Items[keyArrowLeft];
end;

function TStateMain.InputRight: Boolean;
begin
  Result :=
    Container.Pressed.Items[keyD] or
    Container.Pressed.Items[keyArrowRight];
end;

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

  SpherePlayer := DesignedComponent('SpherePlayer') as TCastleTransform;
  SphereDynamic := DesignedComponent('SphereDynamic') as TCastleTransform;

  CheckboxMoveByTranslate := DesignedComponent('CheckboxMoveByTranslate') as TCastleCheckbox;
  CheckboxMoveByTranslate.OnChange := {$ifdef FPC}@{$endif}CheckboxMoveByTranslateChange;

  CheckboxMoveByVelocity := DesignedComponent('CheckboxMoveByVelocity') as TCastleCheckbox;
  CheckboxMoveByVelocity.OnChange := {$ifdef FPC}@{$endif}CheckboxMoveByVelocityChange;

  CheckboxMoveByAnimateTranslation := DesignedComponent('CheckboxMoveByAnimateTranslation') as TCastleCheckbox;
  CheckboxMoveByAnimateTranslation.OnChange := {$ifdef FPC}@{$endif}CheckboxMoveByAnimateTranslationChange;

  CheckboxContinuousCollisionDetection := DesignedComponent('CheckboxContinuousCollisionDetection') as TCastleCheckbox;
  CheckboxContinuousCollisionDetection.OnChange := {$ifdef FPC}@{$endif}CheckboxContinuousCollisionDetectionChange;

  CheckboxContinuousCollisionDetectionSphereDynamic := DesignedComponent('CheckboxContinuousCollisionDetectionSphereDynamic') as TCastleCheckbox;
  CheckboxContinuousCollisionDetectionSphereDynamic.OnChange := {$ifdef FPC}@{$endif}CheckboxContinuousCollisionDetectionSphereDynamicChange;

  ButtonChangeToDynamic := DesignedComponent('ButtonChangeToDynamic') as TCastleButton;
  ButtonChangeToDynamic.OnClick := {$ifdef FPC}@{$endif}MakeDynamic;
  ButtonChangeToAnimated := DesignedComponent('ButtonChangeToAnimated') as TCastleButton;
  ButtonChangeToAnimated.OnClick := {$ifdef FPC}@{$endif}MakeAnimated;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
const
  PlayerSpeed = 500;
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if InputLeft then
  begin
    if CheckboxMoveByTranslate.Checked then
      SpherePlayer.Translate(Vector3(-PlayerSpeed * SecondsPassed, 0, 0))
    else
    if CheckboxMoveByVelocity.Checked then
      SpherePlayer.RigidBody.LinearVelocity := SpherePlayer.RigidBody.LinearVelocity + Vector3(-PlayerSpeed * SecondsPassed, 0, 0)
    //else
    //if CheckboxMoveByAnimateTranslation.Checked then
      //
  end
  else
  if InputRight then
  begin
    if CheckboxMoveByTranslate.Checked then
      SpherePlayer.Translate(Vector3(PlayerSpeed * SecondsPassed, 0, 0))
    else
    if CheckboxMoveByVelocity.Checked then
      SpherePlayer.RigidBody.LinearVelocity := SpherePlayer.RigidBody.LinearVelocity + Vector3(PlayerSpeed * SecondsPassed, 0, 0)
    else
    //if CheckboxMoveByAnimateTranslation.Checked then

  end
  else
  begin
    SpherePlayer.RigidBody.LinearVelocity := Vector3(0, 0, 0);
  end;

end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TStateMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

end.
