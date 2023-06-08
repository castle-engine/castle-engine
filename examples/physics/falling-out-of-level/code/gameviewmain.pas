{
  Copyright 2023-2023 Michalis Kamburelis, Andrzej Kilijański.

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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    CylinderRigidBody: TCastleRigidBody;
    Cylinder: TCastleScene;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils;

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
const
  MaxVelocity = 60;
  MaxVelocityIncrease = MaxVelocity {* 60};
var
  DeltaVelocity: TVector3;
  Rot: TVector4;
  Transl: TVector3;
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  DeltaVelocity := Vector3(0,0,0);

  if Container.Pressed.Items[keyA] then
    DeltaVelocity.z := MaxVelocityIncrease * SecondsPassed;

  if Container.Pressed.Items[keyD] then
    DeltaVelocity.z := - MaxVelocityIncrease * SecondsPassed;

  if Container.Pressed.Items[keyW] then
    DeltaVelocity.x := - MaxVelocityIncrease * SecondsPassed;

  if Container.Pressed.Items[keyS] then
    DeltaVelocity.x := MaxVelocityIncrease * SecondsPassed;

  if Container.Pressed.Items[keyX] then
    DeltaVelocity.y := - MaxVelocityIncrease * SecondsPassed;

  if Container.Pressed.Items[keySpace] then
    DeltaVelocity.y := MaxVelocityIncrease * SecondsPassed;

  CylinderRigidBody.LinearVelocity := CylinderRigidBody.LinearVelocity + DeltaVelocity;

  if Container.Pressed.Items[keyR] then
  begin
    Rot := Cylinder.Rotation;
    Rot.X := 0.2;
    Rot.y := 0.2;
    Rot.z := 0.2;
    Rot.W := Rot.W + 30;
    Cylinder.Rotation := Rot;
  end;

  if Container.Pressed.Items[keyT] then
  begin
    Transl := Cylinder.Translation;
    Transl.y := Transl.y - 0.5;
    Cylinder.Translation := Transl;
  end;

end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
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
