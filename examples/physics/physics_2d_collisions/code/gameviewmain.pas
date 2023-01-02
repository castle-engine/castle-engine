{
  Copyright 2017-2022 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleNotifications,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport, CastleTransform;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps, LabelCollisionsInfo: TCastleLabel;
    Viewport: TCastleViewport;
    Plane: TCastleTransform;
    MainNotifications: TCastleNotifications;
    WaterRigidBody, PlaneRigidBody: TCastleRigidBody;
  private
    procedure UpdatePlanePosition(const EventPosition: TVector2);
    procedure WaterCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
    procedure WaterCollisionExit(const CollisionDetails: TPhysicsCollisionDetails);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Motion(const Event: TInputMotion): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleStringUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  WaterRigidBody.OnCollisionEnter := {$ifdef FPC}@{$endif} WaterCollisionEnter;
  WaterRigidBody.OnCollisionExit := {$ifdef FPC}@{$endif} WaterCollisionExit;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  function CollisionsListStr(const RBody: TCastleRigidBody): String;
  var
    CollisionsList: TCastleTransformList;
    I: Integer;
  begin
    CollisionsList := RBody.GetCollidingTransforms;
    Result := '';

    for I := 0 to CollisionsList.Count - 1 do
      Result := SAppendPart(Result, ',', CollisionsList[I].Name);

    if Result = '' then
      Result := 'nothing';
  end;

begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  LabelCollisionsInfo.Caption := 'Plane collides now with: ' +
    CollisionsListStr(PlaneRigidBody);
end;

procedure TViewMain.UpdatePlanePosition(const EventPosition: TVector2);
begin
  Plane.Translation := Vector3(Viewport.PositionTo2DWorld(EventPosition, true), 0);
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonLeft) then
  begin
    UpdatePlanePosition(Event.Position);
    Exit(true);
  end;
end;

function TViewMain.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if buttonLeft in Event.Pressed then
  begin
    UpdatePlanePosition(Event.Position);
    Exit(true);
  end;
end;

procedure TViewMain.WaterCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
begin
  MainNotifications.Show('Water starts colliding with ' + CollisionDetails.OtherTransform.Name);
end;

procedure TViewMain.WaterCollisionExit(const CollisionDetails: TPhysicsCollisionDetails);
begin
  MainNotifications.Show('Water stops colliding with ' + CollisionDetails.OtherTransform.Name);
end;

end.
