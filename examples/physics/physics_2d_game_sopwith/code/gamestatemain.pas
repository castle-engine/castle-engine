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
{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform, CastleViewport;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelStatus: TCastleLabel;
    Viewport: TCastleViewport;
    Plane: TCastleTransform;
  private
    BoxDropTimer, MissileShootTimer: TCastleTimer;
    procedure EventBoxDrop(Sender: TObject);
    procedure EventMissileShoot(Sender: TObject);
    procedure UpdatePlanePosition(const EventPosition: TVector2);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Motion(const Event: TInputMotion): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleRectangles, CastleUtils;

type
  { The parent transform will be automatically removed when not visible.
    This avoids simulating objects that get off-screen. }
  TAutoRemoveBehavior = class(TCastleBehavior)
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

procedure TAutoRemoveBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  ParentRect, CameraRect: TFloatRectangle;
begin
  inherited;

  ParentRect := Parent.WorldBoundingBox.RectangleXY;
  CameraRect := World.MainCamera.Orthographic.EffectiveRect;

  if not ParentRect.Collides(CameraRect) then
    Parent.Parent.RemoveDelayed(Parent, true);
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

  BoxDropTimer := TCastleTimer.Create(FreeAtStop);
  BoxDropTimer.IntervalSeconds := 0.2;
  BoxDropTimer.OnTimer := {$ifdef FPC}@{$endif} EventBoxDrop;
  InsertFront(BoxDropTimer);

  MissileShootTimer := TCastleTimer.Create(FreeAtStop);
  MissileShootTimer.IntervalSeconds := 0.1;
  MissileShootTimer.OnTimer := {$ifdef FPC}@{$endif} EventMissileShoot;
  InsertFront(MissileShootTimer);
end;

procedure TStateMain.EventBoxDrop(Sender: TObject);
var
  NewTransform: TCastleTransform;
begin
  // stop dropping boxes when too many, it would slow down the game
  if Viewport.Items.Count >= 50 then
    Exit;

  NewTransform := TransformLoad('castle-data:/box_drop.castle-transform', FreeAtStop);
  NewTransform.Translation := Vector3(0, Viewport.Camera.Orthographic.EffectiveRect.Top, 0);
  NewTransform.AddBehavior(TAutoRemoveBehavior.Create(FreeAtStop));
  Viewport.Items.Add(NewTransform);
end;

procedure TStateMain.EventMissileShoot(Sender: TObject);
var
  NewTransform: TCastleTransform;
begin
  NewTransform := TransformLoad('castle-data:/missile.castle-transform', FreeAtStop);
  NewTransform.Translation := Plane.Translation + Vector3(5, 0, 0);
  NewTransform.AddBehavior(TAutoRemoveBehavior.Create(FreeAtStop));
  Viewport.Items.Add(NewTransform);
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelStatus.Caption := Format(
    'FPS: %s' + NL +
    'Viewport Items: %d' + NL +
    'Click or drag with mouse to move the plane.', [
    Container.Fps.ToString,
    Viewport.Items.Count
  ]);
end;

procedure TStateMain.UpdatePlanePosition(const EventPosition: TVector2);
begin
  Plane.Translation := Vector3(Viewport.PositionTo2DWorld(EventPosition, true), 0);
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonLeft) then
  begin
    UpdatePlanePosition(Event.Position);
    Exit(true);
  end;
end;

function TStateMain.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if buttonLeft in Event.Pressed then
  begin
    UpdatePlanePosition(Event.Position);
    Exit(true);
  end;
end;

end.
