{
  Copyright 2023-2023 Michalis Kamburelis.

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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  CastleViewport, CastleScene, CastleSoundEngine;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps, LabelDistance: TCastleLabel;
    MovingEnv, VerticalSizer: TCastleTransform;
    Car1, Car2: TCastlePlane;
    Car1CollideMarker, Car2CollideMarker: TCastleTransform;
    ButtonCar1Left,
      ButtonCar1Right,
      ButtonCar2Left,
      ButtonCar2Right,
      ButtonSpeed: TCastleButton;
    MainViewport: TCastleViewport;
    SoundCrash: TCastleSound;
  private
    Car1TargetX, Car2TargetX: Single;
    MovingEnvCopy: TCastleTransform;
    { Traveled distance. }
    Distance: Double;
    { When and how to move MoveEnv or MoveEnvCopy to keep the illusion
      of infinite street. }
    MovingEnvNextDistance: Double;
    MovingEnvNextTransform: TCastleTransform;
    StreetMoveSpeed: Single;
    procedure ClickCar1Left(Sender: TObject);
    procedure ClickCar1Right(Sender: TObject);
    procedure ClickCar2Left(Sender: TObject);
    procedure ClickCar2Right(Sender: TObject);
    procedure ButtonSpeedClick(Sender: TObject);
    { X that positions the car exactly in the middle of the street column.
      Doesn't allow cars to move outside of the street (of 3 columns we have). }
    function CarNormalizeToColWidth(const X: Single): Single;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, CastleUtils;

const
  CarHorizontalMoveSpeed = 10.0;
  StreetColWidth = 175.0;
  StreetMoveSpeeds: array [ Boolean { fast? } ] of Single = (100.0, 1000.0);
  { Each MovingEnv / MovingEnvCopy should be shifted by Y from each other. }
  MovingEnvSize = 2000.0;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  // synchronize speed with ButtonSpeed.Pressed state
  StreetMoveSpeed := StreetMoveSpeeds[ButtonSpeed.Pressed];

  Car1TargetX := - StreetColWidth; // car1 starts on left column

  MovingEnvCopy := ComponentClone(MovingEnv, FreeAtStop) as TCastleTransform;
  MovingEnvCopy.Translation := MovingEnv.Translation + Vector3(0, MovingEnvSize, 0);
  MainViewport.Items.Add(MovingEnvCopy);

  MovingEnv.InternalMovesPhysicsBodies := true;
  MovingEnvCopy.InternalMovesPhysicsBodies := true;

  MovingEnvNextDistance := MovingEnvSize;
  MovingEnvNextTransform := MovingEnv;

  ButtonCar1Left.OnClick := {$ifdef FPC}@{$endif} ClickCar1Left;
  ButtonCar1Right.OnClick := {$ifdef FPC}@{$endif} ClickCar1Right;
  ButtonCar2Left.OnClick := {$ifdef FPC}@{$endif} ClickCar2Left;
  ButtonCar2Right.OnClick := {$ifdef FPC}@{$endif} ClickCar2Right;
  ButtonSpeed.OnClick := {$ifdef FPC}@{$endif} ButtonSpeedClick;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  procedure UpdateCarCollides(const Car: TCastlePlane; const CarCollideMarker: TCastleTransform);
  var
    OldCollides, Collides: Boolean;
  begin
    OldCollides := CarCollideMarker.Exists;
    Collides := Car.RigidBody.GetCollidingTransforms.Count <> 0;
    if not OldCollides and Collides then
      SoundEngine.Play(SoundCrash);
    CarCollideMarker.Exists := Collides;
    { // does not work on mobile in CGE now
    if Collides then
      Car.RenderOptions.WireframeEffect := weSilhouette
    else
      Car.RenderOptions.WireframeEffect := weNormal;
    }
  end;

  procedure MoveCarHorizontally(const Car: TCastleTransform; const TargetX: Single);
  begin
    Car.Translation := Vector3(
      SmoothTowards(Car.Translation.X, TargetX, SecondsPassed, CarHorizontalMoveSpeed),
      Car.Translation.Y,
      Car.Translation.Z);
  end;

var
  EnvMove: Single;
  MovingEnvPrevTransform: TCastleTransform;
begin
  inherited;

  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  EnvMove := SecondsPassed * StreetMoveSpeed;
  Distance := Distance + EnvMove;
  MovingEnv.Translation := MovingEnv.Translation - Vector3(0, EnvMove, 0);
  MovingEnvCopy.Translation := MovingEnvCopy.Translation - Vector3(0, EnvMove, 0);

  if Distance >= MovingEnvNextDistance then
  begin
    // MovingEnvNextTransform swaps between MovingEnv and MovingEnvCopy
    if MovingEnvNextTransform = MovingEnv then
      MovingEnvPrevTransform := MovingEnvCopy
    else
      MovingEnvPrevTransform := MovingEnv;

    MovingEnvNextTransform.Translation := MovingEnvPrevTransform.Translation + Vector3(0, MovingEnvSize, 0);
    MovingEnvNextTransform := MovingEnvPrevTransform;
    MovingEnvNextDistance := MovingEnvNextDistance + MovingEnvSize;
  end;

  MoveCarHorizontally(Car1, Car1TargetX);
  MoveCarHorizontally(Car2, Car2TargetX);

  LabelDistance.Caption := FormatDot('%f', [Distance]);

  UpdateCarCollides(Car1, Car1CollideMarker);
  UpdateCarCollides(Car2, Car2CollideMarker);
end;

function TViewMain.CarNormalizeToColWidth(const X: Single): Single;
begin
  if X < -StreetColWidth / 2 then
    Result := - StreetColWidth
  else
  if X > StreetColWidth / 2 then
    Result := StreetColWidth
  else
    Result := 0;
end;

procedure TViewMain.ClickCar1Left(Sender: TObject);
begin
  Car1TargetX := CarNormalizeToColWidth(Car1.Translation.X - StreetColWidth);
end;

procedure TViewMain.ClickCar1Right(Sender: TObject);
begin
  Car1TargetX := CarNormalizeToColWidth(Car1.Translation.X + StreetColWidth);
end;

procedure TViewMain.ClickCar2Left(Sender: TObject);
begin
  Car2TargetX := CarNormalizeToColWidth(Car2.Translation.X - StreetColWidth);
end;

procedure TViewMain.ClickCar2Right(Sender: TObject);
begin
  Car2TargetX := CarNormalizeToColWidth(Car2.Translation.X + StreetColWidth);
end;

procedure TViewMain.ButtonSpeedClick(Sender: TObject);
begin
  ButtonSpeed.Pressed := not ButtonSpeed.Pressed;
  StreetMoveSpeed := StreetMoveSpeeds[ButtonSpeed.Pressed];
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyA) then
  begin
    ClickCar1Left(nil);
    Exit(true);
  end;

  if Event.IsKey(keyS) then
  begin
    ClickCar1Right(nil);
    Exit(true);
  end;

  if Event.IsKey(keyK) then
  begin
    ClickCar2Left(nil);
    Exit(true);
  end;

  if Event.IsKey(keyL) then
  begin
    ClickCar2Right(nil);
    Exit(true);
  end;

  if Event.IsKey(keySpace) then
  begin
    ButtonSpeedClick(nil);
    Exit(true);
  end;
end;

end.
