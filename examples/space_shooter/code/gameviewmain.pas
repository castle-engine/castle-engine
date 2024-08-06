{
  Copyright 2019-2024 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleTransform, CastleTimeUtils,
  GameTilingBackground, GameRocketsManager, GameRocksManager;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    SpaceShip: TCastleTransform;
    RocketsParent: TCastleTransform;
    RocksParent: TCastleTransform;
    RectArmedHint: TCastleRectangleControl;
  private
    LifeTime: TFloatTime;
    RocketsManager: TRocketsManager;
    RocksManager: TRocksManager;
    TilingBackground: TTilingBackground;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure Resize; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleRectangles, CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
const
  CannonsCount = 3;
var
  I: Integer;
begin
  inherited;

  TilingBackground := TTilingBackground.Create(FreeAtStop);
  TilingBackground.Translation := Vector3(0, 0, -100); // put in the back
  TilingBackground.UpdateCoordinates(MainViewport);
  MainViewport.Items.Add(TilingBackground);

  RocketsManager := TRocketsManager.Create(FreeAtStop);
  RocketsManager.RocketsParent := RocketsParent;
  SetLength(RocketsManager.Cannons, CannonsCount);
  for I := 0 to CannonsCount - 1 do
    RocketsManager.Cannons[I] := DesignedComponent('Cannon' + IntToStr(I)) as TCastleTransform;
  RocketsManager.InitializeCannons;
  { RocketsManager is a UI element, it has to be inserted into the UI hierarchy.
    It doesn't actually display anything, but it can listen to events like Update
    and have children like TCastleTimer. }
  InsertFront(RocketsManager);

  RocksManager := TRocksManager.Create(FreeAtStop);
  RocksManager.RocksParent := RocksParent;
  RocksManager.InitializeRockResources(MainViewport);
  InsertFront(RocksManager);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  { Move the spaceship by DeltaSign,
    where each DeltaSign component is either -1, 0 or 1.

    The movement is clamped to the allowed range AllowedPositions.

    The total movement distance cannot be more than MaxDelta.
    Both MaxDelta components have to be >= 0.
    Honoring MaxDelta is important to avoid "ping-pong" movement
    around the desired position, when the mouse is very close to the spaceship. }
  procedure MoveSpaceShip(const DeltaSign: TVector2Integer; const MaxDelta: TVector2);
  const
    MoveSpeed = 1000;
    // Allowed SpaceShip.Translation.XY range
    AllowedPositions: TFloatRectangle = (Left: -400; Bottom: -400; Width: 800; Height: 800);
  var
    NewTranslation, Delta: TVector2;
  begin
    Assert((DeltaSign.X = 0) or (DeltaSign.X = 1) or (DeltaSign.X = -1));
    Assert((DeltaSign.Y = 0) or (DeltaSign.Y = 1) or (DeltaSign.Y = -1));
    Assert(MaxDelta.X >= 0);
    Assert(MaxDelta.Y >= 0);

    // calculate Delta as is the vector we move if DeltaSign is (1,1)
    Delta := Vector2(
      MoveSpeed * SecondsPassed,
      MoveSpeed * SecondsPassed
    );
    MinVar(Delta.X, MaxDelta.X);
    MinVar(Delta.Y, MaxDelta.Y);

    NewTranslation := SpaceShip.TranslationXY + Vector2(
      // multiply TVector2 * TVector2Integer, component-wise
      Delta.X * DeltaSign.X,
      Delta.Y * DeltaSign.Y
    );

    ClampVar(NewTranslation.X, AllowedPositions.Left, AllowedPositions.Right);
    ClampVar(NewTranslation.Y, AllowedPositions.Bottom, AllowedPositions.Top);

    SpaceShip.TranslationXY := NewTranslation;
  end;

  { Get desired delta to move from mouse position.
    Resulting vector contains the maximum move vector,
    that would result spaceship to teleport towards the mouse. }
  function GetMovementDirectionFromMouse: TVector2;
  const
    DistanceToReact = 5;
  var
    SpaceShipDesiredTranslation: TVector2;
  begin
    SpaceShipDesiredTranslation := MainViewport.PositionTo2DWorld(
      Container.MousePosition, true);
    Result := SpaceShipDesiredTranslation - SpaceShip.TranslationXY;

    if Abs(Result.X) < DistanceToReact then
      Result.X := 0;
    if Abs(Result.Y) < DistanceToReact then
      Result.Y := 0;
  end;

  { Allow player to move the spaceship by keys or mouse/touch. }
  procedure UpdateMoveSpaceShip;
  var
    MovementDirectionFromMouse, MaxDelta: TVector2;
    DeltaSign: TVector2Integer;
  begin
    DeltaSign := TVector2Integer.Zero;

    if Container.MousePressed = [] then
    begin
      // move with keys, if no mouse button is pressed
      if Container.Pressed[keyArrowLeft] or Container.Pressed[keyA] then
        DeltaSign.X := DeltaSign.X - 1;
      if Container.Pressed[keyArrowRight] or Container.Pressed[keyD] then
        DeltaSign.X := DeltaSign.X + 1;
      if Container.Pressed[keyArrowUp] or Container.Pressed[keyW] then
        DeltaSign.Y := DeltaSign.Y + 1;
      if Container.Pressed[keyArrowDown] or Container.Pressed[keyS] then
        DeltaSign.Y := DeltaSign.Y - 1;
      // large values, we don't want MaxDelta to limit movement in this case
      MaxDelta := Vector2(1000, 1000);
    end else
    begin
      // move with mouse, if any mouse button is pressed
      MovementDirectionFromMouse := GetMovementDirectionFromMouse;
      DeltaSign.X := Sign(MovementDirectionFromMouse.X);
      DeltaSign.Y := Sign(MovementDirectionFromMouse.Y);
      MaxDelta.X := Abs(MovementDirectionFromMouse.X);
      MaxDelta.Y := Abs(MovementDirectionFromMouse.Y);
    end;

    if not DeltaSign.IsZero then
      MoveSpaceShip(DeltaSign, MaxDelta);
  end;

  procedure UpdateBackground;
  const
    BackgroundMoveSpeed = 1.0;
  begin
    TilingBackground.ImageOrigin := Vector2Double(BackgroundMoveSpeed * LifeTime, 0.0);
    TilingBackground.UpdateCoordinates(MainViewport);
  end;

begin
  inherited;
  { This virtual method is executed every frame (many times per second). }

  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption :=
    'FPS: ' + Container.Fps.ToString + NL +
    'Rockets existing count: ' + IntToStr(RocketsParent.Count) + NL +
    'Rocks existing count: ' + IntToStr(RocksParent.Count) + NL +
    'Rocks destroyed: ' + IntToStr(RocksManager.RocksDestroyed);

  { Sum up SecondsPassed values into LifeTime (type TFloatTime = Double).
    This keeps them precise, even after a long play, when the value gets large. }
  LifeTime := LifeTime + SecondsPassed;

  UpdateMoveSpaceShip;
  UpdateBackground;

  RocketsManager.ShootsArmed := Container.Pressed[keySpace];
  if RocketsManager.ShootsArmed then
    RectArmedHint.Border.AllSides := 1
  else
    RectArmedHint.Border.AllSides := 0;
end;

procedure TViewMain.Resize;
begin
  inherited;
  TilingBackground.UpdateCoordinates(MainViewport);
end;

end.
