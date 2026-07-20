{
  Copyright 2014-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ "Invaders" game logic. }
unit GameInvaders;

interface

uses SysUtils, Generics.Collections, Classes,
  CastleUIControls, CastleKeysMouse, CastleVectors,
  CastleFilesUtils, CastleApplicationProperties, CastleUtils,
  CastleRectangles, CastleGLUtils, CastleColors;

type
  { Render and update whole "invaders" game logic.

    Note: This is not our recommended way to write games.

    1. We render here manually, and we ignore UI scaling. We assume the screen
       has always enough size to fit our game.

    2. And we don't load any files, even images.

    But the above constraints match current web platform limits.
    So this is a "good enough" approach for this demo.

    For more flexible code, you should instantiate things like player or enemy
    as

    - instances of TCastleUserInterface descendants (like TCastleImageControl)

    - or TCastleTransform (in the latter case, put them inside TCastleViewport).
      In this case things can also e.g. collide using physics.
  }
  TInvadersGame = class(TCastleUserInterface)
  strict private
    type
      TPlayer = class
        Position: TVector2;
        Alive: Boolean;
        procedure Render;
        function Rect: TFloatRectangle;
      end;

      TEnemy = class
        Position: TVector2;
        Alive: Boolean;
        procedure Render;
        function Rect: TFloatRectangle;
      end;

      TRocket  = class
        Position: TVector2;
        GoingUp: Boolean;
        function Rect: TFloatRectangle;
        procedure Render;
      end;

      TRocketList = class({$ifdef FPC}specialize{$endif} TObjectList<TRocket>)
        procedure DeleteFast(const I: Integer);
      end;

    const
      EnemiesCountX = 10;
      EnemiesCountY = 5;

      MaxRockets = 10;

      TimeToEnemyRocketMin = 0.1;
      TimeToEnemyRocketMax = 1.0;

    var
      Player: TPlayer;
      Enemies: array [0..EnemiesCountX - 1, 0..EnemiesCountY - 1] of TEnemy;
      PlayerRockets, EnemyRockets: TRocketList;
      TimeToEnemyRocket: Single;
      EnemiesHorizMoveOdd: Boolean;
      TimeToEnemiesHorizMoveSwitch: Single;
      FSomeEnemyAlive: Boolean;
      FEnemiesGotToPlayer: Boolean;
      FEasy: Boolean;
  public
    constructor Create(const AOwner: TComponent; const AEasy: Boolean); reintroduce;
    destructor Destroy; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure Render; override;

    { Observe these values to know when game ended. }
    property SomeEnemyAlive: Boolean read FSomeEnemyAlive;
    function PlayerAlive: Boolean;
    property EnemiesGotToPlayer: Boolean read FEnemiesGotToPlayer;
  end;

implementation

uses CastleLog;

{ Draw a triangle. Utility function to draw other things in this game. }
procedure DrawTriangleInRect(const R: TFloatRectangle;
  const Color: TCastleColor; const PointingUp: Boolean);
var
  Points: array [0..2] of TVector2;
begin
  if PointingUp then
  begin
    Points[0] := Vector2(R.Left, R.Bottom);
    Points[1] := Vector2(R.Right, R.Bottom);
    Points[2] := Vector2(R.Center.X, R.Top);
  end else
  begin
    Points[0] := Vector2(R.Left, R.Top);
    Points[1] := Vector2(R.Right, R.Top);
    Points[2] := Vector2(R.Center.X, R.Bottom);
  end;

  DrawPrimitive2D(pmTriangles, Points, Color);
end;

{ TPlayer -------------------------------------------------------------------- }

procedure TInvadersGame.TPlayer.Render;
begin
  inherited;
  if Alive then
    DrawTriangleInRect(Rect, White, true);
end;

function TInvadersGame.TPlayer.Rect: TFloatRectangle;
begin
  Result := FloatRectangle(
    Position.X,
    Position.Y,
    100,
    50);
end;

{ TEnemy --------------------------------------------------------------------- }

procedure TInvadersGame.TEnemy.Render;
begin
  inherited;
  if Alive then
    DrawTriangleInRect(Rect, Red, false);
end;

function TInvadersGame.TEnemy.Rect: TFloatRectangle;
begin
  Result := FloatRectangle(
    Position.X,
    Position.Y,
    40,
    40);
end;

{ TRocket -------------------------------------------------------------------- }

function TInvadersGame.TRocket.Rect: TFloatRectangle;
begin
  Result := FloatRectangle(
    Position.X,
    Position.Y,
    10,
    20);
end;

procedure TInvadersGame.TRocket.Render;
begin
  inherited;
  DrawTriangleInRect(Rect, Green, GoingUp);
end;

{ TRocketList ---------------------------------------------------------------- }

procedure TInvadersGame.TRocketList.DeleteFast(const I: Integer);
begin
  { More efficient then Remove(Index) because we don't care
    about the order of rockets. }
  Exchange(I, Count - 1);
  Count := Count - 1;
end;

{ TInvadersGame -------------------------------------------------------------- }

constructor TInvadersGame.Create(const AOwner: TComponent; const AEasy: Boolean);
var
  X, Y: Integer;
begin
  inherited Create(AOwner);

  FEasy := AEasy;

  Player := TPlayer.Create;
  Player.Alive := true;
  Player.Position := Vector2(400, 30);

  for X := 0 to EnemiesCountX - 1 do
    for Y := 0 to EnemiesCountY - 1 do
    begin
      Enemies[X, Y] := TEnemy.Create;
      Enemies[X, Y].Position.X := 50 + X * 70;
      Enemies[X, Y].Position.Y := Y * 70 + 200;
      Enemies[X, Y].Alive := true;
    end;

  PlayerRockets := TRocketList.Create(true);
  EnemyRockets := TRocketList.Create(true);

  TimeToEnemyRocket := TimeToEnemyRocketMax;
  TimeToEnemiesHorizMoveSwitch := 2;
  FSomeEnemyAlive := true; // default value, will be recalculated in Update
end;

destructor TInvadersGame.Destroy;
begin
  FreeAndNil(Player);
  FreeAndNil(PlayerRockets);
  FreeAndNil(EnemyRockets);
  inherited;
end;


function TInvadersGame.Press(const Event: TInputPressRelease): Boolean;
var
  NewRocket: TRocket;
begin
  Result := inherited;
  if Result then Exit;

  if (// space is already used by web browsers to scroll the page
      {$ifndef WASI} Event.IsKey(keySpace) or {$endif}
      Event.IsKey(keyW)) and
     (PlayerRockets.Count < MaxRockets) then
  begin
    NewRocket := TRocket.Create;
    NewRocket.Position := Player.Rect.Center;
    NewRocket.GoingUp := true;
    PlayerRockets.Add(NewRocket);
    Exit(true);
  end;
end;

procedure TInvadersGame.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  { Check if rocket hits enemy, and if so, destroy the enemy.
    Returns should we destroy the rocket (happens if we destroyed
    the enemey or went off-screen). }
  function RocketHitsEnemy(const Rocket: TRocket): Boolean;
  var
    X, Y: Integer;
  begin
    Result := false;

    if not Rocket.Rect.Collides(FloatRectangle(Container.PixelsRect)) then
      Exit(true);

    for X := 0 to EnemiesCountX - 1 do
      for Y := 0 to EnemiesCountY - 1 do
        if Enemies[X, Y].Alive and
           Rocket.Rect.Collides(Enemies[X, Y].Rect) then
        begin
          Enemies[X, Y].Alive := false;
          Exit(true);
        end;
  end;

  { Check if rocket hits enemy, and if so, mark the player as hit.
    Returns should we destroy the rocket (happens if we destroyed
    the enemey or went off-screen). }
  function RocketHitsPlayer(const Rocket: TRocket): boolean;
  begin
    Result := false;

    if not Rocket.Rect.Collides(FloatRectangle(Container.PixelsRect)) then
      Exit(true);

    if Rocket.Rect.Collides(Player.Rect) then
      Player.Alive := false;
  end;

const
  PlayerSpeed = 300;
  PlayerRocketSpeed = 500;
  EnemyRocketSpeedEasy = 250;
  EnemyRocketSpeedHard = 500;
  EnemyRocketSpeedChanceEasy = 1 / 120;
  EnemyRocketSpeedChanceHard = 1 / 80;
  EnemiesVertSpeed = 10;
  EnemiesHorizSpeed = 10;
var
  X, Y, I: Integer;
  NewRocket: TRocket;
  OddY: Boolean;
begin
  inherited;

  // move player
  if Container.Pressed[keyA]
     // arrows are already used by browser to scroll the page
     {$ifndef WASI} or Container.Pressed[keyArrowLeft] {$endif} then
    Player.Position.X := Player.Position.X - SecondsPassed * PlayerSpeed;
  if Container.Pressed[keyD]
     {$ifndef WASI} or Container.Pressed[keyArrowRight] {$endif} then
    Player.Position.X := Player.Position.X + SecondsPassed * PlayerSpeed;

  // move player rockets and check did they hit enemies
  I := 0;
  while I < PlayerRockets.Count do // while, not for loop, as the Count may change
  begin
    PlayerRockets[I].Position.Y := PlayerRockets[I].Position.Y
      + SecondsPassed * PlayerRocketSpeed;
    if RocketHitsEnemy(PlayerRockets[I]) then
      PlayerRockets.DeleteFast(I)
    else
      Inc(I);
  end;

  // move enemy rockets and check did they hit player
  I := 0;
  while I < EnemyRockets.Count do // while, not for loop, as the Count may change
  begin
    EnemyRockets[I].Position.Y := EnemyRockets[I].Position.Y
      - SecondsPassed * Iff(FEasy, EnemyRocketSpeedEasy, EnemyRocketSpeedHard);
    if RocketHitsPlayer(EnemyRockets[I]) then
      EnemyRockets.DeleteFast(I)
    else
      Inc(I);
  end;

  TimeToEnemyRocket := TimeToEnemyRocket - SecondsPassed;

  FSomeEnemyAlive := false;
  for X := 0 to EnemiesCountX - 1 do
    for Y := 0 to EnemiesCountY - 1 do
      if Enemies[X, Y].Alive then
      begin
        FSomeEnemyAlive := true;
        OddY := Odd(Y);
        if EnemiesHorizMoveOdd then
          OddY := not OddY;
        if OddY then
          Enemies[X, Y].Position.X := Enemies[X, Y].Position.X
            - SecondsPassed * EnemiesHorizSpeed
        else
          Enemies[X, Y].Position.X := Enemies[X, Y].Position.X
            + SecondsPassed * EnemiesHorizSpeed;
        Enemies[X, Y].Position.Y := Enemies[X, Y].Position.Y
          - SecondsPassed * EnemiesVertSpeed;

        if Enemies[X, Y].Position.Y < 100 then
        begin
          FEnemiesGotToPlayer := true;
          Exit;
        end;

        if (EnemyRockets.Count < MaxRockets) and
           (TimeToEnemyRocket <= 0) and
           { TODO: This way of making rockets less often isn't completely
             time-independent, because in case there are few enemies,
             it will be retried next frame, so "how often this happens" ->
             depends on SecondsPassed, which depends on the machine speed. }
           (Random < Iff(FEasy, EnemyRocketSpeedChanceEasy, EnemyRocketSpeedChanceHard)) then
        begin
          NewRocket := TRocket.Create;
          NewRocket.Position := Enemies[X, Y].Rect.Center;
          NewRocket.GoingUp := false;
          EnemyRockets.Add(NewRocket);
          TimeToEnemyRocket := RandomFloatRange(TimeToEnemyRocketMin, TimeToEnemyRocketMax);
        end;
      end;

  TimeToEnemiesHorizMoveSwitch := TimeToEnemiesHorizMoveSwitch - SecondsPassed;
  if TimeToEnemiesHorizMoveSwitch <= 0 then
  begin
    EnemiesHorizMoveOdd := not EnemiesHorizMoveOdd;
    TimeToEnemiesHorizMoveSwitch := 2;
  end;
end;

procedure TInvadersGame.Render;
var
  X, Y, I: Integer;
begin
  inherited;

  Player.Render;

  for X := 0 to EnemiesCountX - 1 do
    for Y := 0 to EnemiesCountY - 1 do
      Enemies[X, Y].Render;

  for I := 0 to PlayerRockets.Count - 1 do
    PlayerRockets[I].Render;
  for I := 0 to EnemyRockets.Count - 1 do
    EnemyRockets[I].Render;
end;

function TInvadersGame.PlayerAlive: Boolean;
begin
  Result := Player.Alive;
end;

end.
