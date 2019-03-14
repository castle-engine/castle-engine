{
  Copyright 2011-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Player (TPlayer). }
unit GamePlayer;

interface

uses SysUtils, CastleImages, CastleGLUtils, CastleGLImages;

type
  TDirection = (
    dirNorth,
    dirEast,
    dirSouth,
    dirWest,
    dirNorthEast,
    dirSouthEast,
    dirSouthWest,
    dirNorthWest);

  TPlayer = class
  private
    MoveDistance: Single;
    MoveStartTime: Single;
    NewX, NewY: Cardinal;
    NewXPixel, NewYPixel: Integer;
    FX, FY: Cardinal;
    FXPixel: Integer;
    FYPixel: Integer;
    FDirection: TDirection;
  public
    { If Moving then he moves from (X, Y) position to (NewX, NewY).
      MovingSmallMoveX, MovingSmallMoveX is the exact pixel displacement
      of the player sprite then. }
    Moving: boolean;
    MovingSmallMoveX, MovingSmallMoveY: Single;

    DrawableImage: array [TDirection] of TDrawableImage;

    constructor Create;
    destructor Destroy; override;

    property X: Cardinal read FX;
    property Y: Cardinal read FY;
    { These are calculated by CalculatePixelPosition. }
    property XPixel: Integer read FXPixel;
    property YPixel: Integer read FYPixel;

    property Direction: TDirection read FDirection;

    procedure Teleport(ANewX, ANewY: Cardinal; NewDirection: TDirection);
    { Calculate XPixel, YPixel once the Window sizes are known. }
    procedure CalculatePixelPosition;
    { Note that ChangeX, ChangeY may point to invalid coords --- we will
      correct (clamp) them as needed. }
    procedure Move(ChangeX, ChangeY: Integer; NewDirection: TDirection); overload;
    procedure Move(NewDirection: TDirection); overload;

    procedure Update;
  end;

implementation

uses CastleFilesUtils, CastleUtils,
  GameWindow;

constructor TPlayer.Create;
const
  MoveShortcutNames: array [TDirection] of string = (
    'N',
    'E',
    'S',
    'W',
    'NE',
    'SE',
    'SW',
    'NW');
var
  Dir: TDirection;
begin
  inherited Create;

  for Dir := Low(Dir) to High(Dir) do
  begin
    DrawableImage[Dir] := TDrawableImage.Create(
      'castle-data:/tiles/woldforge/sprites/creatures/observer/observer_float_' +
      MoveShortcutNames[Dir] + '_1_hh.png');
  end;
end;

destructor TPlayer.Destroy;
var
  Dir: TDirection;
begin
  for Dir := Low(Dir) to High(Dir) do
    FreeAndNil(DrawableImage[Dir]);
  inherited;
end;

procedure TPlayer.Teleport(ANewX, ANewY: Cardinal; NewDirection: TDirection);
begin
  FX := ANewX;
  FY := ANewy;
  FDirection := NewDirection;
end;

procedure TPlayer.CalculatePixelPosition;
begin
  ViewMoveToCenterPosition(X, Y, FXPixel, FYPixel);
end;

procedure TPlayer.Move(ChangeX, ChangeY: Integer; NewDirection: TDirection);
var
  ANewX, ANewY: Cardinal;
begin
  ANewX := Clamped(Integer(X) + ChangeX, 0, Map.Width - 1);
  ANewY := Clamped(Integer(Y) + ChangeY, 0, Map.Height - 1);
  if not Moving then
  begin
    { Change Direction even if no move will be done --- this is a nice effect
      visually when standing on the edge of the map and trying to move beside
      the edge. }
    FDirection := NewDirection;
    if ((X <> ANewX) or
        (Y <> ANewY)) then
    begin
      Moving := true;
      NewX := ANewX;
      NewY := ANewY;
      ViewMoveToCenterPosition(NewX, NewY, NewXPixel, NewYPixel);
      MoveStartTime := GameTime;
      MovingSmallMoveX := 0;
      MovingSmallMoveY := 0;
      MoveDistance := Sqrt(Sqr(XPixel - NewXPixel) +
                           Sqr(YPixel - NewYPixel));

    end;
  end;
end;

procedure TPlayer.Move(NewDirection: TDirection);
begin
  case NewDirection of
    dirNorth: Move(0,  2, NewDirection);
    dirEast : Move( 1, 0, NewDirection);
    dirSouth: Move(0, -2, NewDirection);
    dirWest : Move(-1, 0, NewDirection);
    dirNorthEast: begin if     Odd(Y) then Move(+1,  1, NewDirection) else Move(0,  1, NewDirection) end;
    dirSouthEast: begin if     Odd(Y) then Move(+1, -1, NewDirection) else Move(0, -1, NewDirection) end;
    dirSouthWest: begin if not Odd(Y) then Move(-1, -1, NewDirection) else Move(0, -1, NewDirection) end;
    dirNorthWest: begin if not Odd(Y) then Move(-1,  1, NewDirection) else Move(0,  1, NewDirection) end;
  end;
end;

procedure TPlayer.Update;
var
  Distance: Single;
const
  PlayerMoveSpeed = 300.0;
begin
  if Moving then
  begin
    Distance := (GameTime - MoveStartTime) * PlayerMoveSpeed;
    if Distance > MoveDistance then
    begin
      FX := NewX;
      FY := NewY;
      FXPixel := NewXPixel;
      FYPixel := NewYPixel;
      Moving := false;
    end else
    begin
      MovingSmallMoveX := MapRange(Distance, 0, MoveDistance,
        { It's XPixel - NewXPixel, not
               NewXPixel - XPixel, because
          our MovingSmallMoveX work in reverse fashion. }
        0, XPixel - NewXPixel);
      MovingSmallMoveY := MapRange(Distance, 0, MoveDistance,
        0, YPixel - NewYPixel);
    end;
  end;
end;

end.
