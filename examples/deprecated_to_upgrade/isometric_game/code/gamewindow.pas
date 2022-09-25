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

{ Game state and window. }
unit GameWindow;

interface

uses CastleWindow,
  GameMap;

const
  BaseWidth = 70;
  BaseHeight = 36;

var
  Window: TCastleWindow;

  { Game time, in seconds. Updated in Update. }
  GameTime: Single;

  Map: TMap;

{ Calculate values suitable for ViewMoveX and ViewMoveY to
  see the map point MapX, MapY exactly in the middle.
  MapX, Y don't have to be in the range 0...Map.Width/Height - 1. }
procedure ViewMoveToCenterPosition(const MapX, MapY: Integer;
  var MoveX, MoveY: Integer);

implementation

procedure ViewMoveToCenterPosition(const MapX, MapY: Integer;
  var MoveX, MoveY: Integer);
begin
  { Set MoveX/Y such that point (0, 0) is in the middle. }
  MoveX := (Window.Width div 2) - BaseWidth div 2;
  MoveY := (Window.Height div 2) - BaseHeight div 2;
  { Now translate such that MapX, MapY is in the middle. }
  MoveX := MoveX - MapX * BaseWidth;
  MoveY := MoveY - MapY * (BaseHeight div 2);
  if Odd(MapY) then
    MoveX := MoveX - (BaseWidth div 2);
end;

end.
