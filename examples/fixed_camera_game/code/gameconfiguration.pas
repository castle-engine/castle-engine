{
  Copyright 2007-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Read data/game.xml that configures game. }
unit GameConfiguration;

interface

uses CastleXMLConfig;

var
  GameConfig: TCastleConfig;

implementation

uses SysUtils;

finalization
  FreeAndNil(GameConfig);
end.
