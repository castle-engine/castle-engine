{
  Copyright 2023-2024 Michalis Kamburelis, Eugene Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Steam integration utilities for this project. }
unit GameSteam;

interface

uses CastleSteam;

const
  { Application id on Steam.
    By default we are using AppID of SteamWorks game example: SpaceWar.
    See the documentation on https://castle-engine.io/steam
    for more information about AppID. }
  AppId = 480;

var
  // Created in GameInitialize unit.
  Steam: TCastleSteam;

implementation

uses SysUtils;

initialization
finalization
  FreeAndNil(Steam);
end.
