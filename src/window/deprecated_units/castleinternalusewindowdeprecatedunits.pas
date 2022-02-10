{
  Copyright 2019-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Use deprecated units (in src/window/deprecated_units/ in CGE)
  in a way that doesn't cause warnings when compiling Lazarus castle_window.lpk.
  The deprecated units are compiled this way,
  but you can uncheck AddToUsesPkgSection in lpk. }
unit CastleInternalUseWindowDeprecatedUnits;

{.$warn 05074 off} // do not warn about deprecated units
{.$warn 05075 off} // do not warn about deprecated units
{$warnings off} // do not warn about deprecated units, above lines don't work unfortunately with FPC 3.0.4

interface

uses CastleWindowModes, CastleSoundMenu, CastleWindowProgress;

implementation

end.
