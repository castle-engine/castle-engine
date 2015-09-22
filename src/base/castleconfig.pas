{
  Copyright 2006-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading and saving user preferences (Config). }
unit CastleConfig;

interface

uses SysUtils, CastleXMLConfig;

type
  TCastleConfig = CastleXMLConfig.TCastleConfig;

var
  { An instance of TCastleConfig to manage user preferences.
    All units that want to load or save some configuration should
    add their callbacks to the
    @link(TCastleConfig.AddLoadListener Config.AddLoadListener),
    @link(TCastleConfig.AddSaveListener Config.AddSaveListener) list.
    This way final application may (but doesn't have to) preserve
    the user configuration of all engine components,
    by calling Config.Load, Config.Save. }
  Config: TCastleConfig;

implementation

initialization
  Config := TCastleConfig.Create(nil);
finalization
  FreeAndNil(Config);
end.
