{
  Copyright 2006-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading and saving user preferences (UserConfig). }
unit CastleConfig;

{$I castleconf.inc}

interface

uses SysUtils, CastleXMLConfig;

type
  TCastleConfig = CastleXMLConfig.TCastleConfig;

{ User preferences.
  See http://castle-engine.sourceforge.net/tutorial_user_prefs.php . }
function UserConfig: TCastleConfig;

function Config: TCastleConfig; deprecated 'use UserConfig';

implementation

var
  FUserConfig: TCastleConfig;

function UserConfig: TCastleConfig;
begin
  Result := FUserConfig;
end;

function Config: TCastleConfig;
begin
  Result := UserConfig;
end;

initialization
  FUserConfig := TCastleConfig.Create(nil);
finalization
  FreeAndNil(FUserConfig);
end.
