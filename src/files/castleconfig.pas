{
  Copyright 2006-2018 Michalis Kamburelis.

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
  See https://castle-engine.io/manual_user_prefs.php . }
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
  { Delphi on macOS crashes (runtime error 217)
    when trying to free TDOMDocument underneath, which releases the interface.
    Reason unknown, not happens with FPC or other Delphi platforms (Windows, Linux).
    Reproduce: run examples/viewport_and_scenes/scene_information_cli_tool/
    on macOS (Intel-based) with Delphi 12.3. }
  {$if not (defined(DELPHI) and defined(DARWIN))}
  FreeAndNil(FUserConfig);
  {$endif}
end.
