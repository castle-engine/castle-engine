{%MainUnit castleutils.pas}
{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Platform (TCastlePlatform) definition and helpers. }

{$ifdef read_interface}

type
  { Possible platforms supported by CGE. See @link(Platform). }
  TCastlePlatform = (
    cpDesktop,
    cpAndroid,
    cpIOS,
    cpNintendoSwitch
  );
  TCastlePlatforms = set of TCastlePlatform;

const
  AllPlatforms = [Low(TCastlePlatform)..High(TCastlePlatform)];

{ Current platform of the compiled application.

  Note that you can also use compilations symbols: CASTLE_IOS, CASTLE_NINTENDO_SWITCH.
  And of course all the symbols defined by Pascal compilers: (for OS:)
  ANDROID, MSWINDOWS, UNIX, LINUX etc., (for CPU:) CPUX86_64, CPUI386, CPUAARCH64 etc. }
function Platform: TCastlePlatform;

function PlatformToStr(const T: TCastlePlatform): String;

{ Convert string to a platform name.
  Case (the difference between upper and lower letters) is ignored.
  Raises Exception on invalid platform name. }
function StrToPlatform(const S: String): TCastlePlatform;

{$endif read_interface}

{$ifdef read_implementation}

function Platform: TCastlePlatform;
begin
  {$if defined(CASTLE_NINTENDO_SWITCH)}
  { Note that ANDROID is also defined for Nintendo Switch for now
    (due to a hacky way we use FPC for Nintendo Switch).
    So we check for CASTLE_NINTENDO_SWITCH first. }
  Exit(cpNintendoSwitch);
  {$elseif defined(ANDROID)}
  Exit(cpAndroid);
  {$elseif defined(CASTLE_IOS)}
  Exit(cpIOS);
  {$else}
  Exit(cpDesktop);
  {$endif}
end;

function PlatformToStr(const T: TCastlePlatform): String;
const
  Names: array [TCastlePlatform] of String = (
    'Desktop',
    'Android',
    'iOS',
    'Nintendo Switch'
  );
begin
  Result := Names[T];
end;

function StrToPlatform(const S: String): TCastlePlatform;
begin
  for Result := Low(TCastlePlatform) to High(TCastlePlatform) do
    if AnsiSameText(PlatformToStr(Result), S) then
      Exit;
  raise Exception.CreateFmt('Invalid platform name "%s"', [S]);
end;

{$endif read_implementation}
