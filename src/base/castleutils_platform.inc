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
    cpNintendoSwitch,
    cpWeb
  );
  TCastlePlatforms = set of TCastlePlatform;

const
  AllPlatforms = [Low(TCastlePlatform)..High(TCastlePlatform)];

(*Current platform of this application, where we run on.

  "Platform" is our term that encompasses a group of OS (operating system) and
  CPU (processor) combinations that behave similar in some way (for users,
  for developers).
  For example, cpDesktop is a platform that includes all desktop OSes
  (Linux, Windows, FreeBSD, macOS...) on all their supported CPUs.

  Note There are many ways how you can make your code behave differently
  depending on the platform where it runs.

  @unoderedList(
    @item(Using this function, and checking like @code(if Platform = ... then)
      is one approach.)

    @item(Checking
      @link(TCastleApplicationProperties.TouchDevice ApplicationProperties.TouchDevice)
      is useful if you want to differentiate between devices with touch
      screen (and dedicated API for multi-touch) or not.
      See @url(https://castle-engine.io/touch_input touch input) for more details.)

    @item(You can also use symbols to compile certain
      code only on a specific platform or specific OS or specific CPU.
      For example @code({$ifdef MSWINDOWS} WritelnLog('This is Windows'); {$endif}).

      We define some symbols by our engine build tool:

      @unorderedList(
        @item @code(CASTLE_IOS)
        @item @code(CASTLE_NINTENDO_SWITCH)
      )

      The compilers (Delphi or FPC) define much more symbols for this purpose.
      Detect OS by symbols like this:

      @unorderedList(
        @item @code(LINUX)
        @item @code(UNIX)
        @item @code(MSWINDOWS)
        @item @code(DARWIN)
        @item @code(ANDROID)
        @item @code(WASI) (web)
      )

      Detect CPU by symbols like this:

      @unorderedList(
        @item @code(CPUX86_64)
        @item @code(CPUI386)
        @item @code(CPUAARCH64)
      )

      Refer to the documentation of your Pascal compiler for more details.
    )
  ) *)
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
    'Nintendo Switch',
    'Web'
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
