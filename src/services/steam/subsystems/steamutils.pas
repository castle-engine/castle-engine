{
  Copyright 2023-2024 Michalis Kamburelis, Yevhen Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Integration with Steam.
  See @url(https://castle-engine.io/steam Steam and Castle Game Engine documentation)
  for usage. }
unit steamutils;

interface

uses Classes, CTypes, SysUtils, SteamTypes, SteamSubsystem;

type
  TIconSize = (IconSmall, IconMedium, IconLarge);

  TSteamUtils = class(TSteamSubsystem)
  private
  public
    constructor Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32); override;
    destructor Destroy; override;
    function GetSteamBitmap(const ImageHandle: CInt): TSteamBitmap;
    function GetSteamAppID: CInt;
    { 2 digit ISO 3166-1-alpha-2 format country code this client
      is running in (as looked up via an IP-to-location database) e.g "US" or "UK". }
    function IPCountry: String;

    { Is the Steam overlay running and the user can access it.
      The overlay process could take a few seconds to
      start & hook the game process, so this function will initially return false
      while the overlay is loading. }
    function IsOverlayEnabled: Boolean;

    { Is Steam running in VR mode. }
    function IsSteamRunningInVR: Boolean;

    { Is currently running on the Steam Deck device. }
    function IsRunningOnSteamDeck: Boolean;
  end;

implementation

uses CastleInternalSteamApi, CastleLog;

{ TSteamApps }

constructor TSteamUtils.Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32);
begin
  FAPIHandle := SteamAPI_ISteamClient_GetISteamUtils(
    SteamClient, SteamPipeHandle, STEAMUTILS_INTERFACE_VERSION);
  VerifyLoad(FAPIHandle, Self.ClassName);
end;

destructor TSteamUtils.Destroy;
begin

  inherited;
end;

function TSteamUtils.GetSteamBitmap(const ImageHandle: CInt): TSteamBitmap;
var
  ImWidth, ImHeight: Integer;
  R: TSteamBool;
  Buf: Pointer;
  BufSize: Integer;
begin
  Result := nil;
  if ImageHandle = 0 then
      Exit;

  R := SteamAPI_ISteamUtils_GetImageSize(FAPIHandle, ImageHandle, @ImWidth, @ImHeight);
  if R then
    begin
      Result := TSteamBitmap.Create;
      Result.SetImageFormat(ImWidth, ImHeight, 4);
      try
        BufSize := Result.GetImageMemorySize;
        GetMem(Buf, BufSize);
        if SteamAPI_ISteamUtils_GetImageRGBA(FAPIHandle, ImageHandle, Buf, BufSize) then
          begin
            Result.SetSteamImage(Buf);
            Result.IsValid := True;
          end;
      finally
      end;
    end
  else
    WriteLnLog('GetImageSize Failed for %d in GetStreamBitmap', [ImageHandle]);
end;

function TSteamUtils.GetSteamAppID: CInt;
begin
  Result := SteamAPI_ISteamUtils_GetAppID(FAPIHandle);
end;

function TSteamUtils.IPCountry: String;
begin
  Result := String(SteamAPI_ISteamUtils_GetIPCountry(FAPIHandle));
end;

function TSteamUtils.IsOverlayEnabled: Boolean;
begin
  Result := SteamAPI_ISteamUtils_IsOverlayEnabled(FAPIHandle);
end;

function TSteamUtils.IsSteamRunningInVR: Boolean;
begin
  Result := SteamAPI_ISteamUtils_IsSteamRunningInVR(FAPIHandle);
end;

function TSteamUtils.IsRunningOnSteamDeck: Boolean;
begin
  Result := SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck(FAPIHandle);
end;


end.

