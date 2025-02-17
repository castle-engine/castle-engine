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
unit steamapps;

interface

uses Classes, CTypes, SysUtils, SteamTypes, SteamSubsystem;

type
  TIconSize = (IconSmall, IconMedium, IconLarge);

  TSteamApps = class(TSteamSubsystem)
  private
    function GetAppBuildId: Integer;
    function GetLanguage: String;
  public
    constructor Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32); override;
    destructor Destroy; override;
    function IsDlcInstalled(const DlcAppID: TAppId): Boolean;
    property AppBuildId: Integer read GetAppBuildId;
    property Language: String read GetLanguage;
  end;


implementation

uses CastleInternalSteamApi, CastleLog;

{ TSteamApps }

constructor TSteamApps.Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32);
begin
  FAPIHandle := SteamAPI_ISteamClient_GetISteamApps(
    SteamClient, SteamUserHandle, SteamPipeHandle, STEAMAPPS_INTERFACE_VERSION);
  VerifyLoad(FAPIHandle, Self.ClassName);
end;

destructor TSteamApps.Destroy;
begin

  inherited;
end;

function TSteamApps.GetAppBuildId: Integer;
begin
  if not Assigned(FAPIHandle) then
    Raise Exception.CreateFmt('%s : Class not instantiated',[Self.ClassName]);
  Result := SteamAPI_ISteamApps_GetAppBuildId(FAPIHandle);
end;

function TSteamApps.IsDlcInstalled(const DlcAppID: TAppId): Boolean;
begin
  if not Assigned(FAPIHandle) then
    Raise Exception.CreateFmt('%s : Class not instantiated',[Self.ClassName]);
  Result := SteamAPI_ISteamApps_BIsDlcInstalled(FAPIHandle, DlcAppID);
end;

function TSteamApps.GetLanguage: String;
begin
  if not Assigned(FAPIHandle) then
    Raise Exception.CreateFmt('%s : Class not instantiated',[Self.ClassName]);
  Result := String(SteamAPI_ISteamApps_GetCurrentGameLanguage(FAPIHandle));
end;


end.

