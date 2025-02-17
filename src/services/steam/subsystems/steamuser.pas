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

unit steamuser;
{ Integration with Steam.
  See @url(https://castle-engine.io/steam Steam and Castle Game Engine documentation)
  for usage. }

interface

uses Classes, CTypes, SysUtils, SteamTypes, SteamSubsystem;

type
  TSteamUser = class(TSteamSubsystem)
  private
    function GetSteamID: CUserID;
  public
    constructor Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32); override;
    destructor Destroy; override;
    property SteamID: CUserID read GetSteamID;
  end;

implementation

uses CastleInternalSteamApi, CastleLog;

{ TSteamUser }

constructor TSteamUser.Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32);
begin
  FAPIHandle := SteamAPI_ISteamClient_GetISteamUser(
    SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSER_INTERFACE_VERSION);
  VerifyLoad(FAPIHandle, Self.ClassName);
end;

destructor TSteamUser.Destroy;
begin

  inherited;
end;

function TSteamUser.GetSteamID: CUserID;
begin
  Result := SteamAPI_ISteamUser_GetSteamID(FAPIHandle);
end;

end.

