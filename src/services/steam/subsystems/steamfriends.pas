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
unit steamfriends;

interface

uses Classes, CTypes, SysUtils, SteamTypes, SteamSubsystem;

type
  TSteamFriends = class(TSteamSubsystem)
  strict private
    FEnabled: Boolean;
  private
  public
    constructor Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32); override;
    destructor Destroy; override;
    function GetFriendImageHandle(const FriendID: CUserID;
      const Size: TIconSize = IconLarge): CInt;
  end;

implementation

uses CastleInternalSteamApi, CastleLog;

{ TSteamFriends }

constructor TSteamFriends.Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32);
begin
  FAPIHandle := SteamAPI_ISteamClient_GetISteamFriends(
    SteamClient, SteamUserHandle, SteamPipeHandle, STEAMFRIENDS_INTERFACE_VERSION);
  VerifyLoad(FAPIHandle, Self.ClassName);
end;

destructor TSteamFriends.Destroy;
begin

  inherited;
end;

function TSteamFriends.GetFriendImageHandle(const FriendID: CUserID; const Size: TIconSize): CInt;
begin
  if not Assigned(FAPIHandle) then
    Raise Exception.CreateFmt('%s : Class not instantiated',[Self.ClassName]);

  case Size of
    IconSmall:
      Result := SteamAPI_ISteamFriends_GetSmallFriendAvatar(FAPIHandle, FriendID);
    IconMedium:
      Result := SteamAPI_ISteamFriends_GetMediumFriendAvatar(FAPIHandle, FriendID);
    IconLarge:
      Result := SteamAPI_ISteamFriends_GetLargeFriendAvatar(FAPIHandle, FriendID);
  else
    Result := 0;
  end;
end;


end.

