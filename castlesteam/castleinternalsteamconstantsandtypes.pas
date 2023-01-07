unit CastleInternalSteamConstantsAndTypes;

{$mode ObjFPC}{$H+}

interface

const
  SteamLib = 'steam_api64';

const
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient020'; // in isteamclient.h, I don't know how to pull it from there
  STEAMUSER_INTERFACE_VERSION = 'SteamUser021'; // in isteamuser.h
  STEAMUSERSTATS_INTERFACE_VERSION = 'STEAMUSERSTATS_INTERFACE_VERSION012'; // isteamuserstats.h

type
  HSteamPipe = Int32;
  HSteamUser = Int32;
  CSteamId = UInt64; // It's a struct but passed as UInt64
  CGameID = UInt64;

type
  SteamAPIWarningMessageHook = procedure (nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;

implementation

end.

