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
  //CSteamId = UInt64; // It's a struct but passed as UInt64
  //CGameID = UInt64;

type
  SteamAPIWarningMessageHook = procedure (nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;

type
  { copy from steam_api_internal.h }
  //-----------------------------------------------------------------------------
  // Purpose: Base values for callback identifiers, each callback must
  //			have a unique ID.
  //-----------------------------------------------------------------------------
  const k_iSteamUserCallbacks = 100;
  const k_iSteamGameServerCallbacks = 200;
  const k_iSteamFriendsCallbacks = 300;
  const k_iSteamBillingCallbacks = 400;
  const k_iSteamMatchmakingCallbacks = 500;
  const k_iSteamContentServerCallbacks = 600;
  const k_iSteamUtilsCallbacks = 700;
  const k_iSteamAppsCallbacks = 1000;
  const k_iSteamUserStatsCallbacks = 1100;
  const k_iSteamNetworkingCallbacks = 1200;
  const k_iSteamNetworkingSocketsCallbacks = 1220;
  const k_iSteamNetworkingMessagesCallbacks = 1250;
  const k_iSteamNetworkingUtilsCallbacks = 1280;
  const k_iSteamRemoteStorageCallbacks = 1300;
  const k_iSteamGameServerItemsCallbacks = 1500;
  const k_iSteamGameCoordinatorCallbacks = 1700;
  const k_iSteamGameServerStatsCallbacks = 1800;
  const k_iSteam2AsyncCallbacks = 1900;
  const k_iSteamGameStatsCallbacks = 2000;
  const k_iSteamHTTPCallbacks = 2100;
  const k_iSteamScreenshotsCallbacks = 2300;
  // NOTE: 2500-2599 are reserved
  const k_iSteamStreamLauncherCallbacks = 2600;
  const k_iSteamControllerCallbacks = 2800;
  const k_iSteamUGCCallbacks = 3400;
  const k_iSteamStreamClientCallbacks = 3500;
  const k_iSteamAppListCallbacks = 3900;
  const k_iSteamMusicCallbacks = 4000;
  const k_iSteamMusicRemoteCallbacks = 4100;
  const k_iSteamGameNotificationCallbacks = 4400;
  const k_iSteamHTMLSurfaceCallbacks = 4500;
  const k_iSteamVideoCallbacks = 4600;
  const k_iSteamInventoryCallbacks = 4700;
  const k_ISteamParentalSettingsCallbacks = 5000;
  const k_iSteamGameSearchCallbacks = 5200;
  const k_iSteamPartiesCallbacks = 5300;
  const k_iSteamSTARCallbacks = 5500;
  const k_iSteamRemotePlayCallbacks = 5700;
  const k_iSteamChatCallbacks = 5900;

implementation

end.

