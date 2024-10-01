{
  Copyright 2023-2024 Michalis Kamburelis, Eugene Loza, Sérgio Flores (Relfos).

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Steam constants and types. }
unit CastleInternalSteamConstantsAndTypes;

{$I castleconf.inc}

interface

type
  HSteamPipe = Int32;
  HSteamUser = Int32;
  CSteamId = UInt64; // It's a struct but passed as UInt64
  CGameID = UInt64;
  EResult = UInt32;

const
  { Note for now we are forced to use "version-specific" calls to Steam API
    There are version-free calls in Steam API headers, however, those just crash
    (TODO: reason unknown).
    Therefore for now we use a specific version of Steamworks: 1.57 }
  // TODO: can we clean this up?
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient020'; // in isteamclient.h, I don't know how to pull it from there
  STEAMUSER_INTERFACE_VERSION = 'SteamUser023'; // in isteamuser.h
  STEAMUSERSTATS_INTERFACE_VERSION = 'STEAMUSERSTATS_INTERFACE_VERSION012'; // isteamuserstats.h

type
  SteamAPIWarningMessageHook = procedure (nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;

const
  { copy from steam_api_internal.h }
  //-----------------------------------------------------------------------------
  // Purpose: Base values for callback identifiers, each callback must
  //			have a unique ID.
  //-----------------------------------------------------------------------------
  k_iSteamUserCallbacks = 100;
  k_iSteamGameServerCallbacks = 200;
  k_iSteamFriendsCallbacks = 300;
  k_iSteamBillingCallbacks = 400;
  k_iSteamMatchmakingCallbacks = 500;
  k_iSteamContentServerCallbacks = 600;
  k_iSteamUtilsCallbacks = 700;
  k_iSteamAppsCallbacks = 1000;
  k_iSteamUserStatsCallbacks = 1100;
  k_iSteamNetworkingCallbacks = 1200;
  k_iSteamNetworkingSocketsCallbacks = 1220;
  k_iSteamNetworkingMessagesCallbacks = 1250;
  k_iSteamNetworkingUtilsCallbacks = 1280;
  k_iSteamRemoteStorageCallbacks = 1300;
  k_iSteamGameServerItemsCallbacks = 1500;
  k_iSteamGameCoordinatorCallbacks = 1700;
  k_iSteamGameServerStatsCallbacks = 1800;
  k_iSteam2AsyncCallbacks = 1900;
  k_iSteamGameStatsCallbacks = 2000;
  k_iSteamHTTPCallbacks = 2100;
  k_iSteamScreenshotsCallbacks = 2300;
  // NOTE: 2500-2599 are reserved
  k_iSteamStreamLauncherCallbacks = 2600;
  k_iSteamControllerCallbacks = 2800;
  k_iSteamUGCCallbacks = 3400;
  k_iSteamStreamClientCallbacks = 3500;
  k_iSteamAppListCallbacks = 3900;
  k_iSteamMusicCallbacks = 4000;
  k_iSteamMusicRemoteCallbacks = 4100;
  k_iSteamGameNotificationCallbacks = 4400;
  k_iSteamHTMLSurfaceCallbacks = 4500;
  k_iSteamVideoCallbacks = 4600;
  k_iSteamInventoryCallbacks = 4700;
  k_iSteamParentalSettingsCallbacks = 5000;
  k_iSteamGameSearchCallbacks = 5200;
  k_iSteamPartiesCallbacks = 5300;
  k_iSteamSTARCallbacks = 5500;
  k_iSteamRemotePlayCallbacks = 5700;
  k_iSteamChatCallbacks = 5900;

{ callback received structs, from isteamxxxxxxx.h for other callbacks and constants }

implementation

end.

