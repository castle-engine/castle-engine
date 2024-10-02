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

{$ifdef FPC}
  {$packrecords C}
{$else}
  {$ALIGN 4}
{$endif}

interface

uses CTypes;

type
  HSteamPipe = Int32;
  HSteamUser = Int32;
  { It's a struct in C headers but can be passed as UInt64,
    defined in C headers with explicit statement that it's 64-bit sized. }
  CSteamId = UInt64;
  { It's a struct in C headers but can be passed as UInt64,
    defined in C headers with explicit ability to be typecasted as 64-bit int. }
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

  { Internal structure used in manual callback dispatch
    (CallbackMsg_t in C headers). }
  TCallbackMsg = record
    // Specific user to whom this callback applies.
	  m_hSteamUser: HSteamUser;
    // Callback identifier.  (Corresponds to the k_iCallback enum in the callback structure.)
	  m_iCallback: CInt;
    // Points to the callback structure
	  m_pubParam: Puint8;
    // Size of the data pointed to by m_pubParam
	  m_cubParam: CInt;
  end;
  PCallbackMsg = ^TCallbackMsg;

  // handle to a Steam API call
  TSteamAPICall = UInt64;

const
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

  k_uAPICallInvalid = TSteamAPICall(0);

type
  // Purpose: called when a SteamAsyncCall_t has completed (or failed)
  TSteamAPICallCompleted = record
  const
    k_iCallback = k_iSteamUtilsCallbacks + 3;
  var
    m_hAsyncCall: TSteamAPICall;
    m_iCallback: CInt;
    m_cubParam: UInt32;
  end;
  PSteamAPICallCompleted = ^TSteamAPICallCompleted;

  // Purpose: called when the latests stats and achievements have been received
  // from the server
  TUserStatsReceived = record
  const
    k_iCallback = k_iSteamUserStatsCallbacks + 1;
  var
    // Game these stats are for
		GameID: CGameID;
    // Success / error fetching the stats
		Result: EResult;
    // The user for whom the stats are retrieved for
		SteamID: CSteamId;
	end;
  PUserStatsReceived = ^TUserStatsReceived;

implementation

end.

