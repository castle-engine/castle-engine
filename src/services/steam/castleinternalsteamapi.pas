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
{ Steam headers translated to Pascal.

  We expose only a subset of Steam API that we currently need and tested --
  initialization, manual callbacks dispatchibg, achievements, some more.
  Much more work is coming, to enable more Steam features through our
  integration, which will also make this unit more complete.

  See https://castle-engine.io/steam for documentation how to use Steam
  with Castle Game Engine applications.
  Normal game developers (using Castle Game Engine to develop games)
  should not used this unit directly. Instead, use higher-level CastleSteam unit.

  Engine developers looking to extend Steam integration should look at this
  unit and how CastleSteam unit uses it.
  See https://partner.steamgames.com/doc/sdk/api for full Steam API documentation.

  Credits:

  - Sérgio Flores (Relfos) and https://github.com/Relfos/steamworks_wrappers/

    We did something different in our (CGE) integration, to deliberately avoid
    the need for any additional "wrapper" libraries.
    We also use "manual dispatching" of Steam callbacks, which is a bit less
    hacky.
    Still, we were able to do these improvements thanks to the work of
    Sérgio Flores, looking how Steam can be used at all in Pascal.

  - We note invaluable help that Apus Engine's sources
    by Ivan Polyacov ( https://github.com/Cooler2/ApusGameEngine )
    have provided in hunting down the specific calls
    that work and a few tricks to make them work properly.
}
unit CastleInternalSteamApi;

{$I castleconf.inc}

{$ifdef FPC}
  {$packrecords C}
{$else}
  {$ALIGN 4}
{$endif}

interface

uses
  CTypes, CastleDynLib;

{ Constants and types. }

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
  TAppId = UInt32;

  { Express all "bool" from Steam API using this type.

    Note that CBool, equal to LongBool, would be wrong.
    Symptoms:
    - SteamAPI_RestartAppIfNecessary result is then considered always
      "true" on Windows (but not on Linux).
    - SteamAPI_ISteamUserStats_GetAchievement (has a pointer to modify "achieved"
      status) behaves than as if all achievements are always achieved.
      (on Windows and Linux).

    Testing, it seems for Steam API, bool is 1 byte, not 4 bytes.
    So ByteBool is reliable.
    Pascal's Boolean also happens to work, but it's not guaranteed how it behaves
    for non-1 values. }
  TSteamBool = ByteBool;
  PSteamBool = ^TSteamBool;

const
  { Versions of Steam API interfaces.
    Correspond to Steamworks 1.57 version. }
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient020'; //< isteamclient.h
  STEAMUSER_INTERFACE_VERSION = 'SteamUser023'; //< isteamuser.h
  STEAMUSERSTATS_INTERFACE_VERSION = 'STEAMUSERSTATS_INTERFACE_VERSION012'; //< isteamuserstats.h
  VersionSteamUtils = '010'; //< matches STEAMUTILS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamApps = '008'; //< matches STEAMAPPS_INTERFACE_VERSION *and* accessor in steam_api_flat.h

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
    // (C headers literal translation would be Puint8, but it's pointless,
    // this is not pointer to UInt8, this is a pointer to callback-specific blob).
    m_pubParam: Pointer;
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

  // Pointer to ISteamApps interface from Steam API.
  // For our translation, this is just a pointer to an opaque structure.
  // Defined as a record to be incompatible with e.g. ISteamUtils.
  ISteamApps = record Ptr: Pointer; end;

  // Pointer to ISteamUtils interface from Steam API.
  ISteamUtils = record Ptr: Pointer; end;

var
  // steam_api.h translation (full documentation at https://partner.steamgames.com/doc/api/steam_api )
  SteamAPI_Init: function (): TSteamBool; CDecl;
  SteamAPI_ReleaseCurrentThreadMemory: procedure (); CDecl; // TODO: UNTESTED
  SteamAPI_RestartAppIfNecessary: function (unOwnAppID: TAppId): TSteamBool; CDecl;
  SteamAPI_RunCallbacks: procedure (); CDecl;
  SteamAPI_Shutdown: procedure (); CDecl;
  SteamAPI_ManualDispatch_RunFrame: procedure (SteamPipe: HSteamPipe); CDecl;
  SteamAPI_ManualDispatch_Init: procedure (); CDecl;
  SteamAPI_ManualDispatch_GetNextCallback: function (SteamPipe: HSteamPipe; pCallbackMsg: PCallbackMsg): TSteamBool; CDecl;
  SteamAPI_ManualDispatch_FreeLastCallback: procedure (SteamPipe: HSteamPipe); CDecl;
  SteamAPI_ManualDispatch_GetAPICallResult: function (SteamPipe: HSteamPipe; hSteamAPICall: TSteamAPICall; pCallback: Pointer; cubCallback: CInt; iCallbackExpected: CInt; pbFailed: PSteamBool): TSteamBool; CDecl;

  // steam_api_internal.h translation
  SteamInternal_CreateInterface: function (SteamClientInterfaceVersion: PAnsiChar): Pointer; CDecl;
  SteamAPI_GetHSteamUser: function (): HSteamUser; CDecl;
  SteamAPI_GetHSteamPipe: function (): HSteamPipe; CDecl;
  SteamAPI_RegisterCallback: procedure (pCallback: Pointer; iCallback: Integer); CDecl;
  SteamAPI_UnregisterCallback: procedure (pCallback: Pointer); CDecl;

  // steam_api_flat.h translation below,
  // which is actually "flat" (C, no classes) Steam API corresponding to the C++ API

  // ISteamClient
  SteamAPI_ISteamClient_SetWarningMessageHook: procedure (SteamClient: Pointer; WarningMessageHook: SteamAPIWarningMessageHook); CDecl;
  SteamAPI_ISteamClient_GetISteamUser: function (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamUserInterfaceVersion: PAnsiChar): Pointer; CDecl;
  SteamAPI_ISteamClient_GetISteamUserStats: function (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamUserStatsInterfaceVersion: PAnsiChar): Pointer; CDecl;

  // ISteamUserStats
  SteamAPI_ISteamUserStats_RequestCurrentStats: function (SteamUserStats: Pointer): TSteamBool; CDecl;
  SteamAPI_ISteamUserStats_GetAchievement: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar; Achieved: PSteamBool): TSteamBool; CDecl;
  SteamAPI_ISteamUserStats_SetAchievement: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar): TSteamBool; CDecl;
  SteamAPI_ISteamUserStats_ClearAchievement: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar): TSteamBool; CDecl;
  SteamAPI_ISteamUserStats_GetNumAchievements: function (SteamUserStats: Pointer): UInt32; CDecl;
  // It returns string-ID of the achievement, not a human readable name
  SteamAPI_ISteamUserStats_GetAchievementName: function (SteamUserStats: Pointer; AchievementId: UInt32 ): PAnsiChar; CDecl;
  // Show Steam popup "achievement : 30/100", see https://partner.steamgames.com/doc/api/ISteamUserStats#IndicateAchievementProgress
  SteamAPI_ISteamUserStats_IndicateAchievementProgress: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar; CurrentProgress: UInt32; MaxProgress: UInt32): TSteamBool; CDecl;
  // Call this after changing stats or achievements
  SteamAPI_ISteamUserStats_StoreStats: function (SteamUserStats: Pointer): TSteamBool; CDecl;

  // ISteamUtils
  // A versioned accessor is exported by the library
  //SteamAPI_SteamUtils_v<VersionSteamUtils>: function (): ISteamUtils; CDecl;
  // Unversioned accessor to get the current version.
  // In Pascal translation, this is just an alias to 'SteamAPI_SteamUtils_v' + VersionSteamUtils.
  SteamAPI_SteamUtils: function (): ISteamUtils; CDecl;
  // returns the 2 digit ISO 3166-1-alpha-2 format country code this client
  // is running in (as looked up via an IP-to-location database) e.g "US" or "UK".
  SteamAPI_ISteamUtils_GetIPCountry: function (Self: ISteamUtils): PAnsiChar; CDecl;
  // Returns true if the overlay is running & the user can access it. The overlay process could take a few seconds to
  // start & hook the game process, so this function will initially return false while the overlay is loading.
  SteamAPI_ISteamUtils_IsOverlayEnabled: function (Self: ISteamUtils): TSteamBool; CDecl;
  // returns true if Steam itself is running in VR mode
  SteamAPI_ISteamUtils_IsSteamRunningInVR: function (Self: ISteamUtils): TSteamBool; CDecl;
  // returns true if currently running on the Steam Deck device
  SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck: function (Self: ISteamUtils): TSteamBool; CDecl;

  // ISteamApps
  // A versioned accessor is exported by the library
  //SteamAPI_SteamApps_v<VersionSteamApps>: function (): ISteamApps; CDecl;
  // Unversioned accessor to get the current version.
  // In Pascal translation, this is just an alias to 'SteamAPI_SteamApps_v' + VersionSteamApps.
  SteamAPI_SteamApps: function (): ISteamApps; CDecl;
  // return the buildid of this app, may change at any time based on backend updates to the game
  SteamAPI_ISteamApps_GetAppBuildId: function (Self: ISteamApps): CInt; CDecl;
  // Takes AppID of DLC and checks if the user owns the DLC & if the DLC is installed
  SteamAPI_ISteamApps_BIsDlcInstalled: function (Self: ISteamApps; AppID: TAppId): TSteamBool; CDecl;
  // returns the current game language
  SteamAPI_ISteamApps_GetCurrentGameLanguage: function (Self: ISteamApps): PAnsiChar; CDecl;

var
  SteamLibrary: TDynLib;

const
  SteamLibraryName =
    {$if defined(DARWIN)} // macOS
    'libsteam_api.dylib'
    {$elseif defined(UNIX)}
    'libsteam_api.so'
    {$elseif defined(MSWINDOWS) and defined(CPUX64)}
    'steam_api64.dll'
    {$elseif defined(MSWINDOWS) and defined(CPUX86)}
    'steam_api.dll'
    {$else}
    // Steam library not available on this platform
    ''
    {$endif};

procedure InitializeSteamLibrary;
procedure FinalizeSteamLibrary;

implementation

uses
  SysUtils;

procedure FinalizeSteamLibrary;
begin
  Pointer({$ifndef FPC}@{$endif} SteamAPI_Init) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ReleaseCurrentThreadMemory) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_RestartAppIfNecessary) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_RunCallbacks) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_Shutdown) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ManualDispatch_RunFrame) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ManualDispatch_Init) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ManualDispatch_GetNextCallback) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ManualDispatch_FreeLastCallback) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ManualDispatch_GetAPICallResult) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamInternal_CreateInterface) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_GetHSteamUser) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_GetHSteamPipe) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_RegisterCallback) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_UnregisterCallback) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_SetWarningMessageHook) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUser) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUserStats) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_RequestCurrentStats) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievement) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_SetAchievement) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_ClearAchievement) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetNumAchievements) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementName) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_IndicateAchievementProgress) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_StoreStats) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamUtils) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_GetIPCountry) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsOverlayEnabled) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsSteamRunningInVR) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamApps) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_GetAppBuildId) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_BIsDlcInstalled) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_GetCurrentGameLanguage) := nil;

  FreeAndNil(SteamLibrary);
end;

procedure InitializeSteamLibrary;
begin
  FinalizeSteamLibrary;

  if SteamLibraryName <> '' then
    SteamLibrary := TDynLib.Load(SteamLibraryName, false);

  if SteamLibrary <> nil then
  begin
    Pointer({$ifndef FPC}@{$endif} SteamAPI_Init) := SteamLibrary.Symbol('SteamAPI_Init');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ReleaseCurrentThreadMemory) := SteamLibrary.Symbol('SteamAPI_ReleaseCurrentThreadMemory');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_RestartAppIfNecessary) := SteamLibrary.Symbol('SteamAPI_RestartAppIfNecessary');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_RunCallbacks) := SteamLibrary.Symbol('SteamAPI_RunCallbacks');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_Shutdown) := SteamLibrary.Symbol('SteamAPI_Shutdown');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ManualDispatch_RunFrame) := SteamLibrary.Symbol('SteamAPI_ManualDispatch_RunFrame');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ManualDispatch_Init) := SteamLibrary.Symbol('SteamAPI_ManualDispatch_Init');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ManualDispatch_GetNextCallback) := SteamLibrary.Symbol('SteamAPI_ManualDispatch_GetNextCallback');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ManualDispatch_FreeLastCallback) := SteamLibrary.Symbol('SteamAPI_ManualDispatch_FreeLastCallback');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ManualDispatch_GetAPICallResult) := SteamLibrary.Symbol('SteamAPI_ManualDispatch_GetAPICallResult');
    Pointer({$ifndef FPC}@{$endif} SteamInternal_CreateInterface) := SteamLibrary.Symbol('SteamInternal_CreateInterface');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_GetHSteamUser) := SteamLibrary.Symbol('SteamAPI_GetHSteamUser');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_GetHSteamPipe) := SteamLibrary.Symbol('SteamAPI_GetHSteamPipe');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_RegisterCallback) := SteamLibrary.Symbol('SteamAPI_RegisterCallback');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_UnregisterCallback) := SteamLibrary.Symbol('SteamAPI_UnregisterCallback');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_SetWarningMessageHook) := SteamLibrary.Symbol('SteamAPI_ISteamClient_SetWarningMessageHook');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUser) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamUser');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUserStats) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamUserStats');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_RequestCurrentStats) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_RequestCurrentStats');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievement) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievement');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_SetAchievement) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_SetAchievement');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_ClearAchievement) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_ClearAchievement');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetNumAchievements) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetNumAchievements');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementName) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementName');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_IndicateAchievementProgress) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_IndicateAchievementProgress');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_StoreStats) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_StoreStats');
    // alias to versioned entry point
    Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamUtils) := SteamLibrary.Symbol('SteamAPI_SteamUtils_v' + VersionSteamUtils);
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_GetIPCountry) := SteamLibrary.Symbol('SteamAPI_ISteamUtils_GetIPCountry');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsOverlayEnabled) := SteamLibrary.Symbol('SteamAPI_ISteamUtils_IsOverlayEnabled');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsSteamRunningInVR) := SteamLibrary.Symbol('SteamAPI_ISteamUtils_IsSteamRunningInVR');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck) := SteamLibrary.Symbol('SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck');
    // alias to versioned entry point
    Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamApps) := SteamLibrary.Symbol('SteamAPI_SteamApps_v' + VersionSteamApps);
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_GetAppBuildId) := SteamLibrary.Symbol('SteamAPI_ISteamApps_GetAppBuildId');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_BIsDlcInstalled) := SteamLibrary.Symbol('SteamAPI_ISteamApps_BIsDlcInstalled');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_GetCurrentGameLanguage) := SteamLibrary.Symbol('SteamAPI_ISteamApps_GetCurrentGameLanguage');
  end;
end;

end.

