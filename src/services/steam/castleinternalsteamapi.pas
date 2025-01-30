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

{ USE_TESTING_API allows switching between an established tested API
  and a newer API that is being tested. It makes upgrading between two
  APIs far simpler as when defined an exception may be raised when
  loading the library. The 'testing' library should be renamed to
  include it's version as a suffix e.g. steam_api64.dll would be
  renamed to steam_api64_161.dll and STEAMLIBVER, below, set to the matching
  suffix.

  After successful testing the constants below should all be the same
  until a new API upgrade is required - essentially making the define = !define
  and STEAMLIBVER should be an empty string ('')

  II SHOULD NORMALLY NOT BE DEFINED - note also in CastleSteam while testing
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
  CUserID = UInt64;
  EResult = UInt32;
  TAppId = UInt32;
  PSteamErrMsg = PChar;

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
  TCallbackBool = LongBool;
  PCallbackBool = ^TCallbackBool;

const
  { Versions of Steam API interfaces.
    Correspond to Steamworks 1.xx controlled by API_XXX with fallback to 1.57 version. }
{$if defined(USE_TESTING_API)}
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient021'; //< isteamclient.h
  STEAMUSER_INTERFACE_VERSION = 'SteamUser023'; //< isteamuser.h
  STEAMUSERSTATS_INTERFACE_VERSION = 'STEAMUSERSTATS_INTERFACE_VERSION013'; //< isteamuserstats.h
  STEAMFRIENDS_INTERFACE_VERSION = 'SteamFriends017'; //< isteamuserstats.h
  STEAMUTILS_INTERFACE_VERSION = 'SteamUtils010'; //< isteamuser.h
  STEAMINPUT_INTERFACE_VERSION = 'SteamInput006'; //< isteaminput.h
  STEAMAPPS_INTERFACE_VERSION = 'SteamApps008'; //< isteaminput.h
  VersionSteamUtils = '010'; //< matches STEAMUTILS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamApps = '008'; //< matches STEAMAPPS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamUser = '023'; //< matches STEAMUTILS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamFriends = '017'; //< matches STEAMFRIENDS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamInput = '006'; //< matches STEAMINPUT_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  STEAMLIBVER = '_161';
{$else}
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient020'; //< isteamclient.h
  STEAMUSER_INTERFACE_VERSION = 'SteamUser023'; //< isteamuser.h
  STEAMUSERSTATS_INTERFACE_VERSION = 'STEAMUSERSTATS_INTERFACE_VERSION012'; //< isteamuserstats.h
  STEAMFRIENDS_INTERFACE_VERSION = 'SteamFriends017'; //< isteamuserstats.h
  STEAMUTILS_INTERFACE_VERSION = 'SteamUtils010'; //< isteamuser.h
  STEAMINPUT_INTERFACE_VERSION = 'SteamInput006'; //< isteaminput.h
  VersionSteamUtils = '010'; //< matches STEAMUTILS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamApps = '008'; //< matches STEAMAPPS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamUser = '023'; //< matches STEAMUTILS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamFriends = '017'; //< matches STEAMAPPS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamInput = '006'; //< matches accessor in steam_api_flat.h
  STEAMLIBVER = '';
{$endif}

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

  k_cchLeaderboardNameMax = 128;
  k_cchStatNameMax = 128;
  k_cLeaderboardDetailsMax = 64;

type
  TStatName = Array [0..(k_cchStatNameMax - 1)] of AnsiChar;
  PStatName = ^TStatName;

  TSteamAPIInitResult = (	k_ESteamAPIInitResult_OK = 0, // Success
    k_ESteamAPIInitResult_FailedGeneric = 1, // Some other failure
    k_ESteamAPIInitResult_NoSteamClient = 2, // We cannot connect to Steam, steam probably isn't running
    k_ESteamAPIInitResult_VersionMismatch = 3 // Steam client appears to be out of date
  );

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

  // Purpose: called when the latests stats and achievements have been received
  // from the server
  TUserAchievementIconFetched = packed record
  const
    k_iCallback = k_iSteamUserStatsCallbacks + 9;
  var
    // The Game ID this achievement is for.
    m_nGameID: CGameID;
    // The name of the achievement that this callback is for.
    m_rgchAchievementName: TStatName;
    // Returns whether the icon for the achieved (true) or unachieved (false) version.
    m_bAchieved: TCallbackBool;
    // Handle to the image, which can be used with ISteamUtils::GetImageRGBA to get the image data.
    // 0 means no image is set for the achievement.
    m_nIconHandle: Int32;
  end;
  PUserAchievementIconFetched = ^TUserAchievementIconFetched;

  // Pointer to ISteamApps interface from Steam API.
  // For our translation, this is just a pointer to an opaque structure.
  // Defined as a record to be incompatible with e.g. ISteamUtils.
  ISteamApps = record Ptr: Pointer; end;
  ISteamFriends = record Ptr: Pointer; end;
  ISteamUser = record Ptr: Pointer; end;
  ISteamInput = record Ptr: Pointer; end;

  // Pointer to ISteamUtils interface from Steam API.
  ISteamUtils = record Ptr: Pointer; end;

var
  // steam_api.h translation (full documentation at https://partner.steamgames.com/doc/api/steam_api )
  {$if defined(USE_TESTING_API)}
  SteamAPI_InitFlat: function (pOutErrMsg: PSteamErrMsg): TSteamAPIInitResult; CDecl;
  {$else}
  SteamAPI_Init: function (): TSteamBool; CDecl;
  {$endif}
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
  // SteamUtils has no SteamUser
  SteamAPI_ISteamClient_GetISteamUtils: function (SteamClient: Pointer; SteamPipeHandle: HSteamPipe; const SteamUtilsInterfaceVersion: PAnsiChar): Pointer; CDecl;
  // Steam_ISteamClient_GetISteam...... use SteamUser
  SteamAPI_ISteamClient_GetISteamUser:      function (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamUserInterfaceVersion: PAnsiChar): Pointer; CDecl;
  SteamAPI_ISteamClient_GetISteamApps:      function (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamAppsInterfaceVersion: PAnsiChar): Pointer; CDecl;
  SteamAPI_ISteamClient_GetISteamUserStats: function (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamUserStatsInterfaceVersion: PAnsiChar): Pointer; CDecl;
  SteamAPI_ISteamClient_GetISteamFriends:   function (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamFriendsInterfaceVersion: PAnsiChar): Pointer; CDecl;
  SteamAPI_ISteamClient_GetISteamInput:     function (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamInputInterfaceVersion: PAnsiChar): Pointer; CDecl;

  // ISteamUserStats
  {$if not defined(USE_TESTING_API)}
  SteamAPI_ISteamUserStats_RequestCurrentStats: function (SteamUserStats: Pointer): TSteamBool; CDecl;
  {$endif}

  SteamAPI_ISteamUserStats_GetAchievement: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar; Achieved: PSteamBool): TSteamBool; CDecl;
  SteamAPI_ISteamUserStats_SetAchievement: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar): TSteamBool; CDecl;
  SteamAPI_ISteamUserStats_ClearAchievement: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar): TSteamBool; CDecl;
  SteamAPI_ISteamUserStats_GetNumAchievements: function (SteamUserStats: Pointer): UInt32; CDecl;
  // Returns string-ID of the achievement, not a human readable name
  SteamAPI_ISteamUserStats_GetAchievementName: function (SteamUserStats: Pointer; AchievementId: UInt32 ): PAnsiChar; CDecl;
  // Returns attribute of the achievement, AchievementKey may be name, desc or hidden which return UTF8 string with "0" or "1" indicating hidden state
  SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar; const AchievementKey: PAnsiChar ): PAnsiChar; CDecl;
  // Returns whether the achievement has been completed and the Date/Time of completion if Achieved = True
  SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar; const Achieved: PSteamBool; UnlockTime: PUInt32): TSteamBool; CDecl;
  // Returns a handle for the Achievement's image - needs further processing via callback
  SteamAPI_ISteamUserStats_GetAchievementIcon: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar): UInt32; CDecl;
  // Show Steam popup "achievement : 30/100", see https://partner.steamgames.com/doc/api/ISteamUserStats#IndicateAchievementProgress
  SteamAPI_ISteamUserStats_IndicateAchievementProgress: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar; CurrentProgress: UInt32; MaxProgress: UInt32): TSteamBool; CDecl;
  // Call this after changing stats or achievements
  SteamAPI_ISteamUserStats_StoreStats: function (SteamUserStats: Pointer): TSteamBool; CDecl;

  // ISteamInput
  // A versioned accessor is exported by the library
  SteamAPI_SteamInput: function (): ISteamInput; CDecl;


  // ISteamUtils
  // A versioned accessor is exported by the library
  // SteamAPI_SteamUtils_v<VersionSteamUtils>: function (): ISteamUtils; CDecl;
  // Unversioned accessor to get the current version.
  // In Pascal translation, this is just an alias to 'SteamAPI_SteamUtils_v' + VersionSteamUtils.
  SteamAPI_SteamUtils: function (): ISteamUtils; CDecl;
  // Returns the Raw Bitmap Data in pubDest of image Handle iImage. Must call GetImageSize
  // before calling thius in order to allocate memory for buffer that will be filled
  // the destination buffer size should be 4 * height * width * sizeof(char)
  SteamAPI_ISteamUtils_GetImageRGBA: function (Self: Pointer; iImage: CInt; pubDest: PByte; nDestBufferSize: Int32): TSteamBool; CDecl;
  // Returns the Width + Height of image Handle iImage - bust be called bnefore GetImageRGBA
  SteamAPI_ISteamUtils_GetImageSize: function (Self: Pointer; iImage: CInt; pnWidth: PUInt32; pnHeight: PUInt32): TSteamBool; CDecl;
  // returns the 2 digit ISO 3166-1-alpha-2 format country code this client
  // is running in (as looked up via an IP-to-location database) e.g "US" or "UK".
  SteamAPI_ISteamUtils_GetIPCountry: function (Self: Pointer): PAnsiChar; CDecl;
  // Returns true if the overlay is running & the user can access it. The overlay process could take a few seconds to
  // start & hook the game process, so this function will initially return false while the overlay is loading.
  SteamAPI_ISteamUtils_IsOverlayEnabled: function (Self: Pointer): TSteamBool; CDecl;
  // returns true if Steam itself is running in VR mode
  SteamAPI_ISteamUtils_IsSteamRunningInVR: function (Self: Pointer): TSteamBool; CDecl;
  // returns true if currently running on the Steam Deck device
  SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck: function (Self: Pointer): TSteamBool; CDecl;

  // ISteamApps
  // A versioned accessor is exported by the library
  // SteamAPI_SteamApps_v<VersionSteamApps>: function (): ISteamApps; CDecl;
  // Unversioned accessor to get the current version.
  // In Pascal translation, this is just an alias to 'SteamAPI_SteamApps_v' + VersionSteamApps.
  SteamAPI_SteamApps: function (): ISteamApps; CDecl;
  // return the buildid of this app, may change at any time based on backend updates to the game
  SteamAPI_ISteamApps_GetAppBuildId: function (Self: ISteamApps): CInt; CDecl;
  // Takes AppID of DLC and checks if the user owns the DLC & if the DLC is installed
  SteamAPI_ISteamApps_BIsDlcInstalled: function (Self: ISteamApps; AppID: TAppId): TSteamBool; CDecl;
  // returns the current game language
  SteamAPI_ISteamApps_GetCurrentGameLanguage: function (Self: ISteamApps): PAnsiChar; CDecl;

  // ISteamFriends
  // A versioned accessor is exported by the library
  // SteamAPI_SteamFriends_v<VersionSteamFriends>: function (): ISteamFriends; CDecl;
  // Unversioned accessor to get the current version.
  // In Pascal translation, this is just an alias to 'SteamAPI_SteamFriends_v' + VersionSteamApps.
  SteamAPI_SteamFriends: function (): ISteamFriends; CDecl;
  // Returns Handle to Large Friend Avatar Image for use with SteamAPI_ISteamUtils_GetImageRGBA
  SteamAPI_ISteamFriends_GetLargeFriendAvatar: function (Self: Pointer; steamIDFriend: CUserID): CInt; CDecl;
  // Returns Handle to Medium Friend Avatar Image for use with SteamAPI_ISteamUtils_GetImageRGBA
  SteamAPI_ISteamFriends_GetMediumFriendAvatar: function (Self: Pointer; steamIDFriend: CUserID): CInt; CDecl;
  // Returns Handle to Small Friend Avatar Image for use with SteamAPI_ISteamUtils_GetImageRGBA
  SteamAPI_ISteamFriends_GetSmallFriendAvatar: function (Self: Pointer; steamIDFriend: CUserID): CInt; CDecl;

  // ISteamUser
  // A versioned accessor is exported by the library
  SteamAPI_SteamUser: function (): ISteamUser; CDecl;
  // Returns Steam User ID of currently logged in user
  SteamAPI_ISteamUser_GetSteamID: function (Self: Pointer): CUserID; CDecl;

var
  SteamLibrary: TDynLib;

const
  SteamLibraryName =
    {$if defined(DARWIN)} // macOS
    'libsteam_api' + STEAMLIBVER + '.dylib'
    {$elseif defined(UNIX)}
    'libsteam_api' + STEAMLIBVER + '.so'
    {$elseif defined(MSWINDOWS) and defined(CPUX64)}
    'steam_api64' + STEAMLIBVER + '.dll'
    {$elseif defined(MSWINDOWS) and defined(CPUX86)}
    'steam_api' + STEAMLIBVER + '.dll'
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
  {$if defined(USE_TESTING_API)}
  Pointer({$ifndef FPC}@{$endif} SteamAPI_InitFlat) := nil;
  {$else}
  Pointer({$ifndef FPC}@{$endif} SteamAPI_Init) := nil;
  {$endif}
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
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamApps) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUserStats) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamFriends) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUtils) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamInput) := nil;
  {$if not defined(USE_TESTING_API)}
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_RequestCurrentStats) := nil;
  {$endif}
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievement) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_SetAchievement) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_ClearAchievement) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetNumAchievements) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementName) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementIcon) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_IndicateAchievementProgress) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_StoreStats) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamUtils) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_GetImageRGBA) := Nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_GetImageSize) := Nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_GetIPCountry) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsOverlayEnabled) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsSteamRunningInVR) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamApps) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_GetAppBuildId) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_BIsDlcInstalled) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_GetCurrentGameLanguage) := nil;

  Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamUser) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUser_GetSteamID) := nil;

  Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamFriends) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamFriends_GetLargeFriendAvatar) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamFriends_GetMediumFriendAvatar) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamFriends_GetSmallFriendAvatar) := nil;

  FreeAndNil(SteamLibrary);
end;

procedure InitializeSteamLibrary;
begin
  FinalizeSteamLibrary;

  if SteamLibraryName <> '' then
    SteamLibrary := TDynLib.Load(SteamLibraryName, false);

  if SteamLibrary <> nil then
  begin
    {$if defined(USE_TESTING_API)}
    Pointer({$ifndef FPC}@{$endif} SteamAPI_InitFlat) := SteamLibrary.Symbol('SteamAPI_InitFlat');
    {$else}
    Pointer({$ifndef FPC}@{$endif} SteamAPI_Init) := SteamLibrary.Symbol('SteamAPI_Init');
    {$endif}
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
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamApps) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamApps');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUserStats) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamUserStats');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamFriends) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamFriends');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUtils) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamUtils');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamInput) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamInput');
    {$if not defined(USE_TESTING_API)}
    // RequestCurrentStats removeded in 1.61
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_RequestCurrentStats) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_RequestCurrentStats');
    {$endif}
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievement) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievement');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_SetAchievement) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_SetAchievement');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_ClearAchievement) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_ClearAchievement');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetNumAchievements) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetNumAchievements');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementName) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementName');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementIcon) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementIcon');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_IndicateAchievementProgress) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_IndicateAchievementProgress');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_StoreStats) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_StoreStats');
    // alias to versioned entry point
    Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamUtils) := SteamLibrary.Symbol('SteamAPI_SteamUtils_v' + VersionSteamUtils);
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_GetImageRGBA) := SteamLibrary.Symbol('SteamAPI_ISteamUtils_GetImageRGBA');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_GetImageSize) := SteamLibrary.Symbol('SteamAPI_ISteamUtils_GetImageSize');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_GetIPCountry) := SteamLibrary.Symbol('SteamAPI_ISteamUtils_GetIPCountry');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsOverlayEnabled) := SteamLibrary.Symbol('SteamAPI_ISteamUtils_IsOverlayEnabled');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsSteamRunningInVR) := SteamLibrary.Symbol('SteamAPI_ISteamUtils_IsSteamRunningInVR');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck) := SteamLibrary.Symbol('SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck');
    // alias to versioned entry point
    Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamApps) := SteamLibrary.Symbol('SteamAPI_SteamApps_v' + VersionSteamApps);
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_GetAppBuildId) := SteamLibrary.Symbol('SteamAPI_ISteamApps_GetAppBuildId');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_BIsDlcInstalled) := SteamLibrary.Symbol('SteamAPI_ISteamApps_BIsDlcInstalled');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamApps_GetCurrentGameLanguage) := SteamLibrary.Symbol('SteamAPI_ISteamApps_GetCurrentGameLanguage');
    // alias to versioned entry point
    Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamUser) := SteamLibrary.Symbol('SteamAPI_SteamUser_v' + VersionSteamUser);
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUser_GetSteamID) := SteamLibrary.Symbol('SteamAPI_ISteamUser_GetSteamID');
    // alias to versioned entry point
    Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamFriends) := SteamLibrary.Symbol('SteamAPI_SteamFriends_v' + VersionSteamFriends);
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamFriends_GetLargeFriendAvatar) := SteamLibrary.Symbol('SteamAPI_ISteamFriends_GetLargeFriendAvatar');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamFriends_GetMediumFriendAvatar) := SteamLibrary.Symbol('SteamAPI_ISteamFriends_GetMediumFriendAvatar');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamFriends_GetSmallFriendAvatar) := SteamLibrary.Symbol('SteamAPI_ISteamFriends_GetSmallFriendAvatar');
    // alias to versioned entry point
    Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamInput) := SteamLibrary.Symbol('SteamAPI_SteamInput_v' + VersionSteamInput);
  end;
end;

Initialization
Finalization
  FinalizeSteamLibrary;

end.

