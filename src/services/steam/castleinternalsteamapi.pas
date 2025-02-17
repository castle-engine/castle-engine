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
  CTypes, CastleDynLib, SteamTypes;

{ Constants and types. }

type
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
  PInt32 = ^CInt;

type
  SteamAPIWarningMessageHook = procedure (nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;
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
  k_cLeaderboardDetailsMax = 64;

type
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

  // Pointer to ISteamApps interface from Steam API.
  // For our translation, this is just a pointer to an opaque structure.
  // Defined as a record to be incompatible with e.g. ISteamUtils.
  ISteamApps = record Ptr: Pointer; end;
  ISteamFriends = record Ptr: Pointer; end;
  ISteamUser = record Ptr: Pointer; end;
  ISteamUtils = record Ptr: Pointer; end;

type

  // Enums
  EControllerHapticLocation = (
    k_EControllerHapticLocation_Left = 1,
    k_EControllerHapticLocation_Right = 2,
    k_EControllerHapticLocation_Both = 3
  );
  EControllerHapticType = (
    k_EControllerHapticType_Off = 0,
    k_EControllerHapticType_Tick = 1,
    k_EControllerHapticType_Click = 2
  );
  ESteamControllerPad = (
    k_ESteamControllerPad_Left = 0,
    k_ESteamControllerPad_Right = 1
  );

const
  STEAM_INPUT_MIN_ANALOG_ACTION_DATA = -1.0;
  STEAM_INPUT_MAX_ANALOG_ACTION_DATA = 1.0;
  STEAM_INPUT_MAX_COUNT = 16;
  STEAM_INPUT_MAX_ANALOG_ACTIONS = 24;
  STEAM_INPUT_MAX_DIGITAL_ACTIONS = 256;
  STEAM_INPUT_MAX_ORIGINS = 8;
  STEAM_INPUT_MAX_ACTIVE_LAYERS = 16;
{ When sending an option to a specific controller handle, you can send to all devices via this command }
  STEAM_INPUT_HANDLE_ALL_CONTROLLERS : UInt64 = High(UInt64);

{$I isteamdualsense.inc }

type
    PSteamInputAnalogActionData = ^TSteamInputAnalogActionData;
    TSteamInputAnalogActionData = record
        eMode : EInputSourceMode;
        x : Single;
        y : Single;
        bActive : LongBool;
      end;

    PSteamInputDigitalActionData = ^TSteamInputDigitalActionData;
    TSteamInputDigitalActionData = record
        bState : longbool;
        bActive : longbool;
      end;

    PSteamInputMotionData = ^TSteamInputMotionData;
    TSteamInputMotionData = record
        rotQuatX : single;
        rotQuatY : single;
        rotQuatZ : single;
        rotQuatW : single;
        posAccelX : single;
        posAccelY : single;
        posAccelZ : single;
        rotVelX : single;
        rotVelY : single;
        rotVelZ : single;
      end;

var
  // steam_api.h translation (full documentation at https://partner.steamgames.com/doc/api/steam_api )
  {$if STEAM_API_VERSION >= 1.61}
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
  SteamAPI_ISteamClient_BReleaseSteamPipe:     function  (SteamClient: Pointer; hSteamPipe: HSteamPipe): LongBool; CDecl;
  SteamAPI_ISteamClient_ReleaseUser:           procedure (SteamClient: Pointer; hSteamPipe: HSteamPipe; hSteamUser: HSteamUser); CDecl;
  SteamAPI_ISteamClient_SetWarningMessageHook: procedure (SteamClient: Pointer; WarningMessageHook: SteamAPIWarningMessageHook); CDecl;
  // SteamUtils has no SteamUser
  SteamAPI_ISteamClient_GetISteamUtils:        function  (SteamClient: Pointer; SteamPipeHandle: HSteamPipe; const SteamUtilsInterfaceVersion: PAnsiChar): Pointer; CDecl;
  // Steam_ISteamClient_GetISteam...... use SteamClient pointer
  SteamAPI_ISteamClient_GetISteamApps:         function  (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamAppsInterfaceVersion: PAnsiChar): Pointer; CDecl;
  SteamAPI_ISteamClient_GetISteamFriends:      function  (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamFriendsInterfaceVersion: PAnsiChar): Pointer; CDecl;
  SteamAPI_ISteamClient_GetISteamInput:        function  (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamInputInterfaceVersion: PAnsiChar): Pointer; CDecl;
  SteamAPI_ISteamClient_GetISteamUser:         function  (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamUserInterfaceVersion: PAnsiChar): Pointer; CDecl;
  SteamAPI_ISteamClient_GetISteamUserStats:    function  (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamUserStatsInterfaceVersion: PAnsiChar): Pointer; CDecl;

  // ISteamUserStats
  SteamAPI_ISteamUserStats_ClearAchievement:                  function  (SteamUserStats: Pointer; const AchievementName: PAnsiChar): TSteamBool; CDecl;
  SteamAPI_ISteamUserStats_GetAchievement:                    function  (SteamUserStats: Pointer; const AchievementName: PAnsiChar; Achieved: PSteamBool): TSteamBool; CDecl;
  SteamAPI_ISteamUserStats_GetAchievementAchievedPercent:     function  (SteamUserStats: Pointer; const AchievementName: PAnsiChar; pflPercent: PSingle): TSteamBool; CDecl;
  // Returns whether the achievement has been completed and the Date/Time of completion if Achieved = True
  SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime:       function  (SteamUserStats: Pointer; const AchievementName: PAnsiChar; const Achieved: PSteamBool; UnlockTime: PUInt32): TSteamBool; CDecl;
  // Returns attribute of the achievement, AchievementKey may be name, desc or hidden which return UTF8 string with "0" or "1" indicating hidden state
  SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute:    function  (SteamUserStats: Pointer; const AchievementName: PAnsiChar; const AchievementKey: PAnsiChar ): PAnsiChar; CDecl;
  // Returns a handle for the Achievement's image - needs further processing via callback
  SteamAPI_ISteamUserStats_GetAchievementIcon:                function  (SteamUserStats: Pointer; const AchievementName: PAnsiChar): UInt32; CDecl;
  // Returns string-ID of the achievement, not a human readable name
  SteamAPI_ISteamUserStats_GetAchievementName:                function  (SteamUserStats: Pointer; AchievementId: UInt32 ): PAnsiChar; CDecl;
  // Not Documented but in headers
  SteamAPI_ISteamUserStats_GetAchievementProgressLimitsFloat: function  (SteamUserStats: Pointer; const AchievementName: PAnsiChar; const pnMinProgress: PSingle; const pnMaxProgress: PSingle): TSteamBool; CDecl;
  // Not Documented but in headers
  SteamAPI_ISteamUserStats_GetAchievementProgressLimitsInt32: function  (SteamUserStats: Pointer; const AchievementName: PAnsiChar; const pnMinProgress: PInt32; const pnMaxProgress: PInt32): TSteamBool; CDecl;

  SteamAPI_ISteamUserStats_GetNumAchievements:                function  (SteamUserStats: Pointer): UInt32; CDecl;
  // Show Steam popup "achievement : 30/100", see https://partner.steamgames.com/doc/api/ISteamUserStats#IndicateAchievementProgress
  SteamAPI_ISteamUserStats_IndicateAchievementProgress:       function  (SteamUserStats: Pointer; const AchievementName: PAnsiChar; CurrentProgress: UInt32; MaxProgress: UInt32): TSteamBool; CDecl;
  {$if STEAM_API_VERSION < 1.61}
  SteamAPI_ISteamUserStats_RequestCurrentStats:               function  (SteamUserStats: Pointer): TSteamBool; CDecl;
  {$endif}
  SteamAPI_ISteamUserStats_GetStatFloat: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar; const pData: PSingle): TSteamBool; CDecl;
  SteamAPI_ISteamUserStats_GetStatInt32: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar; const pData: PInt32): TSteamBool; CDecl;
  SteamAPI_ISteamUserStats_RequestGlobalStats: function (SteamUserStats: Pointer; nHistoryDays: CInt): TSteamAPICall; CDecl;
  SteamAPI_ISteamUserStats_RequestUserStats: function (SteamUserStats: Pointer; const SteamIDUser: CSteamID): TSteamAPICall; CDecl;
  SteamAPI_ISteamUserStats_SetAchievement: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar): TSteamBool; CDecl;
  // Call this after changing stats or achievements
  SteamAPI_ISteamUserStats_StoreStats: function (SteamUserStats: Pointer): TSteamBool; CDecl;

  // ISteamInput
  // A versioned accessor is exported by the library
  SteamAPI_SteamInput:                                      function  (): ISteamInput; CDecl;
  SteamAPI_ISteamInput_Init:                                function  (SteamInput: Pointer; bExplicitlyCallRunFrame: LongBool): LongBool; CDecl;
  SteamAPI_ISteamInput_Shutdown:                            function  (SteamInput: Pointer): LongBool; CDecl;
  SteamAPI_ISteamInput_ActivateActionSet:                   procedure (SteamInput: Pointer; inputHandle: TInputHandle;  actionSetHandle: TSteamInputActionSetHandle ); CDecl;
  SteamAPI_ISteamInput_ActivateActionSetLayer:              procedure (SteamInput: Pointer; inputHandle: TInputHandle; actionSetLayerHandle: TSteamInputActionSetHandle ); CDecl;
  SteamAPI_ISteamInput_BNewDataAvailable:                   function  (SteamInput: Pointer): LongBool; CDecl;
  SteamAPI_ISteamInput_BWaitForData:                        function  (SteamInput: Pointer;  bWaitForever: LongBool; unTimeout: UInt32 ): LongBool; CDecl;
  SteamAPI_ISteamInput_DeactivateActionSetLayer:            procedure (SteamInput: Pointer; inputHandle: TInputHandle; actionSetLayerHandle: TSteamInputActionSetHandle );  CDecl;
  SteamAPI_ISteamInput_DeactivateAllActionSetLayers:        procedure (SteamInput: Pointer; inputHandle: TInputHandle ); CDecl;
  SteamAPI_ISteamInput_EnableActionEventCallbacks:          procedure (SteamInput: Pointer; pCallback: TActionCallback); CDecl;
  SteamAPI_ISteamInput_EnableDeviceCallbacks:               procedure (SteamInput: Pointer); CDecl;
  SteamAPI_ISteamInput_GetActionOriginFromXboxOrigin:       function  (SteamInput: Pointer; inputHandle: TInputHandle;  eOrigin: EXboxOrigin ): EInputActionOrigin; CDecl;
  SteamAPI_ISteamInput_GetActionSetHandle:                  function  (SteamInput: Pointer; pszActionSetName: PAnsiChar): TSteamInputActionSetHandle; CDecl;
  SteamAPI_ISteamInput_GetActiveActionSetLayers:            function  (SteamInput: Pointer; inputHandle: TInputHandle; handlesOut: PSteamInputActionSetHandle ): CInt; CDecl;
  SteamAPI_ISteamInput_GetAnalogActionData:                 function  (SteamInput: Pointer; inputHandle: TInputHandle;  analogActionHandle: TSteamInputAnalogActionHandle ): TSteamInputAnalogActionData; CDecl;
  SteamAPI_ISteamInput_GetAnalogActionHandle:               function  (SteamInput: Pointer; pszActionName: PAnsiChar ): TSteamInputAnalogActionHandle; CDecl;
  SteamAPI_ISteamInput_GetAnalogActionOrigins:              function  (SteamInput: Pointer; inputHandle: TInputHandle; actionSetHandle: TSteamInputActionSetHandle; analogActionHandle: TSteamInputAnalogActionHandle; originsOut: PEInputActionOrigin ): CInt; CDecl;
  SteamAPI_ISteamInput_GetConnectedControllers:             function  (SteamInput: Pointer; handlesOut: PSteamInputHandle): CInt; CDecl;
  SteamAPI_ISteamInput_GetControllerForGamepadIndex:        function  (SteamInput: Pointer; nIndex: CInt ): TInputHandle; CDecl;
  SteamAPI_ISteamInput_GetCurrentActionSet:                 function  (SteamInput: Pointer; inputHandle: TInputHandle ): TSteamInputActionSetHandle; CDecl;
  SteamAPI_ISteamInput_GetDeviceBindingRevision:            function  (SteamInput: Pointer; inputHandle: TInputHandle;  pMajor: PCInt; pMinor: PCInt ): LongBool; CDecl;
  SteamAPI_ISteamInput_GetDigitalActionData:                function  (SteamInput: Pointer; inputHandle: TInputHandle;  digitalActionHandle: TSteamInputDigitalActionHandle ): TSteamInputDigitalActionData; CDecl;
  SteamAPI_ISteamInput_GetDigitalActionHandle:              function  (SteamInput: Pointer; pszActionName: PAnsiChar ): TSteamInputDigitalActionHandle; CDecl;
  SteamAPI_ISteamInput_GetDigitalActionOrigins:             function  (SteamInput: Pointer; inputHandle: TInputHandle; actionSetHandle: TSteamInputActionSetHandle; digitalActionHandle: TSteamInputDigitalActionHandle; originsOut: PEInputActionOrigin): CInt; CDecl;
  SteamAPI_ISteamInput_GetGamepadIndexForController:        function  (SteamInput: Pointer; ulinputHandle: TInputHandle ): CInt; CDecl;
  SteamAPI_ISteamInput_GetGlyphForActionOrigin_Legacy:      function  (SteamInput: Pointer; eOrigin: EInputActionOrigin ): PAnsiChar; CDecl;
  SteamAPI_ISteamInput_GetGlyphForXboxOrigin:               function  (SteamInput: Pointer; eOrigin: EXboxOrigin ): PAnsiChar; CDecl;
  SteamAPI_ISteamInput_GetGlyphPNGForActionOrigin:          function  (SteamInput: Pointer; eOrigin: EInputActionOrigin; eSize: ESteamInputGlyphSize; unFlags: UInt32 ): PAnsiChar; CDecl;
  SteamAPI_ISteamInput_GetGlyphSVGForActionOrigin:          function  (SteamInput: Pointer; eOrigin: EInputActionOrigin; unFlags: UInt32 ): PAnsiChar; CDecl;
  SteamAPI_ISteamInput_GetInputTypeForHandle:               function  (SteamInput: Pointer; inputHandle: TInputHandle ): ESteamInputType; CDecl;
  SteamAPI_ISteamInput_GetMotionData:                       function  (SteamInput: Pointer; inputHandle: TInputHandle ): TSteamInputMotionData; CDecl;
  SteamAPI_ISteamInput_GetRemotePlaySessionID:              function  (SteamInput: Pointer; inputHandle: TInputHandle ): UInt32; CDecl;
  SteamAPI_ISteamInput_GetSessionInputConfigurationSettings:function  (SteamInput: Pointer): UInt16; CDecl;
  SteamAPI_ISteamInput_GetStringForActionOrigin:            function  (SteamInput: Pointer; eOrigin: EInputActionOrigin ): PAnsiChar; CDecl;
  SteamAPI_ISteamInput_GetStringForAnalogActionName:        function  (SteamInput: Pointer; eActionHandle: TSteamInputAnalogActionHandle ): PAnsiChar; CDecl;
  SteamAPI_ISteamInput_GetStringForDigitalActionName:       function  (SteamInput: Pointer; eActionHandle: TSteamInputDigitalActionHandle ): PAnsiChar; CDecl;
  SteamAPI_ISteamInput_GetStringForXboxOrigin:              function  (SteamInput: Pointer; eOrigin: EXboxOrigin ): PAnsiChar; CDecl;
  SteamAPI_ISteamInput_Legacy_TriggerHapticPulse:           procedure (SteamInput: Pointer; inputHandle: TInputHandle; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16 ); CDecl;
  SteamAPI_ISteamInput_Legacy_TriggerRepeatedHapticPulse:   procedure (SteamInput: Pointer; inputHandle: TInputHandle; eTargetPad: ESteamControllerPad; usDurationMicroSec: UInt16; usOffMicroSec: UInt16; unRepeat: UInt16; nFlags: CInt ); CDecl;
  SteamAPI_ISteamInput_RunFrame:                            procedure (SteamInput: Pointer; bReservedValue: LongBool); CDecl;
  SteamAPI_ISteamInput_SetDualSenseTriggerEffect:           procedure (SteamInput: Pointer; inputHandle: TInputHandle; const pParam: PScePadTriggerEffectParam ); CDecl;
  SteamAPI_ISteamInput_SetInputActionManifestFilePath:      function  (SteamInput: Pointer; pchInputActionManifestAbsolutePath: PAnsiChar ): LongBool; CDecl;
  SteamAPI_ISteamInput_SetLEDColor:                         procedure (SteamInput: Pointer; inputHandle: TInputHandle; nColorR: UInt8; nColorG: UInt8; nColorB: UInt8; nFlags: UInt32 ); CDecl;
  SteamAPI_ISteamInput_ShowBindingPanel:                    function  (SteamInput: Pointer; inputHandle: TInputHandle ): LongBool; CDecl;
  SteamAPI_ISteamInput_StopAnalogActionMomentum:            procedure (SteamInput: Pointer; inputHandle: TInputHandle; eAction: TSteamInputAnalogActionHandle ); CDecl;
  SteamAPI_ISteamInput_TranslateActionOrigin:               function  (SteamInput: Pointer; eDestinationInputType: ESteamInputType; eSourceOrigin: EInputActionOrigin ): EInputActionOrigin; CDecl;
  SteamAPI_ISteamInput_TriggerSimpleHapticEvent:            procedure (SteamInput: Pointer; inputHandle: TInputHandle; eHapticLocation: EControllerHapticLocation; nIntensity: UInt8; nGainDB: Char; nOtherIntensity: UInt8; nOtherGainDB: Char ); CDecl;
  SteamAPI_ISteamInput_TriggerVibration:                    procedure (SteamInput: Pointer; inputHandle: TInputHandle; usLeftSpeed: UInt16; usRightSpeed: UInt16 ); CDecl;
  SteamAPI_ISteamInput_TriggerVibrationExtended:            procedure (SteamInput: Pointer; inputHandle: TInputHandle; usLeftSpeed: UInt16; usRightSpeed: UInt16; usLeftTriggerSpeed: UInt16; usRightTriggerSpeed: UInt16 ); CDecl;


  // ISteamUtils
  // A versioned accessor is exported by the library
  // SteamAPI_SteamUtils_v<VersionSteamUtils>: function (): ISteamUtils; CDecl;
  // Unversioned accessor to get the current version.
  // In Pascal translation, this is just an alias to 'SteamAPI_SteamUtils_v' + VersionSteamUtils.
  SteamAPI_SteamUtils: function (): ISteamUtils; CDecl;
  // Returns the AppID from Steam (not neccesarily what we tell it the value is)
  SteamAPI_ISteamUtils_GetAppID: function (Self: Pointer): CInt; CDecl;
  // Returns the Raw Bitmap Data in pubDest of image Handle iImage. Must call GetImageSize
  // before calling thius in order to allocate memory for buffer that will be filled
  // the destination buffer size should be 4 * height * width * sizeof(char)
  SteamAPI_ISteamUtils_GetImageRGBA: function (Self: Pointer; iImage: CInt; pubDest: Pointer; nDestBufferSize: Int32): TSteamBool; CDecl;
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
  SteamAPI_ISteamApps_GetAppBuildId: function (Self: Pointer): CInt; CDecl;
  // Takes AppID of DLC and checks if the user owns the DLC & if the DLC is installed
  SteamAPI_ISteamApps_BIsDlcInstalled: function (Self: Pointer; AppID: TAppId): TSteamBool; CDecl;
  // returns the current game language
  SteamAPI_ISteamApps_GetCurrentGameLanguage: function (Self: Pointer): PAnsiChar; CDecl;

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
  {$if STEAM_API_VERSION >= 1.61}
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
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_BReleaseSteamPipe) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_ReleaseUser) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_SetWarningMessageHook) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamApps) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamFriends) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamInput) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUser) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUserStats) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUtils) := nil;

  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_ClearAchievement) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievement) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementAchievedPercent) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementIcon) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementName) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementProgressLimitsFloat) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementProgressLimitsInt32) := nil;

  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetNumAchievements) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_IndicateAchievementProgress) := nil;
  {$if STEAM_API_VERSION < 1.61}
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_RequestCurrentStats) := nil;
  {$endif}
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetStatFloat) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetStatInt32) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_RequestGlobalStats) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_RequestUserStats) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_SetAchievement) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_StoreStats) := nil;

  Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamUtils) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_GetAppID) := nil;
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

  Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamInput) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_Init) := nil;
  Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_Shutdown) := nil;

	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_ActivateActionSet) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_ActivateActionSetLayer) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_BNewDataAvailable) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_BWaitForData) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_DeactivateActionSetLayer) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_DeactivateAllActionSetLayers) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_EnableActionEventCallbacks) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_EnableDeviceCallbacks) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetActionOriginFromXboxOrigin) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetActionSetHandle) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetActiveActionSetLayers) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetAnalogActionData) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetAnalogActionHandle) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetAnalogActionOrigins) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetConnectedControllers) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetControllerForGamepadIndex) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetCurrentActionSet) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetDeviceBindingRevision) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetDigitalActionData) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetDigitalActionHandle) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetDigitalActionOrigins) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetGamepadIndexForController) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetGlyphForActionOrigin_Legacy) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetGlyphForXboxOrigin) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetGlyphPNGForActionOrigin) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetGlyphSVGForActionOrigin) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetInputTypeForHandle) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetMotionData) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetRemotePlaySessionID) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetSessionInputConfigurationSettings) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetStringForActionOrigin) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetStringForAnalogActionName) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetStringForDigitalActionName) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetStringForXboxOrigin) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_Legacy_TriggerHapticPulse) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_Legacy_TriggerRepeatedHapticPulse) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_RunFrame) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_SetDualSenseTriggerEffect) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_SetInputActionManifestFilePath) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_SetLEDColor) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_ShowBindingPanel) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_StopAnalogActionMomentum) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_TranslateActionOrigin) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_TriggerSimpleHapticEvent) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_TriggerVibration) := nil;
	Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_TriggerVibrationExtended) := nil;

  FreeAndNil(SteamLibrary);
end;

procedure InitializeSteamLibrary;
begin
  FinalizeSteamLibrary;

  if SteamLibraryName <> '' then
    SteamLibrary := TDynLib.Load(SteamLibraryName, false);

  if SteamLibrary <> nil then
  begin
    {$if STEAM_API_VERSION >= 1.61}
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
    Pointer({$ifndef FPC}@{$endif} SteamAPI_GetHSteamUser) := SteamLibrary.Symbol('SteamAPI_GetHSteamUser');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_GetHSteamPipe) := SteamLibrary.Symbol('SteamAPI_GetHSteamPipe');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_RegisterCallback) := SteamLibrary.Symbol('SteamAPI_RegisterCallback');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_UnregisterCallback) := SteamLibrary.Symbol('SteamAPI_UnregisterCallback');

    Pointer({$ifndef FPC}@{$endif} SteamInternal_CreateInterface) := SteamLibrary.Symbol('SteamInternal_CreateInterface');

    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_BReleaseSteamPipe) := SteamLibrary.Symbol('SteamAPI_ISteamClient_BReleaseSteamPipe');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_ReleaseUser) := SteamLibrary.Symbol('SteamAPI_ISteamClient_ReleaseUser');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamApps) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamApps');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamFriends) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamFriends');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamInput) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamInput');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUser) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamUser');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUserStats) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamUserStats');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_GetISteamUtils) := SteamLibrary.Symbol('SteamAPI_ISteamClient_GetISteamUtils');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamClient_SetWarningMessageHook) := SteamLibrary.Symbol('SteamAPI_ISteamClient_SetWarningMessageHook');

    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_ClearAchievement) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_ClearAchievement');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievement) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievement');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementAchievedPercent) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementAchievedPercent');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementIcon) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementIcon');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementName) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementName');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementProgressLimitsFloat) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementProgressLimitsFloat');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetAchievementProgressLimitsInt32) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetAchievementProgressLimitsInt32');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetNumAchievements) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetNumAchievements');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_IndicateAchievementProgress) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_IndicateAchievementProgress');
    {$if STEAM_API_VERSION < 1.61}
    // RequestCurrentStats removed in 1.61
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_RequestCurrentStats) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_RequestCurrentStats');
    {$endif}
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetStatFloat) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetStatFloat');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_GetStatInt32) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_GetStatInt32');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_RequestGlobalStats) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_RequestGlobalStats');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_RequestUserStats) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_RequestUserStats');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_SetAchievement) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_SetAchievement');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUserStats_StoreStats) := SteamLibrary.Symbol('SteamAPI_ISteamUserStats_StoreStats');
    // alias to versioned entry point
    Pointer({$ifndef FPC}@{$endif} SteamAPI_SteamUtils) := SteamLibrary.Symbol('SteamAPI_SteamUtils_v' + VersionSteamUtils);
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamUtils_GetAppID) := SteamLibrary.Symbol('SteamAPI_ISteamUtils_GetAppID');
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
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_Init) := SteamLibrary.Symbol('SteamAPI_ISteamInput_Init');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_Shutdown) := SteamLibrary.Symbol('SteamAPI_ISteamInput_Shutdown');

    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_ActivateActionSet)                    := SteamLibrary.Symbol('SteamAPI_ISteamInput_ActivateActionSet');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_ActivateActionSetLayer)               := SteamLibrary.Symbol('SteamAPI_ISteamInput_ActivateActionSetLayer');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_BNewDataAvailable)                    := SteamLibrary.Symbol('SteamAPI_ISteamInput_BNewDataAvailable');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_BWaitForData)                         := SteamLibrary.Symbol('SteamAPI_ISteamInput_BWaitForData');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_DeactivateActionSetLayer)             := SteamLibrary.Symbol('SteamAPI_ISteamInput_DeactivateActionSetLayer');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_DeactivateAllActionSetLayers)         := SteamLibrary.Symbol('SteamAPI_ISteamInput_DeactivateAllActionSetLayers');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_EnableActionEventCallbacks)           := SteamLibrary.Symbol('SteamAPI_ISteamInput_EnableActionEventCallbacks');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_EnableDeviceCallbacks)                := SteamLibrary.Symbol('SteamAPI_ISteamInput_EnableDeviceCallbacks');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetActionOriginFromXboxOrigin)        := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetActionOriginFromXboxOrigin');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetActionSetHandle)                   := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetActionSetHandle');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetActiveActionSetLayers)             := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetActiveActionSetLayers');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetAnalogActionData)                  := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetAnalogActionData');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetAnalogActionHandle)                := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetAnalogActionHandle');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetAnalogActionOrigins)               := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetAnalogActionOrigins');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetConnectedControllers)              := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetConnectedControllers');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetControllerForGamepadIndex)         := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetControllerForGamepadIndex');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetCurrentActionSet)                  := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetCurrentActionSet');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetDeviceBindingRevision)             := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetDeviceBindingRevision');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetDigitalActionData)                 := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetDigitalActionData');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetDigitalActionHandle)               := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetDigitalActionHandle');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetDigitalActionOrigins)              := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetDigitalActionOrigins');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetGamepadIndexForController)         := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetGamepadIndexForController');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetGlyphForActionOrigin_Legacy)       := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetGlyphForActionOrigin_Legacy');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetGlyphForXboxOrigin)                := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetGlyphForXboxOrigin');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetGlyphPNGForActionOrigin)           := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetGlyphPNGForActionOrigin');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetGlyphSVGForActionOrigin)           := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetGlyphSVGForActionOrigin');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetInputTypeForHandle)                := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetInputTypeForHandle');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetMotionData)                        := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetMotionData');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetRemotePlaySessionID)               := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetRemotePlaySessionID');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetSessionInputConfigurationSettings) := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetSessionInputConfigurationSettings');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetStringForActionOrigin)             := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetStringForActionOrigin');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetStringForAnalogActionName)         := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetStringForAnalogActionName');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetStringForDigitalActionName)        := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetStringForDigitalActionName');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_GetStringForXboxOrigin)               := SteamLibrary.Symbol('SteamAPI_ISteamInput_GetStringForXboxOrigin');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_Legacy_TriggerHapticPulse)            := SteamLibrary.Symbol('SteamAPI_ISteamInput_Legacy_TriggerHapticPulse');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_Legacy_TriggerRepeatedHapticPulse)    := SteamLibrary.Symbol('SteamAPI_ISteamInput_Legacy_TriggerRepeatedHapticPulse');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_RunFrame)                             := SteamLibrary.Symbol('SteamAPI_ISteamInput_RunFrame');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_SetDualSenseTriggerEffect)            := SteamLibrary.Symbol('SteamAPI_ISteamInput_SetDualSenseTriggerEffect');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_SetInputActionManifestFilePath)       := SteamLibrary.Symbol('SteamAPI_ISteamInput_SetInputActionManifestFilePath');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_SetLEDColor)                          := SteamLibrary.Symbol('SteamAPI_ISteamInput_SetLEDColor');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_ShowBindingPanel)                     := SteamLibrary.Symbol('SteamAPI_ISteamInput_ShowBindingPanel');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_StopAnalogActionMomentum)             := SteamLibrary.Symbol('SteamAPI_ISteamInput_StopAnalogActionMomentum');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_TranslateActionOrigin)                := SteamLibrary.Symbol('SteamAPI_ISteamInput_TranslateActionOrigin');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_TriggerSimpleHapticEvent)             := SteamLibrary.Symbol('SteamAPI_ISteamInput_TriggerSimpleHapticEvent');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_TriggerVibration)                     := SteamLibrary.Symbol('SteamAPI_ISteamInput_TriggerVibration');
    Pointer({$ifndef FPC}@{$endif} SteamAPI_ISteamInput_TriggerVibrationExtended)             := SteamLibrary.Symbol('SteamAPI_ISteamInput_TriggerVibrationExtended');


  end;
end;

initialization
finalization
  FinalizeSteamLibrary;

end.

