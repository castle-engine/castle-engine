var

{ steam_api.h : See full documentation at https://partner.steamgames.com/doc/api/steam_api }
  SteamAPI_Init: function (): Boolean; CDecl;
  SteamAPI_ReleaseCurrentThreadMemory: procedure (); CDecl; // UNTESTED
  SteamAPI_RestartAppIfNecessary: function (unOwnAppID: UInt32): Boolean; CDecl;
  SteamAPI_RunCallbacks: procedure (); CDecl;
  SteamAPI_Shutdown: procedure (); CDecl;

{ steam_api_internal.h : undocumented? }

//SteamInternal_ContextInit: function ( void *pContextInitData ): Pointer;
  SteamInternal_CreateInterface: function (SteamClientInterfaceVersion: PAnsiChar): Pointer; CDecl;
//SteamInternal_FindOrCreateUserInterface: function ( HSteamUser hSteamUser, const char *pszVersion ): Pointer;
//SteamInternal_FindOrCreateGameServerInterface: function ( HSteamUser hSteamUser, const char *pszVersion ): Pointer;
  SteamAPI_GetHSteamUser: function (): HSteamUser; CDecl;
  SteamAPI_GetHSteamPipe: function (): HSteamPipe; CDecl;

  SteamAPI_RegisterCallback: procedure (pCallback: Pointer; iCallback: Integer); CDecl;
  SteamAPI_UnregisterCallback: procedure (pCallback: Pointer); CDecl;

{ steam_api_flat.h : contains all available functions in one place
  Weird enough not all of them seem to do what they look like they're supposed to do
  So, sometimes experimenting is necessary : which function will work
  and which will simply crash without explaining any reason
  I don't see any big point in translating all of the headers currently,
  for the said weird behavior first of all,
  Let's add here only functions that were tested and proven to work,
  which should be done on use-case basis. }

(* ISteamClient *)

SteamAPI_ISteamClient_SetWarningMessageHook: procedure (SteamClient: Pointer; WarningMessageHook: SteamAPIWarningMessageHook); CDecl;
SteamAPI_ISteamClient_GetISteamUser: function (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamUserInterfaceVersion: PAnsiChar): Pointer; CDecl;
SteamAPI_ISteamClient_GetISteamUserStats: function (SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamUserStatsInterfaceVersion: PAnsiChar): Pointer; CDecl;

(* ISteamUserStats *)

// SteamAPI_SteamUserStats: function (): Pointer; CDecl; // This one returns something that doesn't work
SteamAPI_ISteamUserStats_RequestCurrentStats: function (SteamUserStats: Pointer): Boolean; CDecl;
SteamAPI_ISteamUserStats_GetAchievement: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar; out Achieved: Boolean): Boolean; CDecl;
SteamAPI_ISteamUserStats_SetAchievement: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar): Boolean; CDecl;
SteamAPI_ISteamUserStats_ClearAchievement: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar): Boolean; CDecl;
SteamAPI_ISteamUserStats_GetNumAchievements: function (SteamUserStats: Pointer): UInt32; CDecl;
// It returns string-ID of the achievement, not a human readable name
SteamAPI_ISteamUserStats_GetAchievementName: function (SteamUserStats: Pointer; AchievementId: UInt32 ): PAnsiChar; CDecl;
// Show Steam popup "achievement : 30/100", see https://partner.steamgames.com/doc/api/ISteamUserStats#IndicateAchievementProgress
SteamAPI_ISteamUserStats_IndicateAchievementProgress: function (SteamUserStats: Pointer; const AchievementName: PAnsiChar; CurrentProgress: UInt32; MaxProgress: UInt32): Boolean; CDecl;

// Call this after changing stats or achievements
SteamAPI_ISteamUserStats_StoreStats: function (SteamUserStats: Pointer): Boolean; CDecl;

// the ones below crash without any reason explained
//SteamAPI_ISteamUserStats_GetStatInt32: function (SteamUserStats: Pointer; const StatName: PAnsiChar; Value: Int32): Boolean; CDecl;
//SteamAPI_ISteamUserStats_GetStatFloat: function (SteamUserStats: Pointer; const StatName: PAnsiChar; Value: Single): Boolean; CDecl;
//SteamAPI_ISteamUserStats_SetStatInt32: function (SteamUserStats: Pointer; const StatName: PAnsiChar; Value: Int32): Boolean; CDecl;
//SteamAPI_ISteamUserStats_SetStatFloat: function (SteamUserStats: Pointer; const StatName: PAnsiChar; Value: Single): Boolean; CDecl;
//SteamAPI_ISteamUserStats_UpdateAvgRateStat: function (SteamUserStats: Pointer; const StatName: PAnsiChar; CountThisSession: Single; SessionLength: Double): Boolean; CDecl;

implementation

uses SysUtils, CastleDynLib;

var
  SteamLibrary: TDynLib;

procedure FinalizeSteamLibrary;
begin
  FreeAndNil(SteamLibrary);
end;

procedure InitializeSteamLibrary;
begin
  FinalizeSteamLibrary; // TODO

  {$ifdef MSWINDOWS}
  SteamLibrary := TDynLib.Load('steam_api_TODO.dll', false);
  {$endif}

  {$ifdef UNIX}
  SteamLibrary := TDynLib.Load('steam_api_TODO.so', false);
  {$endif}

  if SteamLibrary <> nil then
  begin
    Pointer({$ifndef FPC}@{$endif} SteamAPI_Init) := SteamLibrary.Symbol('SteamAPI_Init');
  end;
end;

end.
