{ Translation of Steam headers (first of all steam_api_flat.h and steam_api_internal.h).
  Note that for yet unknown reason not all Steam calls are accessible from Pascal code
  Therefore only those calls that were tested and proven to work are included.
  Some features can be accessed in more than one way, through different calls
  which was accounted for in this unit.
  We'd like to note invaluable help that Apus Engine's sources by Ivan Polyacov (Cooler2)
  have provided in hunting down those specific calls and a few tricks to make them work properly. }
unit CastleInternalSteamApi;

{$I castleconf.inc}

interface

{$ifdef LINUX}{$ifdef CPU64}
  {$define STEAM_API}
{$endif}{$endif}
{$ifdef MSWINDOWS}{$ifdef CPU64}
  {$define STEAM_API}
{$endif}{$endif}

{$ifdef STEAM_API}

uses
  CastleInternalSteamConstantsAndTypes;

{ steam_api.h : See full documentation at https://partner.steamgames.com/doc/api/steam_api }
function SteamAPI_Init(): Boolean; CDecl; external SteamLib;
procedure SteamAPI_ReleaseCurrentThreadMemory(); CDecl; external SteamLib; // UNTESTED
function SteamAPI_RestartAppIfNecessary(unOwnAppID: UInt32): Boolean; CDecl; external SteamLib;
procedure SteamAPI_RunCallbacks(); CDecl; external SteamLib;
procedure SteamAPI_Shutdown(); CDecl; external SteamLib;

{ steam_api_internal.h : undocumented? }

//function SteamInternal_ContextInit( void *pContextInitData ): Pointer;
function SteamInternal_CreateInterface(SteamClientInterfaceVersion: PAnsiChar): Pointer; CDecl; external SteamLib;
//function SteamInternal_FindOrCreateUserInterface( HSteamUser hSteamUser, const char *pszVersion ): Pointer;
//function SteamInternal_FindOrCreateGameServerInterface( HSteamUser hSteamUser, const char *pszVersion ): Pointer;
function SteamAPI_GetHSteamUser(): HSteamUser; CDecl; external SteamLib;
function SteamAPI_GetHSteamPipe(): HSteamPipe; CDecl; external SteamLib;

procedure SteamAPI_RegisterCallback(pCallback: Pointer; iCallback: Integer); CDecl; external SteamLib;
procedure SteamAPI_UnregisterCallback(pCallback: Pointer); CDecl; external SteamLib;

{ steam_api_flat.h : contains all available functions in one place
  Weird enough not all of them seem to do what they look like they're supposed to do
  So, sometimes experimenting is necessary : which function will work
  and which will simply crash without explaining any reason
  I don't see any big point in translating all of the headers currently,
  for the said weird behavior first of all,
  Let's add here only functions that were tested and proven to work,
  which should be done on use-case basis. }

(* ISteamClient *)

procedure SteamAPI_ISteamClient_SetWarningMessageHook(SteamClient: Pointer; WarningMessageHook: SteamAPIWarningMessageHook); CDecl; external SteamLib;
function SteamAPI_ISteamClient_GetISteamUser(SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamUserInterfaceVersion: PAnsiChar): Pointer; CDecl; external SteamLib;
function SteamAPI_ISteamClient_GetISteamUserStats(SteamClient: Pointer; SteamUserHandle: HSteamUser; SteamPipeHandle: HSteamPipe; const SteamUserStatsInterfaceVersion: PAnsiChar): Pointer; CDecl; external SteamLib;

(* ISteamUserStats *)

// function SteamAPI_SteamUserStats(): Pointer; CDecl; external SteamLib; // This one returns something that doesn't work
function SteamAPI_ISteamUserStats_RequestCurrentStats(SteamUserStats: Pointer): Boolean; CDecl; external SteamLib;
function SteamAPI_ISteamUserStats_GetAchievement(SteamUserStats: Pointer; const AchievementName: PChar; out Achieved: Boolean): Boolean; CDecl; external SteamLib;
function SteamAPI_ISteamUserStats_SetAchievement(SteamUserStats: Pointer; const AchievementName: PChar): Boolean; CDecl; external SteamLib;
function SteamAPI_ISteamUserStats_ClearAchievement(SteamUserStats: Pointer; const AchievementName: PChar): Boolean; CDecl; external SteamLib;
function SteamAPI_ISteamUserStats_GetNumAchievements(SteamUserStats: Pointer): UInt32; CDecl; external SteamLib;
// It returns string-ID of the achievement, not a human readable name
function SteamAPI_ISteamUserStats_GetAchievementName(SteamUserStats: Pointer; AchievementId: UInt32 ): PAnsiChar; CDecl; external SteamLib;
// Show Steam popup "achievement : 30/100", see https://partner.steamgames.com/doc/api/ISteamUserStats#IndicateAchievementProgress
function SteamAPI_ISteamUserStats_IndicateAchievementProgress(SteamUserStats: Pointer; const AchievementName: PChar; CurrentProgress: UInt32; MaxProgress: UInt32): Boolean; CDecl; external SteamLib;

// Call this after changing stats or achievements
function SteamAPI_ISteamUserStats_StoreStats(SteamUserStats: Pointer): Boolean; CDecl; external SteamLib;

// the ones below crash without any reason explained
//function SteamAPI_ISteamUserStats_GetStatInt32(SteamUserStats: Pointer; const StatName: PChar; Value: Int32): Boolean; CDecl; external SteamLib;
//function SteamAPI_ISteamUserStats_GetStatFloat(SteamUserStats: Pointer; const StatName: PChar; Value: Single): Boolean; CDecl; external SteamLib;
//function SteamAPI_ISteamUserStats_SetStatInt32(SteamUserStats: Pointer; const StatName: PChar; Value: Int32): Boolean; CDecl; external SteamLib;
//function SteamAPI_ISteamUserStats_SetStatFloat(SteamUserStats: Pointer; const StatName: PChar; Value: Single): Boolean; CDecl; external SteamLib;
//function SteamAPI_ISteamUserStats_UpdateAvgRateStat(SteamUserStats: Pointer; const StatName: PChar; CountThisSession: Single; SessionLength: Double): Boolean; CDecl; external SteamLib;

{$endif}

implementation


end.

