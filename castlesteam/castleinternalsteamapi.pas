unit CastleInternalSteamApi;

{$mode ObjFPC}{$H+}

interface

uses
  CastleInternalSteamConstantsAndTypes;

{ char *ver = PAnsiChar }

{ steam_api.h : See full documentation at https://partner.steamgames.com/doc/api/steam_api }
function SteamAPI_Init(): Boolean; CDecl; external SteamLib;
procedure SteamAPI_ReleaseCurrentThreadMemory(); CDecl; external SteamLib; // UNTESTED
function SteamAPI_RestartAppIfNecessary(unOwnAppID: UInt32): Boolean; CDecl; external SteamLib;
procedure SteamAPI_RunCallbacks(); CDecl; external SteamLib;
//procedure SteamAPI_SetMiniDumpComment( const char *pchMsg );
procedure SteamAPI_Shutdown(); CDecl; external SteamLib;
//procedure SteamAPI_WriteMiniDump( uint32 uStructuredExceptionCode, void* pvExceptionInfo, uint32 uBuildID );

{ steam_api_internal.h : undocumented? }

//function SteamInternal_ContextInit( void *pContextInitData ): Pointer;
function SteamInternal_CreateInterface(SteamClientInterfaceVersion: PAnsiChar): Pointer; CDecl; external SteamLib;
//function SteamInternal_FindOrCreateUserInterface( HSteamUser hSteamUser, const char *pszVersion ): Pointer;
//function SteamInternal_FindOrCreateGameServerInterface( HSteamUser hSteamUser, const char *pszVersion ): Pointer;
function SteamAPI_GetHSteamUser(): HSteamUser; CDecl; external SteamLib;
function SteamAPI_GetHSteamPipe(): HSteamPipe; CDecl; external SteamLib;

(* ISteamUserStats *)

function SteamAPI_SteamUserStats(): Pointer; CDecl; external SteamLib;
function SteamAPI_ISteamUserStats_RequestCurrentStats(SteamUserStats: Pointer): Boolean; CDecl; external SteamLib;



implementation

end.

