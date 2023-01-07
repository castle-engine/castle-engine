unit CastleSteam;

{$mode ObjFPC}{$H+}

interface

uses
  CastleInternalSteamConstantsAndTypes;

procedure UpdateSteam;
procedure ShutdownSteam;
function InitSteam(const AppId: Integer): Boolean;
implementation

var
  SteamInterface: Pointer;
  SteamUserHandle: HSteamUser;
  SteamPipeHandle: HSteamPipe;

  { char *ver = PAnsiChar }

  { steam_api.h : See full documentation at https://partner.steamgames.com/doc/api/steam_api }
  function SteamAPI_Init(): Boolean; CDecl; external SteamLib;
  //procedure SteamAPI_ReleaseCurrentThreadMemory(); CDecl; external SteamLib;
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

  { isteamuser.h : See documentation at https://partner.steamgames.com/doc/api/ISteamClient }


  { steam_api_internal.h : undocumented? }

  function SteamAPI_GetHSteamUser(): HSteamUser; CDecl; external SteamLib;
  function SteamAPI_GetHSteamPipe(): HSteamPipe; CDecl; external SteamLib;
  //function GetSteamID(): CSteamId; CDecl; external SteamLib;
  //function GetPlayerSteamLevel(): Integer; CDecl; external SteamLib;

  //function SteamAPI_ISteamUserStats_SetAchievement(pchName: PAnsiChar): Boolean; CDecl; external SteamLib;
  //function SteamAPI_ISteamUtils_GetAppID(): UInt32; CDecl; external SteamLib;
  //function SteamAPI_ISteamUser_BLoggedOn(): Boolean; CDecl; external SteamLib;
  //procedure SteamAPI_ISteamClient_SetWarningMessageHook(pFunction: SteamAPIWarningMessageHook); CDecl; external SteamLib;


function InitSteam(const AppId: Integer): Boolean;
begin
  if SteamAPI_Init() then
  begin
    WriteLn('log: SteamAPI_Init successfull');

    if SteamAPI_RestartAppIfNecessary(AppId) then
    begin
      WriteLn('The app was run through exe - restarting through Steam. DRM will do this automatically.');
      Halt(0);
    end else
      WriteLn('The Steam client is running and no restart is necessary');
  end else
    WriteLn('FATAL: SteamAPI_Init failed!');

  SteamInterface := SteamInternal_CreateInterface(PAnsiChar(STEAMCLIENT_INTERFACE_VERSION));
  SteamUserHandle := SteamAPI_GetHSteamUser();
  SteamPipeHandle := SteamAPI_GetHSteamPipe();
end;

procedure UpdateSteam;
begin
  SteamAPI_RunCallbacks();
end;

procedure ShutdownSteam;
begin
  SteamAPI_Shutdown();
end;

end.

