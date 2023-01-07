unit CastleSteam;

{$mode ObjFPC}{$H+}

interface

procedure UpdateSteam;
procedure ShutdownSteam;
function InitSteam(const AppId: Integer): Boolean;
implementation
uses
  CastleInternalSteamApi, CastleInternalSteamConstantsAndTypes;

var
  SteamClient: Pointer;
  SteamUser: Pointer;
  SteamUserStats: Pointer;
  SteamUserHandle: HSteamUser;
  SteamPipeHandle: HSteamPipe;

procedure WarningHook(nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;
begin
  WriteLn(NSeverity, pchDebugText^);
  // TODO: CastleLog
end;

function InitSteam(const AppId: Integer): Boolean;
begin
  if SteamAPI_Init() then
  begin
    WriteLn('log: SteamAPI_Init successfull');

    if SteamAPI_RestartAppIfNecessary(AppId) then
    begin
      WriteLn('The app was run through exe - restarting through Steam.');
      Halt(0);
      Exit(false); // ? do we need this ?
    end else
      WriteLn('The Steam client is running and no restart is necessary');
  end else
  begin
    WriteLn('FATAL: SteamAPI_Init failed!');
    Exit(false);
  end;

  SteamClient := SteamInternal_CreateInterface(PAnsiChar(STEAMCLIENT_INTERFACE_VERSION));

  SteamAPI_ISteamClient_SetWarningMessageHook(SteamClient, @WarningHook);

  SteamUserHandle := SteamAPI_GetHSteamUser();
  SteamPipeHandle := SteamAPI_GetHSteamPipe();

  SteamUser := SteamAPI_ISteamClient_GetISteamUser(SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSER_INTERFACE_VERSION);

  SteamUserStats := SteamAPI_ISteamClient_GetISteamUserStats(SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSERSTATS_INTERFACE_VERSION);
  //SteamAPI_ISteamUserStats_RequestCurrentStats(SteamUserStats);
  Exit(true);
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

