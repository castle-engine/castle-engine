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
  SteamInterface: Pointer;
  SteamUserHandle: HSteamUser;
  SteamPipeHandle: HSteamPipe;

function InitSteam(const AppId: Integer): Boolean;
begin
  if SteamAPI_Init() then
  begin
    WriteLn('log: SteamAPI_Init successfull');

    if SteamAPI_RestartAppIfNecessary(AppId) then
    begin
      WriteLn('The app was run through exe - restarting through Steam. DRM will do this automatically.');
      Exit(false);
      Halt(0);
    end else
      WriteLn('The Steam client is running and no restart is necessary');
  end else
  begin
    WriteLn('FATAL: SteamAPI_Init failed!');
    Exit(false);
  end;

  SteamInterface := SteamInternal_CreateInterface(PAnsiChar(STEAMCLIENT_INTERFACE_VERSION));
  SteamUserHandle := SteamAPI_GetHSteamUser();
  SteamPipeHandle := SteamAPI_GetHSteamPipe();
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

