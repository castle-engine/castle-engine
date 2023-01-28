unit CastleSteam;

{$mode ObjFPC}{$H+}

interface

(* Basic functions *)

{ This one needs to be run at least 10 times a second,
  Better if every frame }
procedure UpdateSteam;
{ Disconnect from Steam }
procedure ShutdownSteam;
{ Connect to Steam and initialize everything }
function InitSteam(const AppId: Integer): Boolean;

(* Achievements *)

procedure SetAchievement(const AchievementId: String);
function GetAchievement(const AchievementId: String): Boolean;
procedure ClearAchievement(const AchievementId: String);
implementation
uses
  CastleInternalSteamApi, CastleInternalSteamConstantsAndTypes, SteamCallback;

type
  TTemporary = class(TObject)
    procedure OnUserStatsReceived(P:Pointer);
  end;

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

procedure OnUserStatsReceived(Data: UserStatsReceived_t); Cdecl;
begin
  WriteLn('OnUserStatsReceived');
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
  SteamAPI_ISteamUserStats_RequestCurrentStats(SteamUserStats);

  //SteamAPI_RegisterCallback(@OnUserStatsReceived, UserStatsReceived_t.k_iCallback);
  SteamCallbackDispatcher.Create(SteamStatsCallbackID , TTemporary(nil).OnUserStatsReceived, SizeOf(Steam_UserStatsReceived));

  Exit(true);
end;

procedure SetAchievement(const AchievementId: String);
begin
  SteamAPI_ISteamUserStats_SetAchievement(SteamUserStats, PAnsiChar(AchievementId));
end;

function GetAchievement(const AchievementId: String): Boolean;
begin
  SteamAPI_ISteamUserStats_GetAchievement(SteamUserStats, PAnsiChar(AchievementId), {out} Result);
end;

procedure ClearAchievement(const AchievementId: String);
begin
  SteamAPI_ISteamUserStats_ClearAchievement(SteamUserStats, PAnsiChar(AchievementId));
end;

procedure UpdateSteam;
begin
  SteamAPI_RunCallbacks();
end;

procedure ShutdownSteam;
begin
  SteamAPI_Shutdown();
end;

{ TTemporary }

procedure TTemporary.OnUserStatsReceived(P: Pointer);
begin
  WriteLn('OnUserStatsReceived');
end;

end.

