unit CastleSteam;

{$mode ObjFPC}{$H+}

interface
uses
  CastleInternalSteamConstantsAndTypes, SteamCallback;

type
  TCastleSteam = class(TObject)
  strict private
    SteamClient: Pointer;
    //SteamUser: Pointer;
    SteamUserStats: Pointer;
    SteamUserHandle: HSteamUser;
    SteamPipeHandle: HSteamPipe;
    SteamUserStatsCallbackDispatcher: SteamCallbackDispatcher;
    procedure OnUserStatsReceived(P:Pointer);
  public
    procedure SetAchievement(const AchievementId: String);
    function GetAchievement(const AchievementId: String): Boolean;
    procedure ClearAchievement(const AchievementId: String);
  public
    { Updates callbacks and other internal Steam functions
      Run at least 10 times a second, better if every frame }
    procedure Update;
    constructor Create; // override;
  end;

function Steam: TCastleSteam;

{ Connect to Steam and initialize everything }
function InitSteam(const AppId: Integer): Boolean;

implementation
uses
  SysUtils,
  CastleLog,
  CastleInternalSteamApi;

var
  FSteam: TCastleSteam;

function SteamInitialized: Boolean;
begin
  Exit(FSteam <> nil);
end;

function Steam: TCastleSteam;
begin
  if FSteam = nil then
    raise Exception.Create('Steam is not initialized');
  Exit(FSteam);
end;

procedure WarningHook(nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;
begin
  WriteLnLog('Steam Warning: ''%s'' (Severity: %d)', [pchDebugText^, NSeverity]);
end;

function InitSteam(const AppId: Integer): Boolean;
begin
  if SteamAPI_Init() then
  begin
    WriteLnLog('SteamAPI_Init successfull');

    if SteamAPI_RestartAppIfNecessary(AppId) then
    begin
      WriteLnLog('The app was run through exe - restarting through Steam.');
      Halt(0);
    end else
      WriteLnLog('The app was started through Steam');
  end else
  begin
    WriteLnWarning('FATAL: SteamAPI_Init failed!');
    Halt(1);
  end;

  FSteam := TCastleSteam.Create;

  Exit(true);
end;

procedure TCastleSteam.OnUserStatsReceived(P: Pointer);
begin
  SteamUserStatsCallbackDispatcher.Free;
  WriteLnLog('OnUserStatsReceived');
end;

procedure TCastleSteam.SetAchievement(const AchievementId: String);
begin
  SteamAPI_ISteamUserStats_SetAchievement(SteamUserStats, PAnsiChar(AchievementId));
  SteamAPI_ISteamUserStats_StoreStats(SteamUserStats);
end;

function TCastleSteam.GetAchievement(const AchievementId: String): Boolean;
begin
  SteamAPI_ISteamUserStats_GetAchievement(SteamUserStats, PAnsiChar(AchievementId), {out} Result);
end;

procedure TCastleSteam.ClearAchievement(const AchievementId: String);
begin
  SteamAPI_ISteamUserStats_ClearAchievement(SteamUserStats, PAnsiChar(AchievementId));
  SteamAPI_ISteamUserStats_StoreStats(SteamUserStats)
end;

procedure TCastleSteam.Update;
begin
  SteamAPI_RunCallbacks();
end;

constructor TCastleSteam.Create;
begin
  inherited; // parent is empty
  SteamClient := SteamInternal_CreateInterface(PAnsiChar(STEAMCLIENT_INTERFACE_VERSION));

  SteamAPI_ISteamClient_SetWarningMessageHook(SteamClient, @WarningHook);

  SteamUserHandle := SteamAPI_GetHSteamUser();
  SteamPipeHandle := SteamAPI_GetHSteamPipe();

  //SteamUser := SteamAPI_ISteamClient_GetISteamUser(SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSER_INTERFACE_VERSION);

  SteamUserStats := SteamAPI_ISteamClient_GetISteamUserStats(SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSERSTATS_INTERFACE_VERSION);
  SteamAPI_ISteamUserStats_RequestCurrentStats(SteamUserStats);

  SteamUserStatsCallbackDispatcher := SteamCallbackDispatcher.Create(SteamStatsCallbackID , @OnUserStatsReceived, SizeOf(Steam_UserStatsReceived));
end;

finalization
  if FSteam <> nil then
  begin
    FSteam.Free;
    SteamAPI_Shutdown();
  end;

end.

