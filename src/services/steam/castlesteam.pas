unit CastleSteam;

{$mode ObjFPC}{$H+}

interface
uses
  Classes,
  CastleInternalSteamConstantsAndTypes, SteamCallback;

type
  TCastleSteam = class(TObject)
  strict private
    FInitialized: Boolean;
    StoreStats: Boolean;
    SteamClient: Pointer;
    //SteamUser: Pointer;
    SteamUserStats: Pointer;
    SteamUserHandle: HSteamUser;
    SteamPipeHandle: HSteamPipe;
    SteamUserStatsCallbackDispatcher: SteamCallbackDispatcher;
    procedure OnUserStatsReceived(P:Pointer);
    procedure GetAchievements;
    { I'm not sure how to deal with steam errors
      Making every procedure "boolean" and rely on user doing the checks seems inconvenient
      But there are situation when some operation failed because Steam API isn't ready }
    procedure SteamError(const ErrorMsg: String);
  public
    Achievements: TStringList;
    procedure SetAchievement(const AchievementId: String);
    function GetAchievement(const AchievementId: String): Boolean;
    procedure ClearAchievement(const AchievementId: String);
  public
    property Initialized: Boolean read FInitialized;
    { Updates callbacks and other internal Steam functions
      Run at least 10 times a second, better if every frame }
    procedure Update;
    constructor Create; // override;
    destructor Destroy; override;
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
  Exit((FSteam <> nil) and FSteam.Initialized);
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
  WriteLnLog('Steam', 'OnUserStatsReceived');
  FInitialized := true; // maybe only after all callbacks are received - one boolean per each init callback. But as long as we have only one now, this will do
  GetAchievements;
end;

procedure TCastleSteam.GetAchievements;
var
  NumAchievements: UInt32;
  I: Integer;
begin
  NumAchievements := SteamAPI_ISteamUserStats_GetNumAchievements(SteamUserStats);
  if NumAchievements > 0 then
  begin
    Achievements := TStringList.Create;
    for I := 0 to Pred(NumAchievements) do
      Achievements.Add(SteamAPI_ISteamUserStats_GetAchievementName(SteamUserStats, I));
    {for I := 0 to Pred(Achievements.Count) do
      WriteLnLog('"' + Achievements[I] + '"');}
  end;
end;

procedure TCastleSteam.SteamError(const ErrorMsg: String);
begin
  WriteLnWarning(ErrorMsg);
end;

procedure TCastleSteam.SetAchievement(const AchievementId: String);
begin
  if Initialized then
  begin
    if not SteamAPI_ISteamUserStats_SetAchievement(SteamUserStats, PAnsiChar(AchievementId)) then
      SteamError('Failed to SteamAPI_ISteamUserStats_SetAchievement');
    StoreStats := true;
  end else
    SteamError('SetAchievement failed! Steam is not initialized!');
end;

function TCastleSteam.GetAchievement(const AchievementId: String): Boolean;
begin
  if Initialized then
  begin
    if not SteamAPI_ISteamUserStats_GetAchievement(SteamUserStats, PAnsiChar(AchievementId), {out} Result) then
      SteamError('Failed to SteamAPI_ISteamUserStats_GetAchievement');
  end else
    SteamError('GetAchievement failed! Steam is not initialized!');
end;

procedure TCastleSteam.ClearAchievement(const AchievementId: String);
begin
  if Initialized then
  begin
    if not SteamAPI_ISteamUserStats_ClearAchievement(SteamUserStats, PAnsiChar(AchievementId)) then
      SteamError('Failed to SteamAPI_ISteamUserStats_ClearAchievement');
    StoreStats := true;
  end else
    SteamError('ClearAchievement failed! Steam is not initialized!');
end;

procedure TCastleSteam.Update;
begin
  SteamAPI_RunCallbacks();
  if Initialized and StoreStats then
    if SteamAPI_ISteamUserStats_StoreStats(SteamUserStats) then // repeat it every Update until success
      StoreStats := false;
end;

constructor TCastleSteam.Create;
begin
  inherited; // parent is empty
  StoreStats := false;
  FInitialized := false; // waiting for callback

  SteamClient := SteamInternal_CreateInterface(PAnsiChar(STEAMCLIENT_INTERFACE_VERSION));

  SteamAPI_ISteamClient_SetWarningMessageHook(SteamClient, @WarningHook);

  SteamUserHandle := SteamAPI_GetHSteamUser();
  SteamPipeHandle := SteamAPI_GetHSteamPipe();

  //SteamUser := SteamAPI_ISteamClient_GetISteamUser(SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSER_INTERFACE_VERSION);

  SteamUserStats := SteamAPI_ISteamClient_GetISteamUserStats(SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSERSTATS_INTERFACE_VERSION);
  SteamAPI_ISteamUserStats_RequestCurrentStats(SteamUserStats);
  SteamUserStatsCallbackDispatcher := SteamCallbackDispatcher.Create(SteamStatsCallbackID , @OnUserStatsReceived, SizeOf(Steam_UserStatsReceived));
end;

destructor TCastleSteam.Destroy;
begin
  Achievements.Free;
  inherited Destroy;
end;

finalization
  if FSteam <> nil then
  begin
    FSteam.Free;
    SteamAPI_Shutdown();
  end;

end.

