{
  Copyright 2023-2024 Michalis Kamburelis, Yevhen Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Integration with Steam.
  See @url(https://castle-engine.io/steam Steam and Castle Game Engine documentation)
  for usage. }
unit CastleSteam;

{$I castleconf.inc}

interface

uses Classes,
  CastleInternalSteamConstantsAndTypes, CastleInternalSteamApi;

type
  { Access to Steam.
    Do not create a manual instance of the class,
    call SteamInit(AppId) to initialize a single instance of this class.
    Steam API will shut down automatically when the app shuts down. }
  TCastleSteam = class
  strict private
    FEnabled: Boolean;
    FInitialized: Boolean;
    StoreStats: Boolean;
    SteamClient: Pointer;
    //SteamUser: Pointer; // We aren't using it right now, but it works
    SteamUserStats: Pointer;
    SteamUserHandle: HSteamUser;
    SteamPipeHandle: HSteamPipe;
    procedure CallbackUserStatsReceived(P: PUserStatsReceived);
    procedure GetAchievements;
    { For now if some operation failed, we log an error into WriteLnWarning
      This seems to be the practice of SteamWorks examples.
      So hopefully Steam API cannot randomly fail at regular requests
      if they are formulated correctly (e.g. achievement exists, etc.).
      Make sure Steam.Initialized before calling Steam features }
    procedure SteamError(const ErrorMsg: String);
  public
    { List of achievements for this game
      These are IDs of achievements, used in other calls
      This field is initialized after Steam has been initialized
      and is nil until that moment. }
    Achievements: TStringList;

    constructor Create(const AppId: Integer);
    destructor Destroy; override;

    { Set achievement as "achieved",
      Steam will automatically show a small overlay indicating that the
      achievement is obtained. }
    procedure SetAchievement(const AchievementId: String);

    { Is this achievement "achieved". }
    function GetAchievement(const AchievementId: String): Boolean;

    { Set achievement as "not achieved".
      Should not be necessary for normal usage (the convention is that
      once users achieve something, it stays achieved forever).
      But this is useful for testing purposes -- you may want to clear own
      achievements during testing. }
    procedure ClearAchievement(const AchievementId: String);

    { Clears all achievements from the connected user.
      @seealso ClearAchievement }
    procedure ClearAllAchievements;

    { Show Steam overlay "progress towards achievement" i.e. "Wins 33/100" }
    procedure IndicateAchievementProgress(const AchievementId: String;
      const CurrentProgress, MaxProgress: UInt32);
  public
    { Are we connected to Steam successfully.
      Before this is @true, methods of this class (like reporting achievements)
      don't do anything. }
    property Initialized: Boolean read FInitialized;

    { Updates callbacks, performs saving of user stats
      According to SteamWorks documentation you should run this at least 10 times a second, better if every frame.
      TODO: This should not be necessary to be public }
    procedure Update;

    { Do we have Steam integration available (but not necessarily initialized yet,
      see @link(Initialized) for that).

      If @true, this means that Steam dynamic library was found,
      and we don't need to restart the application.
      See https://castle-engine.io/steam for more information.

      This is the normal state of things when the game is run through Steam.
      It means that all features of this class work as they should. }
    property Enabled: Boolean read FEnabled;
  end;

{ Instance of Steam API bridge,
  use this singleton to access to Steam API.
  Will throw an exception "Steam not initialized" if called before InitSteam }
function Steam: TCastleSteam;

{ Connect to Steam and initialize everything.

  @orderedList(
    @item(It tries to connect to Steam API and checks if the game was run
      through Steam or through exe file. In the latter case the game will
      automatically restart through Steam.
      See the `examples/steam/` README for a description how does the Steam
      integration behave, when it restarts the game.)

    @item(Will ask TCastleSteam to initialize interfaces and request user stats.

      Note: Steam will not be fully initialized until confirmation callback
      will be received by TCastleSteam, this may take several frames.
      Check for Steam.Initialized before using non-trivial features of Steam API.
      You need to provide AppId for your app to this function.
    )
  )

  TODO: update docs for OnInitialize.
  TODO: Let user do Steam := TCastleSteam.Create(AppId) instead of InitSteam(AppId). }
procedure InitSteam(const AppId: Integer);

implementation

uses SysUtils, CTypes,
  CastleLog;

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

procedure InitSteam(const AppId: Integer);
begin
  FSteam := TCastleSteam.Create(AppId);
end;

{ TCastleSteam --------------------------------------------------------------- }

constructor TCastleSteam.Create(const AppId: Integer);

  { Set some conditions and set Enabled.
    It may also restart the game through Steam if it was run through exe,
    which means it will Halt the current process. }
  procedure CheckEnabledAndRestart;
  begin
    FEnabled := false;

    { Is Steam library available at runtime. }
    if SteamLibrary <> nil then
    begin
      // Initialize Steam API
      if SteamAPI_Init() then
      begin
        WriteLnLog('SteamAPI_Init successfull');

        // If the app was started through EXE - restart the game through Steam
        if SteamAPI_RestartAppIfNecessary(AppId) then
        begin
          WriteLnLog('The app was run through exe - restarting through Steam.');
          Halt(0);
        end else
        begin
          FEnabled := true;
          WriteLnLog('The app was started through Steam');
        end;
      end else
        WriteLnWarning('SteamAPI_Init failed. This means Steam does not run in the background, but you run the application in development mode (with steam_appid.txt). In this case Steam integration will not work. See https://castle-engine.io/steam for information how to test the Steam integration.');
    end else
    begin
      {$warnings off} // ignore FPC warnings about unreachable code, since SteamLibraryName is constant
      if SteamLibraryName <> '' then
        WriteLnWarning('Steam dynamic library "%s" not found', [SteamLibraryName])
      else
        WriteLnWarning('Steam is not supported on this platform');
      {$warnings on}
    end;
  end;

  procedure InitialSteamCalls;
  begin
    if not Enabled then
      Exit;
    SteamClient := SteamInternal_CreateInterface(PAnsiChar(STEAMCLIENT_INTERFACE_VERSION));

    // Set a callback for Steam warnings
    SteamAPI_ISteamClient_SetWarningMessageHook(SteamClient,
      {$ifdef FPC}@{$endif} WarningHook);

    SteamUserHandle := SteamAPI_GetHSteamUser();
    SteamPipeHandle := SteamAPI_GetHSteamPipe();

    // Not used yet
    // SteamUser := SteamAPI_ISteamClient_GetISteamUser(
    //   SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSER_INTERFACE_VERSION);

    // Init SteamUserStats and request UserStats (will wait for callback, handled in Update)
    SteamUserStats := SteamAPI_ISteamClient_GetISteamUserStats(
      SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSERSTATS_INTERFACE_VERSION);
    SteamAPI_ISteamUserStats_RequestCurrentStats(SteamUserStats);

    SteamAPI_ManualDispatch_Init();
  end;

begin
  inherited Create;

  StoreStats := false;
  FInitialized := false; // waiting for callback

  InitializeSteamLibrary;
  CheckEnabledAndRestart;
  InitialSteamCalls;
end;

destructor TCastleSteam.Destroy;
begin
  FreeAndNil(Achievements);
  if Enabled then
    SteamAPI_Shutdown();
  inherited Destroy;
end;

procedure TCastleSteam.CallbackUserStatsReceived(P: PUserStatsReceived);
begin
  WriteLnLog('Steam', 'Received UserStats from Steam');
  FInitialized := true; // TODO: Initialized -> UserStatsInitialized?
  GetAchievements;
end;

procedure TCastleSteam.GetAchievements;
var
  NumAchievements: UInt32;
  I: Integer;
begin
  if not Enabled then
    Exit;
  NumAchievements := SteamAPI_ISteamUserStats_GetNumAchievements(SteamUserStats);
  Achievements := TStringList.Create;
  if NumAchievements > 0 then
    for I := 0 to NumAchievements - 1 do
      Achievements.Add(SteamAPI_ISteamUserStats_GetAchievementName(SteamUserStats, I));
  WriteLnLog('Steam Achievements', Achievements.Count.ToString);
end;

procedure TCastleSteam.SteamError(const ErrorMsg: String);
begin
  WriteLnWarning(ErrorMsg);
end;

procedure TCastleSteam.SetAchievement(const AchievementId: String);
begin
  if not Enabled then
    Exit;
  if Initialized then
  begin
    if not SteamAPI_ISteamUserStats_SetAchievement(SteamUserStats, PAnsiChar(AchievementId)) then
      SteamError('Failed to SteamAPI_ISteamUserStats_SetAchievement');
    StoreStats := true;
  end else
    SteamError('SetAchievement failed! Steam is not initialized!');
end;

function TCastleSteam.GetAchievement(const AchievementId: String): Boolean;
var
  CAchieved: CBool;
begin
  Result := false;
  if not Enabled then
    Exit;
  if Initialized then
  begin
    if SteamAPI_ISteamUserStats_GetAchievement(SteamUserStats,
        PAnsiChar(AchievementId), @CAchieved) then
    begin
      Result := CAchieved;
    end else
      SteamError('Failed to SteamAPI_ISteamUserStats_GetAchievement');
  end else
    SteamError('GetAchievement failed! Steam is not initialized!');
end;

procedure TCastleSteam.ClearAchievement(const AchievementId: String);
begin
  if not Enabled then
    Exit;
  if Initialized then
  begin
    if not SteamAPI_ISteamUserStats_ClearAchievement(SteamUserStats, PAnsiChar(AchievementId)) then
      SteamError('Failed to SteamAPI_ISteamUserStats_ClearAchievement');
    StoreStats := true;
  end else
    SteamError('ClearAchievement failed! Steam is not initialized!');
end;

procedure TCastleSteam.ClearAllAchievements;
var
  S: String;
begin
  if not Enabled then
    Exit;
  if Initialized then
    for S in Achievements do
      ClearAchievement(S)
  else
  SteamError('ClearAllAchievements failed! Steam is not initialized!');
end;

procedure TCastleSteam.IndicateAchievementProgress(const AchievementId: String;
  const CurrentProgress, MaxProgress: UInt32);
begin
  if not Enabled then
    Exit;
  if Initialized then
  begin
    if not SteamAPI_ISteamUserStats_IndicateAchievementProgress(SteamUserStats, PAnsiChar(AchievementId), CurrentProgress, MaxProgress) then
      SteamError('Failed to SteamAPI_ISteamUserStats_IndicateAchievementProgress');
  end else
    SteamError('IndicateAchievementProgress failed! Steam is not initialized!');
end;

procedure TCastleSteam.Update;

  { Request callbacks from Steam API if any pending.

    We don't use SteamAPI_RunCallbacks, as it
    - Requires quite hacky code, to simulate C++ class VMT, with a specially
      crafted record and assembler help. See TODO link to old code.
    - Crashes on Linux/x86_64 (maybe because of the above).

    Following the Steamworks docs recommendation, we use manual dispatching,
    see https://partner.steamgames.com/doc/sdk/api#manual_dispatch
    and example code at SteamAPI_ManualDispatch_Init.
  }
  procedure SteamRunCallbacks;
  var
    Callback: TCallbackMsg;
    PCallCompleted: PSteamAPICallCompleted;
    PTmpCallResult: Pointer;
    BFailed: CBool;
  begin
  	SteamAPI_ManualDispatch_RunFrame(SteamPipeHandle);
	  while SteamAPI_ManualDispatch_GetNextCallback(SteamPipeHandle, @Callback) do
    begin
		  // Check for dispatching API call results

      // Look at callback.m_iCallback to see what kind of callback it is,
      // and dispatch to appropriate handler(s)
		  case Callback.m_iCallback of
        TSteamAPICallCompleted.k_iCallback:
          begin
            PCallCompleted := PSteamAPICallCompleted(Callback.m_pubParam);
            PTmpCallResult := GetMem(Callback.m_cubParam);
            if SteamAPI_ManualDispatch_GetAPICallResult(SteamPipeHandle,
                PCallCompleted^.m_hAsyncCall, PTmpCallResult, Callback.m_cubParam,
                Callback.m_iCallback, @BFailed) then
            begin
              // TODO:
              // Dispatch the call result to the registered handler(s) for the
              // call identified by pCallCompleted^.m_hAsyncCall
              WritelnLog('Steam', 'Dispatch the call result to the handlers for m_iCallback %d, m_hAsyncCall %d', [
                PCallCompleted^.m_iCallback,
                PCallCompleted^.m_hAsyncCall
              ]);
            end;
            FreeMem(PTmpCallResult);
          end;
        TUserStatsReceived.k_iCallback:
          begin
            // SteamUserStatsCallbackDispatcher.Dispatch(Callback.m_pubParam);
            CallbackUserStatsReceived(PUserStatsReceived(Callback.m_pubParam));
          end;
        else
          begin
            WritelnLog('Steam', 'Callback m_iCallback %d, we ignore it now', [
              Callback.m_iCallback
            ]);
          end;
      end;

      // We must call this before next SteamAPI_ManualDispatch_GetNextCallback.
      SteamAPI_ManualDispatch_FreeLastCallback(SteamPipeHandle);
    end;
  end;

begin
  if not Enabled then
    Exit;

  SteamRunCallbacks;

  // If we have unsaved changes, try saving them; if failed - repeat
  if Initialized and StoreStats then
    if SteamAPI_ISteamUserStats_StoreStats(SteamUserStats) then // repeat it every Update until success
      StoreStats := false;
end;

initialization // Delphi needs initialization before finalization
finalization
  FreeAndNil(FSteam);
end.

