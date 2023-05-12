{
  Copyright 2023-2023 Michalis Kamburelis, Yevhen Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Provides basic interaction with Steam API
  Call InitSteam(AppId) and wait for Steam.Initialized before starting using it
  For now features mostly interaction with Steam Achievements
  Note: Current calls to Steam API require a specific version of SteamWorks: 1.57
  You can download the corresponding dynamic library from
  https://partner.steamgames.com/downloads/list
  Currently supported OS:
   * Windows 32 - functional
   * Windows 64 - functional
   * Linux 64 - not tested, should be working
   * Linux 32 - not tested, has a chance to work out of the box
   * Mac OS X - failing
}
unit CastleSteam;

{$I castleconf.inc}

interface

uses
  Classes,
  CastleInternalSteamConstantsAndTypes, CastleInternalSteamCallback;

type
  { Provides simplified access to some Steam API functions
    Do not create a manual instance of the class,
    call SteamInit(AppId) to initialize singleton of this class.
    Steam API will shut down automatically when the app shuts down }
  TCastleSteam = class(TObject)
  strict private
    FInitialized: Boolean;
    StoreStats: Boolean;
    SteamClient: Pointer;
    //SteamUser: Pointer; // We aren't using it right now, but it works
    SteamUserStats: Pointer;
    SteamUserHandle: HSteamUser;
    SteamPipeHandle: HSteamPipe;
    SteamUserStatsCallbackDispatcher: SteamCallbackDispatcher;
    procedure OnUserStatsReceived(P: Pointer);
    procedure GetAchievements;
    { For now if some operation failed, we log an error into WriteLnWarning
      This seems to be the practice of SteamWorks examples,
      So hopefully Steam API cannot randomly fail at regular requests
      if they are formulated correctly (e.g. achievement exists, etc.)
      Make sure Steam.Initialized before calling Steam features }
    procedure SteamError(const ErrorMsg: String);
  public
    { List of achievements for this game
      These are IDs of achievements, used in other calls
      This field is initialized after Steam has been initialized
      and is nil until that moment }
    Achievements: TStringList;
    { Set achievement as "achieved",
      This will also toggle overlay indicating the obtained achievement to the Player }
    procedure SetAchievement(const AchievementId: String);
    { Get state of the achievement. Returns:
      "true" - achievement has been achieved
      "false" - achievement has not been achieved }
    function GetAchievement(const AchievementId: String): Boolean;
    { Set achievement as "not achieved"
      sometimes useful for testing purposes }
    procedure ClearAchievement(const AchievementId: String);
    { Clears all achievements from the connected user }
    procedure ClearAllAchievements;
    { Shows Steam overlay "progress towards achievement" i.e. Wins 33/100 }
    procedure IndicateAchievementProgress(const AchievementId: String; const CurrentProgress, MaxProgress: UInt32);
  public
    { If this instance is properly initialized,
      Check this before using more complex Steam API calls
      It should regularly take several frames to initialize Steam API,
      So avoid calling Steam features too early, e.g. from Initialization code }
    property Initialized: Boolean read FInitialized;
    { Updates callbacks, performs saving of user stats
      According to SteamWorks documentation you should run this at least 10 times a second, better if every frame }
    procedure Update;
    constructor Create; // override;
    destructor Destroy; override;
  end;

{ Instance of Steam API bridge,
  use this singleton to access to Steam API
  Will throw an exception "Steam not initialized" if called before InitSteam }
function Steam: TCastleSteam;

{ Connect to Steam and initialize everything:
  1. It checks if Steam is running and exits with error code 1 otherwise
  2. It tries to connect to Steam API and checks if the game was run
     through Steam or through exe file. In the latter case the game will
     automatically restart through Steam
     Note: this behavior is recommended for end-user build of the game,
     to avoid this, place steam_appid.txt with your game's AppId near app's executable
  3. Will ask TCastleSteam to initialize interfaces and request user stats
     Note: Steam will not be fully initialized until confirmation callback
     will be received by TCastleSteam, this may take several frames.
     Check for Steam.Initialized before using non-trivial features of Steam API.
  You need to provide AppId for your app to this function }
procedure InitSteam(const AppId: Integer);
{ If Steam library is available runtime }
function SteamLibraryAvailable: Boolean;
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

procedure InitSteam(const AppId: Integer);
begin
  InitializeSteamLibrary;
  if SteamLibraryAvailable then
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
        WriteLnLog('The app was started through Steam');
    end else
    begin
      WriteLnWarning('FATAL: SteamAPI_Init failed!');
      Halt(1);
    end;
  end else
    WriteLnWarning('Steam Library is not available.');
    // NOTE: in this case SteamLibrary will not be available;
    // it's up to user to check if SteamLibraryAvailable and potentially halt program execution

  // Create TCastleSteam instance and initialize it
  FSteam := TCastleSteam.Create;
end;

procedure TCastleSteam.OnUserStatsReceived(P: Pointer);
begin
  if not SteamLibraryAvailable then
    Exit;
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
  if not SteamLibraryAvailable then
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
  if not SteamLibraryAvailable then
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
begin
  if not SteamLibraryAvailable then
    Exit;
  if Initialized then
  begin
    if not SteamAPI_ISteamUserStats_GetAchievement(SteamUserStats, PAnsiChar(AchievementId), {out} Result) then
      SteamError('Failed to SteamAPI_ISteamUserStats_GetAchievement');
  end else
    SteamError('GetAchievement failed! Steam is not initialized!');
end;

procedure TCastleSteam.ClearAchievement(const AchievementId: String);
begin
  if not SteamLibraryAvailable then
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
  if not SteamLibraryAvailable then
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
  if not SteamLibraryAvailable then
    Exit;
  if Initialized then
  begin
    if not SteamAPI_ISteamUserStats_IndicateAchievementProgress(SteamUserStats, PAnsiChar(AchievementId), CurrentProgress, MaxProgress) then
      SteamError('Failed to SteamAPI_ISteamUserStats_IndicateAchievementProgress');
  end else
    SteamError('IndicateAchievementProgress failed! Steam is not initialized!');
end;

procedure TCastleSteam.Update;
begin
  if not SteamLibraryAvailable then
    Exit;
  // Request callbacks from Steam API if any pending
  SteamAPI_RunCallbacks();
  // If we have unsaved changes, try saving them; if failed - repeat
  if Initialized and StoreStats then
    if SteamAPI_ISteamUserStats_StoreStats(SteamUserStats) then // repeat it every Update until success
      StoreStats := false;
end;

constructor TCastleSteam.Create;
begin
  inherited; // parent is empty
  StoreStats := false;
  FInitialized := false; // waiting for callback

  if not SteamLibraryAvailable then
    Exit;
  SteamClient := SteamInternal_CreateInterface(PAnsiChar(STEAMCLIENT_INTERFACE_VERSION));

  // Set a callback for Steam warnings
  SteamAPI_ISteamClient_SetWarningMessageHook(SteamClient, {$ifdef FPC}@{$endif}WarningHook);

  SteamUserHandle := SteamAPI_GetHSteamUser();
  SteamPipeHandle := SteamAPI_GetHSteamPipe();

  // Not used yet
  // SteamUser := SteamAPI_ISteamClient_GetISteamUser(SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSER_INTERFACE_VERSION);

  // Init UserStats interface and request UserStats - wait for callback
  SteamUserStats := SteamAPI_ISteamClient_GetISteamUserStats(SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSERSTATS_INTERFACE_VERSION);
  SteamAPI_ISteamUserStats_RequestCurrentStats(SteamUserStats);
  SteamUserStatsCallbackDispatcher := SteamCallbackDispatcher.Create(SteamStatsCallbackID , {$ifdef FPC}@{$endif}OnUserStatsReceived, SizeOf(Steam_UserStatsReceived));
end;

destructor TCastleSteam.Destroy;
begin
  Achievements.Free;
  inherited Destroy;
end;

function SteamLibraryAvailable: Boolean;
begin
  Result := SteamLibrary <> nil;
end;

initialization // Delphi needs initialization before finalization
finalization
  if FSteam <> nil then
  begin
    FSteam.Free;
    if SteamLibraryAvailable then
       SteamAPI_Shutdown();
  end;

end.

