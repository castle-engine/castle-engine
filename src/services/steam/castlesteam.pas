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

{ Do not warn at PAnsiChar(String) typecasts here,
  they are OK -- we cast strings to 8-bit for Steam API usage. }
{$ifndef FPC}
  {$warn SUSPICIOUS_TYPECAST off}
{$endif}

interface

uses Classes,
  CastleInternalSteamApi;

type
  TAppId = CastleInternalSteamApi.TAppId;

  { Integration with Steam.
    See @url(https://castle-engine.io/steam Steam and Castle Game Engine documentation)
    for usage.
    Create an instance of this class (passing to the constructor
    the Steam application id), observe properties like @link(Enabled)
    and @link(UserStatsReceived), and call methods like @link(SetAchievement)
    to interact with Steam API. }
  TCastleSteam = class
  strict private
    FAppId: TAppId;
    FEnabled: Boolean;
    FAchievements: TStrings;
    FUserStatsReceived: Boolean;
    FOnUserStatsReceived: TNotifyEvent;
    StoreStats: Boolean;
    SteamClient: Pointer;
    //SteamUser: Pointer; // We aren't using it right now, but it works
    SteamUserStats: Pointer;
    SteamUserHandle: HSteamUser;
    SteamPipeHandle: HSteamPipe;
    procedure CallbackUserStatsReceived(P: PUserStatsReceived);
    procedure GetAchievements;
    { Makes a warning.

      For now if some operation failed, we log an error using @link(WriteLnWarning).
      If it will be useful in the future, we could also raise an exception
      here, to make the issue more prominent.
      But it seems just logging the error is more standard,
      it's also the practice of SteamWorks examples. This way failure
      to communicate with Steam doesn't stop the game, it just logs a warning. }
    procedure SteamError(const ErrorMsg: String);
    { Runs callbacks, performs saving of user stats.
      According to SteamWorks documentation you should run this
      at least 10 times a second, better if every frame. }
    procedure Update(Sender: TObject);
  public
    { Connect to Steam and initialize everything.

      @orderedList(
        @item(It tries to connect to Steam API and checks if the game was run
          through Steam or through exe file.

          In the latter case the game will
          automatically restart through Steam.
          See @url(https://castle-engine.io/steam Steam and Castle Game Engine documentation)
          for a description how does the Steam
          integration behave, when it restarts the game.
        )

        @item(Will request user stats from Steam.

          Only once user stats are received, you can use achievements.
          Observe @link(UserStatsReceived) or use event @link(OnUserStatsReceived)
          to know when it happens.
        )
      )
    }
    constructor Create(const AAppId: TAppId);
    destructor Destroy; override;

    { Steam application id, given when creating this. }
    property AppId: TAppId read FAppId;

    { Do we have Steam integration available.

      If @true, this means that Steam dynamic library was found,
      and we don't need to restart the application.
      See https://castle-engine.io/steam for more information.

      This is the normal state of things when the game is run through Steam.
      It means that all features of this class work as they should. }
    property Enabled: Boolean read FEnabled;

    { Achievement names for this game (read-only).
      These are IDs of achievements, can be used in other calls
      like @link(SetAchievement) or @link(GetAchievement).
      This field is initialized when @link(UserStatsReceived) becomes @true,
      it is @nil before. }
    property Achievements: TStrings read FAchievements;

    { Have we received user stats from Steam.
      Before this is @true, methods of this class dealing with user stats
      (like achievements) don't do anything. }
    property UserStatsReceived: Boolean read FUserStatsReceived;

    { We have received user stats from Steam.
      Right before calling this event, @link(UserStatsReceived) changed to @true
      and @link(Achievements) have been filled with achievement names.
      From now on you can use Steam features that depend on it,
      like @link(Achievements). }
    property OnUserStatsReceived: TNotifyEvent
      read FOnUserStatsReceived write FOnUserStatsReceived;

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

    { Show Steam overlay "progress towards achievement" e.g. "Wins 33/100".

      Don't use this when achievement is already achieved.
      Doing so will only result in an error from Steam, visible as a warning in logs:
      "Failed to SteamAPI_ISteamUserStats_IndicateAchievementProgress".

      Calling this with CurrentProgress >= MaxProgress @italic(does not)
      mark the achievement as achieved. It only shows the progress in the overlay.
      You still have to call @link(SetAchievement) to make it achieved. }
    procedure IndicateAchievementProgress(const AchievementId: String;
      const CurrentProgress, MaxProgress: UInt32);

    { 2 digit ISO 3166-1-alpha-2 format country code this client
      is running in (as looked up via an IP-to-location database) e.g "US" or "UK". }
    function Country: String;

    { Is the Steam overlay running and the user can access it.
      The overlay process could take a few seconds to
      start & hook the game process, so this function will initially return false
      while the overlay is loading. }
    function OverlayEnabled: Boolean;

    { Is Steam running in VR mode. }
    function RunningInVR: Boolean;

    { Is currently running on the Steam Deck device. }
    function RunningOnSteamDeck: Boolean;

    { Build id of this app. }
    function BuildId: Integer;

    { Checks if the user owns the DLC and if the DLC is installed. }
    function DlcInstalled(const DlcAppID: TAppId): Boolean;

    { Current game language. }
    function Language: String;
  end;

implementation

uses SysUtils, CTypes,
  CastleLog, CastleUtils, CastleApplicationProperties;

procedure WarningHook(nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;
begin
  WriteLnLog('Steam Warning: "%s" (Severity: %d)', [pchDebugText^, NSeverity]);
end;

{ TCastleSteam --------------------------------------------------------------- }

constructor TCastleSteam.Create(const AAppId: TAppId);

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
          WriteLnLog('The app was run through exe directly (not by clicking "Play" in Steam), restarting through Steam.');
          Halt(0);
        end else
        begin
          FEnabled := true;
          WriteLnLog('The app was started through Steam (or runs in developer mode with steam_appid.txt), Steam integration enabled OK.');
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
  FAppId := AAppId;

  StoreStats := false;
  FUserStatsReceived := false; // waiting for callback

  InitializeSteamLibrary;
  CheckEnabledAndRestart;
  InitialSteamCalls;

  ApplicationProperties.OnUpdate.Add({$ifdef FPC}@{$endif} Update);
end;

destructor TCastleSteam.Destroy;
begin
  FreeAndNil(FAchievements);
  if Enabled then
    SteamAPI_Shutdown();
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnUpdate.Remove({$ifdef FPC}@{$endif} Update);
  inherited;
end;

procedure TCastleSteam.CallbackUserStatsReceived(P: PUserStatsReceived);
begin
  WriteLnLog('Steam', 'Received UserStats from Steam');
  FUserStatsReceived := true;
  GetAchievements;
  if Assigned(OnUserStatsReceived) then
    OnUserStatsReceived(Self);
end;

procedure TCastleSteam.GetAchievements;
var
  NumAchievements: UInt32;
  I: Integer;
begin
  if not Enabled then
    Exit;

  FreeAndNil(FAchievements);
  FAchievements := TStringList.Create;

  NumAchievements := SteamAPI_ISteamUserStats_GetNumAchievements(SteamUserStats);
  if NumAchievements > 0 then
    for I := 0 to NumAchievements - 1 do
      FAchievements.Add(SteamAPI_ISteamUserStats_GetAchievementName(SteamUserStats, I));
  WriteLnLog('Steam Achievements: %d', [Achievements.Count]);
end;

procedure TCastleSteam.SteamError(const ErrorMsg: String);
begin
  WriteLnWarning(ErrorMsg);
end;

const
  SUserStatsNotReceived = 'User stats not received yet from Steam (wait for TCastleSteam.UserStatsReceived or observe TCastleSteam.OnUserStatsReceived).';

procedure TCastleSteam.SetAchievement(const AchievementId: String);
begin
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    if not SteamAPI_ISteamUserStats_SetAchievement(SteamUserStats, PAnsiChar(AchievementId)) then
      SteamError('Failed to SteamAPI_ISteamUserStats_SetAchievement');
    StoreStats := true;
  end else
    SteamError('SetAchievement failed. ' + SUserStatsNotReceived);
end;

function TCastleSteam.GetAchievement(const AchievementId: String): Boolean;
var
  CAchieved: TSteamBool;
begin
  Result := false;
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    if SteamAPI_ISteamUserStats_GetAchievement(SteamUserStats,
        PAnsiChar(AchievementId), @CAchieved) then
    begin
      Result := CAchieved;
    end else
      SteamError('Failed to SteamAPI_ISteamUserStats_GetAchievement');
  end else
    SteamError('GetAchievement failed. ' + SUserStatsNotReceived);
end;

procedure TCastleSteam.ClearAchievement(const AchievementId: String);
begin
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    if not SteamAPI_ISteamUserStats_ClearAchievement(SteamUserStats, PAnsiChar(AchievementId)) then
      SteamError('Failed to SteamAPI_ISteamUserStats_ClearAchievement');
    StoreStats := true;
  end else
    SteamError('ClearAchievement failed. ' + SUserStatsNotReceived);
end;

procedure TCastleSteam.ClearAllAchievements;
var
  S: String;
begin
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    for S in Achievements do
      ClearAchievement(S)
  end else
    SteamError('ClearAllAchievements failed. ' + SUserStatsNotReceived);
end;

procedure TCastleSteam.IndicateAchievementProgress(const AchievementId: String;
  const CurrentProgress, MaxProgress: UInt32);
begin
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    if not SteamAPI_ISteamUserStats_IndicateAchievementProgress(SteamUserStats, PAnsiChar(AchievementId), CurrentProgress, MaxProgress) then
      SteamError('Failed to SteamAPI_ISteamUserStats_IndicateAchievementProgress');
    StoreStats := true; // not really necessary it seems
  end else
    SteamError('IndicateAchievementProgress failed. ' + SUserStatsNotReceived);
end;

procedure TCastleSteam.Update(Sender: TObject);

  { Request callbacks from Steam API if any pending.

    We don't use SteamAPI_RunCallbacks, as it
    - Requires quite hacky code, to simulate C++ class VMT, with a specially
      crafted record and assembler help.
      See
      https://github.com/castle-engine/castle-engine/blob/af09fba831c76db45dde7e0d230c1304dffbd4d3/src/services/steam/castleinternalsteamcallback.pas
      that we removed in
      https://github.com/castle-engine/castle-engine/commit/28d9a2e4558b0b7f66c77c5e93f47bddec186285 .
    - Crashes on Linux/x86_64 (maybe because of the above).

    Following the Steamworks docs recommendation, we use manual dispatching,
    see https://partner.steamgames.com/doc/sdk/api#manual_dispatch
    and example code at SteamAPI_ManualDispatch_Init.
  }
  procedure SteamRunCallbacks;
  { Extra logging of unhandled Steam callbacks (it's normal that we have some). }
  {.$define CASTLE_DEBUG_STEAM_CALLBACKS}
  var
    Callback: TCallbackMsg;
    PCallCompleted: PSteamAPICallCompleted;
    PTmpCallResult: Pointer;
    BFailed: TSteamBool;
  begin
    SteamAPI_ManualDispatch_RunFrame(SteamPipeHandle);
    while SteamAPI_ManualDispatch_GetNextCallback(SteamPipeHandle, @Callback) do
    begin
      // Look at callback.m_iCallback to see what kind of callback it is,
      // and dispatch to appropriate handler(s)
      case Callback.m_iCallback of
        TSteamAPICallCompleted.k_iCallback:
          begin
            // TODO: remove this warning once this code is tested
            WritelnWarning('Steam', 'SteamAPICallCompleted callback: TODO untested handling');

            // Check for dispatching API call results
            PCallCompleted := PSteamAPICallCompleted(Callback.m_pubParam);
            PTmpCallResult := GetMem(Callback.m_cubParam);
            if SteamAPI_ManualDispatch_GetAPICallResult(SteamPipeHandle,
                PCallCompleted^.m_hAsyncCall, PTmpCallResult, Callback.m_cubParam,
                Callback.m_iCallback, @BFailed) then
            begin
              { Dispatch the call result to the registered handler(s) for the
                call identified by pCallCompleted^.m_hAsyncCall

                Note (CGE): The piece of code handling SteamAPICallCompleted
                is adjusted from the example code in C API about
                SteamAPI_ManualDispatch_GetNextCallback.
                But right now, we don't have any need for this,
                and the log below never happens in our Steam usage so far.
              }
              {$ifdef CASTLE_DEBUG_STEAM_CALLBACKS}
              WritelnLog('Steam', 'Dispatch the call result to the handlers for m_iCallback %d, m_hAsyncCall %d', [
                PCallCompleted^.m_iCallback,
                PCallCompleted^.m_hAsyncCall
              ]);
              {$endif}
            end;
            FreeMem(PTmpCallResult);
          end;
        TUserStatsReceived.k_iCallback:
          begin
            CallbackUserStatsReceived(PUserStatsReceived(Callback.m_pubParam));
          end;
        else
          begin
            {$ifdef CASTLE_DEBUG_STEAM_CALLBACKS}
            WritelnLog('Steam', 'Callback m_iCallback %d, we ignore it now', [
              Callback.m_iCallback
            ]);
            {$endif}
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
  if UserStatsReceived and StoreStats then
    if SteamAPI_ISteamUserStats_StoreStats(SteamUserStats) then // repeat it every Update until success
      StoreStats := false;
end;

function TCastleSteam.Country: String;
begin
  if not Enabled then
    Exit('');
  Result := SteamAPI_ISteamUtils_GetIPCountry(SteamAPI_SteamUtils());
end;

function TCastleSteam.OverlayEnabled: Boolean;
begin
  if not Enabled then
    Exit(false);
  Result := SteamAPI_ISteamUtils_IsOverlayEnabled(SteamAPI_SteamUtils());
end;

function TCastleSteam.RunningInVR: Boolean;
begin
  if not Enabled then
    Exit(false);
  Result := SteamAPI_ISteamUtils_IsSteamRunningInVR(SteamAPI_SteamUtils());
end;

function TCastleSteam.RunningOnSteamDeck: Boolean;
begin
  if not Enabled then
    Exit(false);
  Result := SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck(SteamAPI_SteamUtils());
end;

function TCastleSteam.BuildId: Integer;
begin
  if not Enabled then
    Exit(0);
  Result := SteamAPI_ISteamApps_GetAppBuildId(SteamAPI_SteamApps());
end;

function TCastleSteam.DlcInstalled(const DlcAppID: TAppId): Boolean;
begin
  if not Enabled then
    Exit(false);
  Result := SteamAPI_ISteamApps_BIsDlcInstalled(SteamAPI_SteamApps(), DlcAppID);
end;

function TCastleSteam.Language: String;
begin
  if not Enabled then
    Exit('');
  Result := SteamAPI_ISteamApps_GetCurrentGameLanguage(SteamAPI_SteamApps());
end;

end.
