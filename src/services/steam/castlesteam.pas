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

{$define CASTLE_DEBUG_STEAM_API_TESTING}

uses Classes, CTypes, SysUtils, {SteamTypes,}
  {$ifndef fpc}System.Generics.Collections,{$else}Contnrs,Generics.Collections,{$endif}
  CastleInternalSteamApi;

type
  TAppId = CastleInternalSteamApi.TAppId;
  TIconSize = (IconSmall, IconMedium, IconLarge);
  TAchievementChanged = (AchievedChanged, IconChanged, ImageChanged);
  TSteamAchievement = Class;

  TAchievementUpdatedEvent = procedure(AValue: TSteamAchievement; const WhatChanged: TAchievementChanged) of object;

  TSteamBitmap = class
  strict private
    FIsValid: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FBPP: Integer;
    FImage: PByteArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetImageFormat(const ImWidth, ImHeight, BytesPerPixel: Integer);
    function GetImageMemorySize(): UInt64;
    procedure SetImage(const AImage: PByteArray);
    property IsValid: Boolean read FIsValid write FIsValid;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property BPP: Integer read FBPP;
    property Image: PByteArray read FImage;
  end;

  TCastleSteam = class;

  TSteamAchievement = Class
    strict private
      FOwner: TObject;
      FAchId: UInt32;
      FApiId: String;
      FName: String;
      FDesc: String;
      FHidden: Boolean;
      FAchieved: Boolean;
      FDoneDate: TDateTime;
      FIcon: CInt;
      FOnAchievementUpdated: TAchievementUpdatedEvent;
      procedure SetAchieved(const AChecked: Boolean);
    protected
      procedure Populate(SteamUserStats: Pointer; AchievementId: UInt32);
    public
      constructor Create(AOwner: TCastleSteam);
      destructor Destroy; override;
      function GetIcon(SteamUserStats: Pointer; AchievementId: UInt32): CInt;
      property ApiId: String read FApiId;
      property Name: String read FName;
      property Desc: String read FDesc;
      property Hidden: Boolean read FHidden;
      property Achieved: Boolean read FAchieved write SetAchieved;
      property DoneDate: TDateTime read FDoneDate;
      property Icon: CInt read FIcon write FIcon;
      property OnAchievementUpdated: TAchievementUpdatedEvent
        read FOnAchievementUpdated write FOnAchievementUpdated;

  end;

  TSteamAchievementList = {$ifdef fpc}specialize{$endif} TObjectList<TSteamAchievement>;

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
    FUserID: CUserID;
    FAchievements: TSteamAchievementList;
    FUserStatsReceived: Boolean;
    FOnUserStatsReceived: TNotifyEvent;
    StoreStats: Boolean;
    // Pointers for ISteam....
    // SteamApps: Pointer;
    SteamClient: Pointer;
    SteamFriends: Pointer;
    SteamInput: Pointer;
    SteamUser: Pointer;
    SteamUserStats: Pointer;
    SteamUtils: Pointer;
    SteamUserHandle: HSteamUser;
    SteamPipeHandle: HSteamPipe;
    procedure CallbackUserStatsReceived(P: PUserStatsReceived);
    procedure CallbackUserAchievementIconFetched(P: PUserAchievementIconFetched);
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
    { Callback may be set after event is available so trigger it if ready
      As of 1.61 Stats are no longer async so this callback would not happen
      without this setter }
    procedure SetOnUserStatsReceived(const AValue: TNotifyEvent);
  public
    function GetFriendImageHandle(const FriendID: CUserID; const Size: TIconSize = IconLarge): CInt;
    function GetSteamBitmap(const ImageHandle: CInt): TSteamBitmap;
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

    // Public Interface Access
    property ISteamUserStats: Pointer read SteamUserStats;

    { Achievement names for this game (read-only).
      These are IDs of achievements, can be used in other calls
      like @link(SetAchievement) or @link(GetAchievement).
      This field is initialized when @link(UserStatsReceived) becomes @true,
      it is @nil before. }
    property Achievements: TSteamAchievementList read FAchievements;

    { Have we received user stats from Steam.
      Before this is @true, methods of this class dealing with user stats
      (like achievements) don't do anything. }
    property UserStatsReceived: Boolean read FUserStatsReceived;

    { The currently logged in Steam User's ID }
    property UserId: CUserID read FUserId;

    { We have received user stats from Steam.
      Right before calling this event, @link(UserStatsReceived) changed to @true
      and @link(Achievements) have been filled with achievement names.
      From now on you can use Steam features that depend on it,
      like @link(Achievements). }
    property OnUserStatsReceived: TNotifyEvent
      read FOnUserStatsReceived write SetOnUserStatsReceived;

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

uses DateUtils,
  CastleLog, CastleUtils, CastleApplicationProperties;

procedure WarningHook(nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;
var
  DebugTextAnsi: AnsiString;
begin
  DebugTextAnsi := pchDebugText;
  WriteLnLog('Steam Warning: "%s" (Severity: %d)', [DebugTextAnsi, NSeverity]);
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
      {$if defined(USE_TESTING_API)}
      if SteamAPI_InitFlat(Nil) = k_ESteamAPIInitResult_OK then
      {$else}
      if SteamAPI_Init() then
      {$endif}
      begin
        {$if defined(USE_TESTING_API)}
        WriteLnLog('SteamAPI with USE_TESTING_API using ' + SteamLibraryName);
        {$else}
        WriteLnLog('SteamAPI using ' + SteamLibraryName);
        {$endif}
        WriteLnLog('SteamAPI_Init successful');

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

    // Init SteamUser
    SteamUser := SteamAPI_ISteamClient_GetISteamUser(
       SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSER_INTERFACE_VERSION);

    // Init SteamApps - returns nil?
//    SteamApps := SteamAPI_ISteamClient_GetISteamApps(
//      SteamClient, SteamUserHandle, SteamPipeHandle, STEAMAPPS_INTERFACE_VERSION);

    // Init SteamFriends
    SteamFriends := SteamAPI_ISteamClient_GetISteamFriends(
      SteamClient, SteamUserHandle, SteamPipeHandle, STEAMFRIENDS_INTERFACE_VERSION);

    // Init SteamUtils
    SteamUtils := SteamAPI_ISteamClient_GetISteamUtils(
       SteamClient, SteamPipeHandle, STEAMUTILS_INTERFACE_VERSION);

    // Init SteamInput
    SteamInput := SteamAPI_ISteamClient_GetISteamInput(
       SteamClient, SteamUserHandle, SteamPipeHandle, STEAMINPUT_INTERFACE_VERSION);


    // Init SteamUserStats and request UserStats (will wait for callback, handled in Update)
    SteamUserStats := SteamAPI_ISteamClient_GetISteamUserStats(
      SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSERSTATS_INTERFACE_VERSION);

    FUserId := SteamAPI_ISteamUser_GetSteamID(SteamUser);
    {$if not defined(USE_TESTING_API)}
    SteamAPI_ISteamUserStats_RequestCurrentStats(SteamUserStats);
    {$else}
    if SteamUserStats <> Nil then
      begin
        GetAchievements;
        FUserStatsReceived := true;
        if Assigned(OnUserStatsReceived) then
          OnUserStatsReceived(Self);
      end;
    {$endif}

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

procedure TCastleSteam.CallbackUserAchievementIconFetched(
  P: PUserAchievementIconFetched);
var
  S: String;
  F, I : Integer;
begin
  if P{$ifdef fpc}^{$endif}.m_nIconHandle = 0 then
    begin
      {$if defined(CASTLE_DEBUG_STEAM_API_TESTING)}
      WriteLnLog('Steam', 'No Icon Available');
      {$endif}
      Exit;
    end;

  S := String(P{$ifdef fpc}^{$endif}.m_rgchAchievementName);
  if Length(S) > 0 then
    begin
      F := -1;
      // Absolutely horrible linear search
      for I := 0 to FAchievements.Count - 1 do
        begin
        if FAchievements[I].ApiId = S then
          begin
            F := I;
            Break;
          end;
        end;

      if F <> -1 then
        begin
          {$if defined(CASTLE_DEBUG_STEAM_API_TESTING)}
          WriteLnLog('Image ==>', 'Fetched UserAchievementIcon from Steam for %s with Handle %d',[S, (P^).m_nIconHandle]);
          {$endif}
          FAchievements[F].Icon := P{$ifdef fpc}^{$endif}.m_nIconHandle;
          if Assigned(FAchievements[F].OnAchievementUpdated) then
            FAchievements[F].OnAchievementUpdated(FAchievements[F], ImageChanged);
        end;

    end;
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
  SteamAchievement: TSteamAchievement;
begin
  if not Enabled then
    Exit;

  FAchievements := TSteamAchievementList.Create;

  NumAchievements := SteamAPI_ISteamUserStats_GetNumAchievements(SteamUserStats);
  if NumAchievements > 0 then
    for I := 0 to NumAchievements - 1 do
      begin
        SteamAchievement := TSteamAchievement.Create(Self);
        SteamAchievement.Populate(SteamUserStats, I);
        FAchievements.Add(SteamAchievement);
      end;
  WriteLnLog('Steam Achievements: %d', [Achievements.Count]);
end;

function TCastleSteam.GetSteamBitmap(const ImageHandle: CInt): TSteamBitmap;
var
  ImWidth, ImHeight: Integer;
  R: TSteamBool;
  Buf: Pointer;
  BufSize: Integer;
begin
  Result := nil;
  if ImageHandle = 0 then
      Exit;

  R := SteamAPI_ISteamUtils_GetImageSize(SteamUtils, ImageHandle, @ImWidth, @ImHeight);
  if R then
    begin
      {$if defined(CASTLE_DEBUG_STEAM_API_TESTING)}
//      WriteLnLog('Steam', 'GetImageSize : Width : %d, Height : %d', [ImWidth, ImHeight]);
      {$endif}
      Result := TSteamBitmap.Create;
      Result.SetImageFormat(ImWidth, ImHeight, 4);
      try
        BufSize := Result.GetImageMemorySize;
        Buf := GetMem(BufSize);
        if SteamAPI_ISteamUtils_GetImageRGBA(SteamUtils, ImageHandle, Buf, BufSize) then
          begin
            Result.SetImage(Buf);
            Result.IsValid := True;
          end;
      finally
      end;
    end
  {$if defined(CASTLE_DEBUG_STEAM_API_TESTING)}
  else
    WriteLnLog('GetImageSize Failed for %d in GetStreamBitmap', [ImageHandle]);
  {$else}
  ;
  {$endif}
end;

function TCastleSteam.GetFriendImageHandle(const FriendID: CUserID; const Size: TIconSize): CInt;
begin
  case Size of
    IconSmall:
      Result := SteamAPI_ISteamFriends_GetSmallFriendAvatar(SteamFriends, FriendID);
    IconMedium:
      Result := SteamAPI_ISteamFriends_GetMediumFriendAvatar(SteamFriends, FriendID);
    IconLarge:
      Result := SteamAPI_ISteamFriends_GetLargeFriendAvatar(SteamFriends, FriendID);
  else
    Result := 0;
  end;
end;

procedure TCastleSteam.SteamError(const ErrorMsg: String);
begin
  WriteLnWarning(ErrorMsg);
end;

const
  SUserStatsNotReceived = 'User stats not received yet from Steam (wait for TCastleSteam.UserStatsReceived or observe TCastleSteam.OnUserStatsReceived).';

procedure TCastleSteam.SetAchievement(const AchievementId: String);
var
  AchievementIdAnsi: AnsiString;
begin
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    AchievementIdAnsi := AnsiString(AchievementId);
    if not SteamAPI_ISteamUserStats_SetAchievement(SteamUserStats, PAnsiChar(AchievementIdAnsi)) then
      SteamError('Failed to SteamAPI_ISteamUserStats_SetAchievement');
    StoreStats := true;
  end else
    SteamError('SetAchievement failed. ' + SUserStatsNotReceived);
end;

procedure TCastleSteam.SetOnUserStatsReceived(const AValue: TNotifyEvent);
begin
  FOnUserStatsReceived := AValue;
  if Assigned(FOnUserStatsReceived) and FUserStatsReceived then
    FOnUserStatsReceived(Self);
end;

function TCastleSteam.GetAchievement(const AchievementId: String): Boolean;
var
  CAchieved: TSteamBool;
  AchievementIdAnsi: AnsiString;
begin
  Result := false;
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    AchievementIdAnsi := AnsiString(AchievementId);
    if SteamAPI_ISteamUserStats_GetAchievement(SteamUserStats,
        PAnsiChar(AchievementIdAnsi), @CAchieved) then
    begin
      Result := CAchieved;
    end else
      SteamError('Failed to SteamAPI_ISteamUserStats_GetAchievement');
  end else
    SteamError('GetAchievement failed. ' + SUserStatsNotReceived);
end;

procedure TCastleSteam.ClearAchievement(const AchievementId: String);
var
  AchievementIdAnsi: AnsiString;
begin
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    AchievementIdAnsi := AnsiString(AchievementId);
    if not SteamAPI_ISteamUserStats_ClearAchievement(SteamUserStats, PAnsiChar(AchievementIdAnsi)) then
      SteamError('Failed to SteamAPI_ISteamUserStats_ClearAchievement');
    StoreStats := true;
  end else
    SteamError('ClearAchievement failed. ' + SUserStatsNotReceived);
end;

procedure TCastleSteam.ClearAllAchievements;
begin

  if not Enabled then
    Exit;
  if UserStatsReceived then
    FAchievements.Clear;

end;

procedure TCastleSteam.IndicateAchievementProgress(const AchievementId: String;
  const CurrentProgress, MaxProgress: UInt32);
var
  AchievementIdAnsi: AnsiString;
begin
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    AchievementIdAnsi := AnsiString(AchievementId);
    if not SteamAPI_ISteamUserStats_IndicateAchievementProgress(SteamUserStats,
       PAnsiChar(AchievementIdAnsi), CurrentProgress, MaxProgress) then
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
  {$define CASTLE_DEBUG_STEAM_CALLBACKS}
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
      WritelnWarning('Steam', 'Got Callback : ' + IntToStr(Callback.m_iCallback));
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
            {$ifdef CASTLE_DEBUG_STEAM_CALLBACKS}
            WritelnLog('Steam', 'Handle Stats Received (m_iCallback : %d)', [Callback.m_iCallback]);
            {$endif}
            CallbackUserStatsReceived(PUserStatsReceived(Callback.m_pubParam));
          end;
        TUserAchievementIconFetched.k_iCallback:
          begin
            if(SizeOf(TUserAchievementIconFetched) <> Callback.m_cubParam) then
              WritelnLog('Callbacks', 'TUserAchievementIconFetched Size = %d, should be %d)', [SizeOf(TUserAchievementIconFetched), Callback.m_cubParam])
            else
              CallbackUserAchievementIconFetched(PUserAchievementIconFetched(Callback.m_pubParam));
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
  Result := String(SteamAPI_ISteamUtils_GetIPCountry(SteamUtils));
end;

function TCastleSteam.OverlayEnabled: Boolean;
begin
  if not Enabled then
    Exit(false);
  Result := SteamAPI_ISteamUtils_IsOverlayEnabled(SteamUtils);
end;

function TCastleSteam.RunningInVR: Boolean;
begin
  if not Enabled then
    Exit(false);
  Result := SteamAPI_ISteamUtils_IsSteamRunningInVR(SteamUtils);
end;

function TCastleSteam.RunningOnSteamDeck: Boolean;
begin
  if not Enabled then
    Exit(false);
  Result := SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck(SteamUtils);
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
  Result := String(SteamAPI_ISteamApps_GetCurrentGameLanguage(SteamAPI_SteamApps()));
end;

{ TSteamAchievement }

constructor TSteamAchievement.Create(AOwner: TCastleSteam);
begin
  Inherited Create;
  FOwner := AOwner;
end;

destructor TSteamAchievement.Destroy;
begin
  inherited;
end;

function TSteamAchievement.GetIcon(SteamUserStats: Pointer; AchievementId: UInt32): CInt;
begin
  Result := SteamAPI_ISteamUserStats_GetAchievementIcon(SteamUserStats, PAnsiChar(AnsiString(FApiId)));
  if Result <> 0 then
    begin
      {$if defined(CASTLE_DEBUG_STEAM_API_TESTING)}
      WritelnLog('GetAchievementIcon returned Icon Handle %d for %s', [Result, FApiId]);
      {$endif}
    end
  else
    WritelnLog('GetAchievementIcon Pending');
end;

procedure TSteamAchievement.Populate(SteamUserStats: Pointer; AchievementId: UInt32);
var
  pchName: PAnsiChar;
  pchHidden: PAnsiChar;
  bAchieved: TSteamBool;
  uDate: UInt32;
begin
  FAchId := AchievementId;
  pchName := SteamAPI_ISteamUserStats_GetAchievementName(SteamUserStats, AchievementId);
  FApiId := String(pchName);
  FName := String(SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute(SteamUserStats, pchName, 'name'));
  FDesc := String(SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute(SteamUserStats, pchName, 'desc'));
  pchHidden := SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute(SteamUserStats, pchName, 'hidden');
  if CompareStr(String(pchHidden), '1') = 0 then
    FHidden := True
  else
    FHidden := False;
  SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime(SteamUserStats, pchName, @bAchieved, @uDate);
  FAchieved := bAchieved;
  FDoneDate := UnixToDateTime(uDate);
  FIcon := GetIcon(SteamUserStats, AchievementId);

end;

procedure TSteamAchievement.SetAchieved(const AChecked: Boolean);
var
  NIcon: CInt;
begin
  if not(FOwner is TCastleSteam) then
    Exit;

  FAchieved := AChecked;
  if FAchieved then
    TCastleSteam(FOwner).SetAchievement(FApiId)
  else
    TCastleSteam(FOwner).ClearAchievement(FApiId);
  NIcon := GetIcon(TCastleSteam(FOwner).ISteamUserStats, FIcon);
  if (NIcon <> FIcon) then
    begin
      FIcon := NIcon;
      if Assigned(OnAchievementUpdated) then
        OnAchievementUpdated(Self, IconChanged);
    end;
end;

{ TSteamBitmap }

constructor TSteamBitmap.Create;
begin

end;

destructor TSteamBitmap.Destroy;
begin
  if Assigned(FImage) then
    FreeMem(Fimage);
  inherited;
end;

function TSteamBitmap.GetImageMemorySize: UInt64;
begin
  Result := FWidth * FHeight * FBPP;
end;

procedure TSteamBitmap.SetImage(const AImage: PByteArray);
begin
  FImage := AImage;
end;

procedure TSteamBitmap.SetImageFormat(const ImWidth, ImHeight, BytesPerPixel: Integer);
begin
  FWidth := ImWidth;
  FHeight := ImHeight;
  FBPP := BytesPerPixel;
end;

end.

