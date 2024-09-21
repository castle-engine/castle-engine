{
  Copyright 2015-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Integration with game service (Google Play Games or Apple Game Center) (TGameService). }
unit CastleGameService;

{$I castleconf.inc}

interface

uses Classes,
  CastleStringUtils, CastleTimeUtils;

type
  { Event for @link(TGameService.OnPlayerBestScoreReceived). }
  TPlayerBestScoreEvent = procedure (Sender: TObject; const LeaderboardId: string; const Score: Int64) of object;

  { User choice at "save game" dialog displayed by @link(TGameService.ShowSaveGames).
    Used as a parameter for @link(TGameService.OnSaveGameChosen) event. }
  TSaveGameChoice = (sgCancel, sgNew, sgExisting);

  { Event for @link(TGameService.OnSaveGameChosen). }
  TSaveGameChosenEvent = procedure (Sender: TObject; const Choice: TSaveGameChoice; const SaveGameName: string) of object;

  { Event for @link(TGameService.OnSaveGameLoaded).
    @param Success Whether we loaded the savegame successfully.
    @param Content The savegame content, if Success. If not Success, this is the error message. }
  TSaveGameLoadedEvent = procedure (Sender: TObject; const Success: boolean; const Content: string) of object;

  { Status of TGameService sign-in. }
  TGameServiceStatus = (
    // Not signed-in to the service.
    gsSignedOut,
    // Not signed-in to the service, but during signing-in.
    gsSigningIn,
    // Signed-in to the service. All game service methods work now.
    gsSignedIn,
    // During signing-out from the service. You should not call other methods on the service now.
    gsSigningOut
  );

  { Integration with a game service,
    that can be used to show achievements, leaderboards, and store save games "in the cloud".
    This integrates with

    @unorderedList(
      @itemSpacing Compact
      @item Google Play Games on Android
      @item Apple Game Center on iOS.
    )

    Usage:

    @orderedList(
      @item(Include the necessary integration code in your Android / iOS project.

        For Android, add the "google_play_games" service inside CastleEngineManifest.xml.
        See @url(https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/google_play_games/README.adoc
        Android "google_play_games" service docs).

        For iOS, add the "apple_game_center" service inside CastleEngineManifest.xml.
        See @url(https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/ios/services/apple_game_center/README.adoc
        iOS "apple_game_center" service docs).)

      @item(Create an instance of this class.
        Only a single instance of this class makes sense right now,
        both Android and iOS only allow using a single game service connection
        from a given game.)

      @item(Call @link(TGameService.Initialize).
        You usually do it from @link(TCastleApplication.OnInitialize).
        The @link(TGameService.Initialize) must be called before calling any other method
        of this class.)

      @item(Call other methods of this class as you see fit --
        to manage achievements, leaderboards and so on.)

      @item(The user must be "signed in" to the game service.

        The methods that display some user-interface will automatically attempt
        to sign-in the user if needed. These include @link(ShowAchievements),
        @link(ShowLeaderboard), @link(ShowSaveGames).
        So you don't have to do anything before you call them.

        All other methods will @italic(not sign-in the user automatically).
        For example, @link(Achievement) or @link(SubmitScore) or
        @link(SaveGameLoad) or @link(SaveGameSave).
        You should always make sure that the user is signed-in before calling them.
        To do this, pass AutoStartSignInFlow parameter as @true to @link(Initialize)
        or call the @link(RequestSignedIn RequestSignedIn(true)).
        And then wait for the @link(Status) property to change to gsSignedIn
        (you can register @link(OnStatusChanged) to be notified about changes).

        Note that some platforms (like new Google Play Games v2) may automatically
        sign-in the user always. Try not to rely on it, if you want to work
        in all cases (and with all supported platforms).
      )
    )
  }
  TGameService = class(TComponent)
  private
    FOnPlayerBestScoreReceived: TPlayerBestScoreEvent;
    FInitialized,
      FInitializedAutoStartSignInFlow,
      FInitializedSaveGames: boolean;
    FOnSaveGameChosen: TSaveGameChosenEvent;
    FOnSaveGameLoaded: TSaveGameLoadedEvent;
    FOnStatusChanged: TNotifyEvent;
    FStatus: TGameServiceStatus;
    function MessageReceived(const Received: TCastleStringList;
      const ReceivedStream: TMemoryStream): boolean;
    procedure ReinitializeJavaActivity(Sender: TObject);
  protected
    procedure DoSignedInChanged; virtual; deprecated 'use DoStatusChanged';
    procedure DoStatusChanged; virtual;
    procedure DoPlayerBestScoreReceived(const LeaderboardId: string; const Score: Int64); virtual;
    procedure DoSaveGameChosen(const Choice: TSaveGameChoice; const SaveGameName: string); virtual;
    procedure DoSaveGameLoaded(const Success: boolean; const Content: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Connect to the service, optionally try to sign-in player.

      If the player already connected this application with
      Google Play Games (Android) or Apple Game Center (iOS),
      (s)he will be signed-in automatically.
      If not, player will be asked to sign-in (which means asking to accept permissions
      on Android) only if AutoStartSignInFlow.
      So if you want to avoid the initial Google Play Games dialog on Android,
      just always pass AutoStartSignInFlow=false.

      Calling this when already initialized is harmless (will not do anything).

      In theory, you can call this at any point in your application.
      But you probably want to call it early, e.g. from Application.OnInitialize,
      otherwise user will not be signed-in automatically.
      Most calls (like sending the leaderboars or achievements) will be ignored until
      you call this.

      @param(SaveGames Indicates whether you want to use save games feature.
        You can then use @link(ShowSaveGames), @link(SaveGameSave), @link(SaveGameLoad)
        methods. See also the description of this feature in Google Play Games:
        https://developers.google.com/games/services/common/concepts/savedgames.) }
    procedure Initialize(const AutoStartSignInFlow: boolean = true;
      const SaveGames: boolean = false);

    { Was the @link(Initialize) called. }
    property Initialized: boolean read FInitialized;

    { Current status of signing-in. }
    property Status: TGameServiceStatus read FStatus;

    { Is user currently signed-in. Just a shortcut for @code(Status = gsSignedIn) check. }
    function SignedIn: boolean;

    { Report the given achievement as achieved.

      For Google Play Games (Android):
      Use Google Developer Console to define achievements for your game,
      and use their ids with this method. The achievement ids are autogenerated by Google,
      they look like @code(CgkIvYuzpZsIEAIQAQ).

      For Apple Game Center (iOS):
      Use iTunes Connect to define achievements for your game,
      and use their ids with this method. The achievement ids are chosen by you,
      so you can choose something readable like 'boss_defeated'.
    }
    procedure Achievement(const AchievementId: string);

    { Report a score in given leaderboard.
      Use Google Developer Console (Android) or iTunes Connect (iOS)
      to create leaderboards for your game, use their ids here.

      TODO: Not implemented for Apple Game Center (iOS) yet. }
    procedure SubmitScore(const LeaderboardId: string; const Score: Int64);

    { Get the best score, if available, for given leaderboard.

      This may (after some time, asynchronously) call
      OnPlayerBestScoreReceived with this score.

      Note that no error is signalled if loading the score fails for any reason.
      This includes failing to load because user is not connected
      to the game service (this method doesn't connect automatically;
      wait for OnStatusChanged before calling this, if you need).

      TODO: Not implemented for Apple Game Center (iOS) yet. }
    procedure RequestPlayerBestScore(const LeaderboardId: string);

    { Request sign-in or sign-out.
      The operation will be done asynchronously (it will in most cases require
      some network communication).

      Watch for changes to the @link(Status) property.
      You can register an event on @link(OnStatusChanged) to be notified about this. }
    procedure RequestSignedIn(const Value: boolean);

    { Show the user achievements, using the default UI.
      Automatically connects (signs-in) player to game services,
      if not connected yet. }
    procedure ShowAchievements;

    { Show the given leaderboard, using the default UI.
      The leaderboard should be created in the Games configuration
      of the Google Developer Console (Android) or iTunes Connect (iOS).
      Automatically connects (signs-in) player to game services,
      if not connected yet. }
    procedure ShowLeaderboard(const LeaderboardId: string);

    { Show the existing @italic(saved games) stored in the cloud
      for this user. This can be used to offer user a choice in which slot
      to save the game, or from which slot to load the game.

      Note that it's not necessary to use this method to manage savegames.
      E.g. if you want, you can just choose a constant savegame name for your game,
      and use SaveGameSave and SaveGameLoad with this name. This would imply
      that each game service user (Google Play Games user or Apple Game Center user)
      has only one savegame in the cloud for your game.

      The user may choose an existing savegame, or indicate creation
      of a new savegame (if parameter AllowAddButton is @true). In response,
      the callback OnSaveGameChosen will be called.
      It will either

      @orderedList(
        @item(indicate the name of an existing savegame user has chosen,)
        @item(or that user wants to create a new savegame,)
        @item(or that user cancelled the dialog.)
      )

      Using this requires initializing the game service first,
      by calling the @link(Initialize) with @code(SaveGames) parameter set to @true.
      Just like @link(ShowAchievements) and @link(ShowLeaderboard),
      this method automatically signs-in player to game service,
      if not signed-in yet.

      Note that @italic(the OnSaveGameChosen callback
      is not called if the user cancels the sign-in operation), and therefore
      cancels the savegame choice this way. The same remark applies to all
      reasons why sign-in may fail (network problems etc.).
      If this is a problem for your logic (e.g. if you want to "wait"
      until OnSaveGameChosen is called), then never call ShowSaveGames
      when @link(SignedIn) is @false. Instead, call RequestSignedIn, wait until
      SignedIn changed to @true (which may be "never", in case of network problems
      or user cancelling!), and only then call ShowSaveGames.

      TODO: Not implemented for Apple Game Center (iOS) yet.

      @param(Title Dialog title to display.)
      @param(AllowAddButton Enable user to choose "new save game".)
      @param(AllowDelete Enable user to delete savegames from the dialog.)
      @param(MaxNumberOfSaveGamesToShow Maximum number of savegames to show.
        Use -1 to just show all savegames.)
    }
    procedure ShowSaveGames(const Title: string; const AllowAddButton, AllowDelete: boolean;
      const MaxNumberOfSaveGamesToShow: Integer);
      deprecated 'avoid this, as this is unlikely to be implemented on other platforms than Android';

    { Save a savegame identified by the given name.
      See the SaveGameLoad documentation about the conflict resolution
      and valid savegame names.

      The Contents should be a valid UTF-8 string. For implementation
      reasons, you should not save arbitrary binary data this way, for now
      (or it could result in exceptions about being unable to encode/decode UTF-8
      sequences).

      Description and PlayedTime are shown to player in ShowSaveGames.
      PlayedTime may also be used for conflict resolution (if the savegame
      on the server was modified in the meantime, without loading it in this game).

      No callback is called in response now, the game is saved in the background.

      This does not connect player to game service, if it's not connected
      already. An error when saving is not reported back to Pascal, for now.
      (The assumption here is that you will keep a local savegame anyway,
      in case user does not connect to game service. So inability to save
      the savegame to the cloud is not alarming, and does not require any special
      reaction. Please submit a request if you'd like to have a callback
      about it.) }
    procedure SaveGameSave(const SaveGameName, Contents, Description: string;
      const PlayedTime: TFloatTime);

    { Load a savegame identified by the given name.
      If the savegame does not exist, it will be automatically created.

      If the server requires conflict resolution,
      the most recently modified savegame is used with Google Play Games,
      see RESOLUTION_POLICY_MOST_RECENTLY_MODIFIED docs.

      Valid savegame names are defined by Google Play Games:
      Must be between 1 and 100 non-URL-reserved characters (a-z, A-Z, 0-9,
      or the symbols "-", ".", "_", or "~").

      In response, the callback OnSaveGameLoaded will be @italic(always) called,
      with the loaded savegame contents (as a string),
      or the error message.

      This does not connect player to game service (like Google Play Games),
      if it's not connected already.

      An error will be reported to OnSaveGameLoaded callback
      if trying to load fails for any reason.
      This includes the case when loading fails because user is not connected
      to game service yet (this method @italic(does not) connect user
      automatically; wait for OnStatusChanged before calling this method,
      if you need it). }
    procedure SaveGameLoad(const SaveGameName: string);
  published
    { Event called when @link(Status) changed, for example because
      @link(RequestSignedIn) was called, or because user signs-in automatically
      (which may happen if you used AutoStartSignInFlow with @link(Initialize),
      or if user was signed-in in this application previously). }
    property OnStatusChanged: TNotifyEvent read FOnStatusChanged write FOnStatusChanged;

    property OnSignedInChanged: TNotifyEvent read FOnStatusChanged write FOnStatusChanged stored false;
      {$ifdef FPC}deprecated 'use OnStatusChanged';{$endif}

    { Event received in response to @link(RequestPlayerBestScore). }
    property OnPlayerBestScoreReceived: TPlayerBestScoreEvent read FOnPlayerBestScoreReceived write FOnPlayerBestScoreReceived;

    { Event received in response to @link(ShowSaveGames). }
    property OnSaveGameChosen: TSaveGameChosenEvent read FOnSaveGameChosen write FOnSaveGameChosen;

    { Event received in response to @link(SaveGameLoad).
      See TSaveGameLoadedEvent for documentation of parameters. }
    property OnSaveGameLoaded: TSaveGameLoadedEvent read FOnSaveGameLoaded write FOnSaveGameLoaded;
  end;

function GameServiceStatusToStr(const Status: TGameServiceStatus): String;

implementation

uses SysUtils,
  CastleUtils, CastleMessaging, CastleApplicationProperties, CastleLog;

constructor TGameService.Create(AOwner: TComponent);
begin
  inherited;
  Messaging.OnReceive.Add({$ifdef FPC}@{$endif} MessageReceived);
  ApplicationProperties.OnInitializeJavaActivity.Add(
    {$ifdef FPC}@{$endif} ReinitializeJavaActivity);
end;

destructor TGameService.Destroy;
begin
  if Messaging <> nil then
    Messaging.OnReceive.Remove({$ifdef FPC}@{$endif} MessageReceived);
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnInitializeJavaActivity.Remove(
      {$ifdef FPC}@{$endif} ReinitializeJavaActivity);
  inherited;
end;

procedure TGameService.ReinitializeJavaActivity(Sender: TObject);
begin
  { in case Java activity got killed and is created again, reinitialize services }
  if FInitialized then
  begin
    FStatus := gsSignedOut;
    Initialize(FInitializedAutoStartSignInFlow, FInitializedSaveGames);
  end;
end;

procedure TGameService.DoStatusChanged;
begin
  if Assigned(OnStatusChanged) then
    OnStatusChanged(Self);
  {$warnings off} // calling deprecated to keep it working
  DoSignedInChanged;
  {$warnings on}
end;

procedure TGameService.DoSignedInChanged;
begin
end;

function TGameService.SignedIn: boolean;
begin
  Result := FStatus = gsSignedIn;
end;

procedure TGameService.DoPlayerBestScoreReceived(const LeaderboardId: string; const Score: Int64);
begin
  if Assigned(OnPlayerBestScoreReceived) then
    OnPlayerBestScoreReceived(Self, LeaderboardId, Score);
end;

procedure TGameService.DoSaveGameChosen(const Choice: TSaveGameChoice; const SaveGameName: string);
begin
  if Assigned(OnSaveGameChosen) then
    OnSaveGameChosen(Self, Choice, SaveGameName);
end;

procedure TGameService.DoSaveGameLoaded(const Success: boolean; const Content: string);
begin
  if Assigned(OnSaveGameLoaded) then
    OnSaveGameLoaded(Self, Success, Content);
end;

function TGameService.MessageReceived(const Received: TCastleStringList;
  const ReceivedStream: TMemoryStream): boolean;
var
  StatusInt: Int64;
begin
  Result := false;

  if (Received.Count = 3) and
     (Received[0] = 'best-score') then
  begin
    DoPlayerBestScoreReceived(Received[1], StrToInt64(Received[2]));
    Result := true;
  end else

  if (Received.Count = 2) and
     (Received[0] = 'game-service-status') then
  begin
    StatusInt := StrToIntDef(Received[1], -1);
    if (StatusInt >= Ord(Low(TGameServiceStatus))) and
       (StatusInt <= Ord(High(TGameServiceStatus))) then
    begin
      FStatus := TGameServiceStatus(StatusInt);
      DoStatusChanged;
    end else
      WritelnWarning('Invalid game-service-status parameter "%s"', [Received[1]]);
    Result := true;
  end else

  if (Received.Count = 2) and
     (Received[0] = 'chosen-save-game') then
  begin
    DoSaveGameChosen(sgExisting, Received[1]);
    Result := true;
  end;

  if (Received.Count = 1) and
     (Received[0] = 'chosen-save-game-new') then
  begin
    DoSaveGameChosen(sgNew, '');
    Result := true;
  end;

  if (Received.Count = 1) and
     (Received[0] = 'chosen-save-game-cancel') then
  begin
    DoSaveGameChosen(sgCancel, '');
    Result := true;
  end;

  if (Received.Count = 3) and
     (Received[0] = 'save-game-loaded') then
  begin
    DoSaveGameLoaded(StrToBool(Received[1]), Received[2]);
    Result := true;
  end;
end;

procedure TGameService.Initialize(const AutoStartSignInFlow: boolean;
  const SaveGames: boolean);
begin
  { at first Initialize call, remember AutoStartSignInFlow }
  if not FInitialized then
  begin
    FInitializedAutoStartSignInFlow := AutoStartSignInFlow;
    FInitializedSaveGames := SaveGames;
  end;
  FInitialized := true;
  Messaging.Send(['game-service-initialize',
    TMessaging.BoolToStr(AutoStartSignInFlow),
    TMessaging.BoolToStr(SaveGames)
  ]);
end;

procedure TGameService.Achievement(const AchievementId: string);
begin
  { Report invalid AchievementId right now, otherwise Google Play will report
    this error too. It's better to have it error on all platforms. }
  if AchievementId = '' then
    raise Exception.Create('Achievement name cannot be empty');
  Messaging.Send(['achievement', AchievementId]);
end;

procedure TGameService.SubmitScore(const LeaderboardId: string; const Score: Int64);
begin
  Messaging.Send(['submit-score', LeaderboardId, IntToStr(Score)]);
end;

procedure TGameService.RequestPlayerBestScore(const LeaderboardId: string);
begin
  Messaging.Send(['request-player-best-score', LeaderboardId]);
end;

procedure TGameService.RequestSignedIn(const Value: boolean);
begin
  Messaging.Send(['game-service-sign-in', TMessaging.BoolToStr(Value)]);
end;

procedure TGameService.ShowAchievements;
begin
  Messaging.Send(['show', 'achievements']);
end;

procedure TGameService.ShowLeaderboard(const LeaderboardId: string);
begin
  Messaging.Send(['show', 'leaderboard', LeaderboardId]);
end;

procedure TGameService.ShowSaveGames(const Title: string; const AllowAddButton, AllowDelete: boolean;
  const MaxNumberOfSaveGamesToShow: Integer);
begin
  Messaging.Send(['show', 'save-games', Title,
    TMessaging.BoolToStr(AllowAddButton),
    TMessaging.BoolToStr(AllowDelete),
    IntToStr(MaxNumberOfSaveGamesToShow)
  ]);
end;

procedure TGameService.SaveGameLoad(const SaveGameName: string);
begin
  Messaging.Send(['save-game-load', SaveGameName]);
end;

procedure TGameService.SaveGameSave(const SaveGameName, Contents, Description: string;
  const PlayedTime: TFloatTime);
begin
  Messaging.Send(['save-game-save', SaveGameName, Contents, Description, TMessaging.TimeToStr(PlayedTime)]);
end;

{ routines ------------------------------------------------------------------- }

function GameServiceStatusToStr(const Status: TGameServiceStatus): String;
const
  Names: array [TGameServiceStatus] of string =
  (
    'Signed Out',
    'Signing In...',
    'Signed In',
    'Signing Out..'
  );
begin
  Result := Names[Status];
end;

end.
