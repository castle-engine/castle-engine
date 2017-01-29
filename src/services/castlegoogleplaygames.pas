{
  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Google Play Game Services integration (TGooglePlayGames). }
unit CastleGooglePlayGames;

{$I castleconf.inc}

interface

uses Classes,
  CastleStringUtils, CastleTimeUtils;

type
  { Event for @link(TGooglePlayGames.OnPlayerBestScoreReceived). }
  TPlayerBestScoreEvent = procedure (Sender: TObject; const LeaderboardId: string; const Score: Int64) of object;

  { User choice at "save game" dialog displayed by @link(TGooglePlayGames.ShowSaveGames).
    Used as a parameter for @link(TGooglePlayGames.OnSaveGameChosen) event. }
  TSaveGameChoice = (sgCancel, sgNew, sgExisting);

  { Event for @link(TGooglePlayGames.OnSaveGameChosen). }
  TSaveGameChosenEvent = procedure (Sender: TObject; const Choice: TSaveGameChoice; const SaveGameName: string) of object;

  { Event for @link(TGooglePlayGames.OnSaveGameLoaded).
    @param Success Whether we loaded the savegame successfully.
    @param Content The savegame content, if Success. If not Success, this is the error message. }
  TSaveGameLoadedEvent = procedure (Sender: TObject; const Success: boolean; const Content: string) of object;

  { Google Play Games integration (achievements, leaderboards, save games).
    Right now only on Android (will simply do nothing on other platforms).

    Usage:

    @orderedList(
      @item(Create an instance of it (only a single instance allowed).)
      @item(Call @link(TGooglePlayGames.Initialize) at some point.
        Usually from @link(TCastleApplication.OnInitialize).)
      @item(Use this to manage Google Games achievements, leaderboards and so on.)
      @item(To include the necessary integration code in your Android project,
        declare your Android project type as "integrated" with
        the "google_play_games" component inside CastleEngineManifest.xml.
        See https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine .)
    ) }
  TGooglePlayGames = class(TComponent)
  private
    FOnPlayerBestScoreReceived: TPlayerBestScoreEvent;
    FSignedIn, FInitialized,
      FInitializedAutoStartSignInFlow,
      FInitializedSaveGames: boolean;
    FOnSaveGameChosen: TSaveGameChosenEvent;
    FOnSaveGameLoaded: TSaveGameLoadedEvent;
    FOnSignedInChanged: TNotifyEvent;
    function MessageReceived(const Received: TCastleStringList): boolean;
    procedure ReinitializeJavaActivity(Sender: TObject);
  protected
    procedure DoSignedInChanged; virtual;
    procedure DoPlayerBestScoreReceived(const LeaderboardId: string; const Score: Int64); virtual;
    procedure DoSaveGameChosen(const Choice: TSaveGameChoice; const SaveGameName: string); virtual;
    procedure DoSaveGameLoaded(const Success: boolean; const Content: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Connect to Google, try to sign-in player.
      If the player already connected this application with Google Play Games,
      (s)he will be signed-in automatically, without any dialogs.
      If the player did not connect with Google Play Games,
      (s)he will be asked to sign-in (accept permissions etc.) only if
      AutoStartSignInFlow.

      Calling this when already initialized is harmless (will not do anything).

      In theory, you can call this at any point in your application.
      But you probably want to call it early, e.g. from Application.OnInitialize,
      otherwise user will not be signed-in automatically.
      Most calls (like sending or showing the leaderboars) will be ignored until
      you call this. If you want to avoid the initial Google Games dialog, just
      pass AutoStartSignInFlow=false.

      @param(SaveGames Indicates whether you want to use save games feature.
        You can then use @link(ShowSaveGames), @link(SaveGameSave), @link(SaveGameLoad)
        methods. See also the description of this feature in Google:
        https://developers.google.com/games/services/common/concepts/savedgames.) }
    procedure Initialize(const AutoStartSignInFlow: boolean = true;
      const SaveGames: boolean = false);

    { Was the @link(Initialize) called. }
    property Initialized: boolean read FInitialized;

    { Is user currently signed-in. }
    property SignedIn: boolean read FSignedIn;

    { Report the given achievement as achieved.
      Use Google Developer Console to create achievements for your game,
      copy their ids from there. }
    procedure Achievement(const AchievementId: string);

    { Report a score in given leaderboard.
      Use Google Developer Console to create leaderboards for your game,
      copy their ids from there. }
    procedure SubmitScore(const LeaderboardId: string; const Score: Int64);

    { Get the best score, if available, for given leaderboard.
      This will (may) eventually (after some unspecified time) call
      OnPlayerBestScoreReceived event with this score.

      Note that no error is signalled if loading the score fails for any reason.
      This includes failing to load because user is not connected
      to Google Play Games (this method doesn't connect automatically;
      wait for OnSignedInChanged before calling this, if you need). }
    procedure RequestPlayerBestScore(const LeaderboardId: string);

    { Request sign-in or sign-out.
      This will (may) eventually (after some network delay) change
      the @link(SignedIn) value, also calling OnSignedInChanged event. }
    procedure RequestSignedIn(const Value: boolean);

    { Show the user achievements, using the default UI.
      Automatically connects player to Google Play Games,
      if not connected yet. }
    procedure ShowAchievements;

    { Show the given leaderboard, using the default UI.
      The leaderboard should be created in the Games configuration
      in the Google Play Developer Console.
      Automatically connects player to Google Play Games,
      if not connected yet. }
    procedure ShowLeaderboard(const LeaderboardId: string);

    { Show the existing @italic(saved games) stored in Google Play Games
      for this user. This can be used to offer user a choice in which slot
      to save the game, or from which slot to load the game.

      Note that it's not necessary to use this method to manage savegames.
      E.g. if you want, you can just choose a constant savegame name for your game,
      and use SaveGameSave and SaveGameLoad with this name. This would mean
      that each Google user has only one savegame in the cloud for your game.

      Using this requires being logged to Google Play Games,
      and the @link(Initialize) method must have been called
      with @code(SaveGames) parameter set to @true.

      The user may choose an existing savegame, or indicate creation
      of a new savegame (if parameter AllowAddButton is @true). In response,
      the callback OnSaveGameChosen will be called.
      It will either

      @orderedList(
        @item(indicate the name of an existing savegame user has chosen,)
        @item(or that user wants to create a new savegame,)
        @item(or that user cancelled the dialog.)
      )

      Just like @link(ShowAchievements) and @link(ShowLeaderboard),
      this method automatically connects player to Google Play Games,
      if not connected yet.

      Note that @italic(the OnSaveGameChosen callback
      is not called if the user cancels the sign-in operation), and therefore
      cancels the savegame choice this way. The same remark applies to all
      reasons why sign-in may fail (network problems etc.).
      If this is a problem for your logic (e.g. if you want to "wait"
      until OnSaveGameChosen is called), then never call ShowSaveGames
      when SignedIn is @false. Instead, call RequestSignedIn, wait until
      SignedIn changed to @true (which may be "never", in case of network problems
      or user cancelling!), for example using OnSignedInChanged event,
      and only then call ShowSaveGames.

      @param(Title Dialog title to display.)
      @param(AllowAddButton Enable user to choose "new save game".)
      @param(AllowDelete Enable user to delete savegames from the dialog.)
      @param(MaxNumberOfSaveGamesToShow Maximum number of savegames to show.
        Use -1 to just show all savegames.)
    }
    procedure ShowSaveGames(const Title: string; const AllowAddButton, AllowDelete: boolean;
      const MaxNumberOfSaveGamesToShow: Integer);

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

      No callback is called in response, the game is saved in the background.

      This does not connect player to Google Play Games, if it's not connected
      already. An error when saving is not reported back to Pascal, for now.
      (The assumption here is that you will keep a local savegame anyway,
      in case user does not connect to Google Play Games. So inability to save
      the savegame to the cloud is not alarming, and does not require any special
      reaction. Please submit a request if you'd like to have a callback
      about it.) }
    procedure SaveGameSave(const SaveGameName, Contents, Description: string;
      const PlayedTime: TFloatTime);

    { Load a savegame identified by the given name.
      If the savegame does not exist, it will be automatically created.

      If the server requires conflict resolution,
      the savegame with longest playtime is used (internally: we use
      RESOLUTION_POLICY_LONGEST_PLAYTIME flag with Google Play Games).
      For this to work, you must provide a proper PlayedTime parameter
      when saving all your savegames through @link(SaveGameSave).

      Valid savegame names are defined by Google Play Games:
      Must be between 1 and 100 non-URL-reserved characters (a-z, A-Z, 0-9,
      or the symbols "-", ".", "_", or "~").

      In response, the callback OnSaveGameLoaded will be @italic(always) called,
      with the loaded savegame contents (as a string),
      or the error message.

      This does not connect player to Google Play Games, if it's not connected
      already.

      An error will be reported to OnSaveGameLoaded callback
      if trying to load fails for any reason.
      This includes the case when loading fails because user is not connected
      to Google Play Games yet (this method @italic(does not) connect user
      automatically; wait for OnSignedInChanged before calling this method,
      if you need it). }
    procedure SaveGameLoad(const SaveGameName: string);
  published
    { Event called when @link(SignedIn) changed, for example because
      @link(RequestSignedIn) was called, or because user signs-in automatically
      (which may happen if you used AutoStartSignInFlow with @link(Initialize),
      or if user was signed-in in this application previously). }
    property OnSignedInChanged: TNotifyEvent read FOnSignedInChanged write FOnSignedInChanged;
    { Event received in response to @link(RequestPlayerBestScore). }
    property OnPlayerBestScoreReceived: TPlayerBestScoreEvent read FOnPlayerBestScoreReceived write FOnPlayerBestScoreReceived;
    { Event received in response to @link(ShowSaveGames). }
    property OnSaveGameChosen: TSaveGameChosenEvent read FOnSaveGameChosen write FOnSaveGameChosen;
    { Event received in response to @link(SaveGameLoad).
      See TSaveGameLoadedEvent for documentation of parameters. }
    property OnSaveGameLoaded: TSaveGameLoadedEvent read FOnSaveGameLoaded write FOnSaveGameLoaded;
  end;

implementation

uses SysUtils,
  CastleUtils, CastleMessaging, CastleApplicationProperties;

constructor TGooglePlayGames.Create(AOwner: TComponent);
begin
  inherited;
  Messaging.OnReceive.Add(@MessageReceived);
  ApplicationProperties.OnInitializeJavaActivity.Add(@ReinitializeJavaActivity);
end;

destructor TGooglePlayGames.Destroy;
begin
  if Messaging <> nil then
    Messaging.OnReceive.Remove(@MessageReceived);
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnInitializeJavaActivity.Remove(@ReinitializeJavaActivity);
  inherited;
end;

procedure TGooglePlayGames.ReinitializeJavaActivity(Sender: TObject);
begin
  { in case Java activity got killed and is created again, reinitialize components }
  if FInitialized then
  begin
    FSignedIn := false;
    Initialize(FInitializedAutoStartSignInFlow, FInitializedSaveGames);
  end;
end;

procedure TGooglePlayGames.DoSignedInChanged;
begin
  if Assigned(OnSignedInChanged) then
    OnSignedInChanged(Self);
end;

procedure TGooglePlayGames.DoPlayerBestScoreReceived(const LeaderboardId: string; const Score: Int64);
begin
  if Assigned(OnPlayerBestScoreReceived) then
    OnPlayerBestScoreReceived(Self, LeaderboardId, Score);
end;

procedure TGooglePlayGames.DoSaveGameChosen(const Choice: TSaveGameChoice; const SaveGameName: string);
begin
  if Assigned(OnSaveGameChosen) then
    OnSaveGameChosen(Self, Choice, SaveGameName);
end;

procedure TGooglePlayGames.DoSaveGameLoaded(const Success: boolean; const Content: string);
begin
  if Assigned(OnSaveGameLoaded) then
    OnSaveGameLoaded(Self, Success, Content);
end;

function TGooglePlayGames.MessageReceived(const Received: TCastleStringList): boolean;
begin
  Result := false;

  if (Received.Count = 3) and
     (Received[0] = 'best-score') then
  begin
    DoPlayerBestScoreReceived(Received[1], StrToInt64(Received[2]));
    Result := true;
  end else

  if (Received.Count = 2) and
     (Received[0] = 'google-sign-in-status') and
     (Received[1] = 'true') then
  begin
    if not FSignedIn then
    begin
      FSignedIn := true;
      DoSignedInChanged;
    end;
    Result := true;
  end else

  if (Received.Count = 2) and
     (Received[0] = 'google-sign-in-status') and
     (Received[1] = 'false') then
  begin
    if FSignedIn then
    begin
      FSignedIn := false;
      DoSignedInChanged;
    end;
    Result := true;
  end;

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

procedure TGooglePlayGames.Initialize(const AutoStartSignInFlow: boolean;
  const SaveGames: boolean);
begin
  { at first Initialize call, remember AutoStartSignInFlow }
  if not FInitialized then
  begin
    FInitializedAutoStartSignInFlow := AutoStartSignInFlow;
    FInitializedSaveGames := SaveGames;
  end;
  FInitialized := true;
  Messaging.Send(['google-play-games-initialize',
    TMessaging.BoolToStr(AutoStartSignInFlow),
    TMessaging.BoolToStr(SaveGames)
  ]);
end;

procedure TGooglePlayGames.Achievement(const AchievementId: string);
begin
  Messaging.Send(['achievement', AchievementId]);
end;

procedure TGooglePlayGames.SubmitScore(const LeaderboardId: string; const Score: Int64);
begin
  Messaging.Send(['submit-score', LeaderboardId, IntToStr(Score)]);
end;

procedure TGooglePlayGames.RequestPlayerBestScore(const LeaderboardId: string);
begin
  Messaging.Send(['request-player-best-score', LeaderboardId]);
end;

procedure TGooglePlayGames.RequestSignedIn(const Value: boolean);
begin
  Messaging.Send(['google-sign-in', TMessaging.BoolToStr(Value)]);
end;

procedure TGooglePlayGames.ShowAchievements;
begin
  Messaging.Send(['show', 'achievements']);
end;

procedure TGooglePlayGames.ShowLeaderboard(const LeaderboardId: string);
begin
  Messaging.Send(['show', 'leaderboard', LeaderboardId]);
end;

procedure TGooglePlayGames.ShowSaveGames(const Title: string; const AllowAddButton, AllowDelete: boolean;
  const MaxNumberOfSaveGamesToShow: Integer);
begin
  Messaging.Send(['show', 'save-games', Title,
    TMessaging.BoolToStr(AllowAddButton),
    TMessaging.BoolToStr(AllowDelete),
    IntToStr(MaxNumberOfSaveGamesToShow)
  ]);
end;

procedure TGooglePlayGames.SaveGameLoad(const SaveGameName: string);
begin
  Messaging.Send(['save-game-load', SaveGameName]);
end;

procedure TGooglePlayGames.SaveGameSave(const SaveGameName, Contents, Description: string;
  const PlayedTime: TFloatTime);
begin
  Messaging.Send(['save-game-save', SaveGameName, Contents, Description, TMessaging.TimeToStr(PlayedTime)]);
end;

end.
