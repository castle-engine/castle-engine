{
  Copyright 2015-2015 Michalis Kamburelis.

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
  CastleStringUtils;

type
  TBestScoreEvent = procedure (const LeaderboardId: string; const Score: Int64) of object;

  { Google Play Game Services (achievements, leaderboards) integration.
    Right now only on Android (will simply do nothing on other platforms).

    Usage:

    @orderedList(
      @item(Create an instance of it (only a single instance allowed).)
      @item(Call @link(TGooglePlayGames.Initialize) at some point.
        Usually from @link(TCastleApplication.OnInitialize).
        User will be automatically asked to sign-in to Google Play then.)
      @item(Use this to manage Google Games achievements, leaderboards and so on.)
      @item(To include the necessary integration code in your Android project,
        declare your Android project type as "integrated" with
        the "google_play_games" component inside CastleEngineManifest.xml.
        See https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine .)
    ) }
  TGooglePlayGames = class(TComponent)
  private
    FOnBestScoreReceived: TBestScoreEvent;
    FSignedIn, FInitialized: boolean;
    function MessageReceived(const Received: TCastleStringList): boolean;
    procedure ReinitializeJavaActivity(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize;

    property OnBestScoreReceived: TBestScoreEvent read FOnBestScoreReceived write FOnBestScoreReceived;

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
      This will (may) eventually (after some network delay) call
      OnBestScoreReceived event. }
    procedure RequestBestScore(const LeaderboardId: string);

    { Request sign-in or sign-out.
      This will (may) eventually (after some network delay) change
      the @link(SignedIn) value. }
    procedure RequestSignedIn(const Value: boolean);

    procedure ShowAchievements;

    procedure ShowLeaderboard(const LeaderboardId: string);
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
    Initialize;
  end;
end;

function TGooglePlayGames.MessageReceived(const Received: TCastleStringList): boolean;
begin
  Result := false;

  if (Received.Count = 3) and
     (Received[0] = 'best-score') then
  begin
    if Assigned(OnBestScoreReceived) then
      OnBestScoreReceived(Received[1], StrToInt64(Received[2]));
    Result := true;
  end else

  if (Received.Count = 2) and
     (Received[0] = 'google-sign-in-status') and
     (Received[1] = 'true') then
  begin
    FSignedIn := true;
    Result := true;
  end else

  if (Received.Count = 2) and
     (Received[0] = 'google-sign-in-status') and
     (Received[1] = 'false') then
  begin
    FSignedIn := false;
    Result := true;
  end;
end;

procedure TGooglePlayGames.Initialize;
begin
  FInitialized := true;
  Messaging.Send(['google-play-games-initialize']);
end;

procedure TGooglePlayGames.Achievement(const AchievementId: string);
begin
  Messaging.Send(['achievement', AchievementId]);
end;

procedure TGooglePlayGames.SubmitScore(const LeaderboardId: string; const Score: Int64);
begin
  Messaging.Send(['submit-score', LeaderboardId, IntToStr(Score)]);
end;

procedure TGooglePlayGames.RequestBestScore(const LeaderboardId: string);
begin
  Messaging.Send(['get-best-score', LeaderboardId]);
end;

procedure TGooglePlayGames.RequestSignedIn(const Value: boolean);
begin
  Messaging.Send(['google-sign-in', Iff(Value, 'true', 'false')]);
end;

procedure TGooglePlayGames.ShowAchievements;
begin
  Messaging.Send(['show', 'achievements']);
end;

procedure TGooglePlayGames.ShowLeaderboard(const LeaderboardId: string);
begin
  Messaging.Send(['show', 'leaderboard', LeaderboardId]);
end;

end.
