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

interface

uses CastleStringUtils;

type
  TBestScoreEvent = procedure (const LeaderboardId: string; const Score: Int64) of object;

  { Google Play Game Services (achievements, leaderboards) integration.
    Right now only on Android (will simply do nothing on other platforms).

    Usage:

    @orderedList(
      @item(Create an instance of it (only a single instance allowed).)
      @item(Use this to manage Google Games achievements, leaderboards and so on.)
      @item(
        To include the necessary integration code in your Android project,
        you must declare your Android project type as "integrated".
        See https://sourceforge.net/p/castle-engine/wiki/Android%20development/ .
      )
    ) }
  TGooglePlayGames = class
  private
    FOnBestScoreReceived: TBestScoreEvent;
    FSignedIn: boolean;
    function MessageReceived(const Received: TCastleStringList): boolean;
  public
    constructor Create;
    destructor Destroy; override;

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
  CastleUtils, CastleMessaging;

constructor TGooglePlayGames.Create;
begin
  inherited;
  Messaging.OnReceive.Add(@MessageReceived);
end;

destructor TGooglePlayGames.Destroy;
begin
  if Messaging <> nil then
    Messaging.OnReceive.Remove(@MessageReceived);
  inherited;
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
