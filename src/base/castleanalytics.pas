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

{ Analytics (TAnalytics). }
unit CastleAnalytics;

interface

uses Classes, CastleTimeUtils;

type
  { Gathering analytics through Google Analytics https://www.google.com/analytics/
    and/or Game Analytics http://www.gameanalytics.com/ .
    Right now they only work on Android, through Java APIs.

    Usage:

    @orderedList(
      @item(Create an instance of it (only a single instance allowed).)
      @item(Initialize at least one analytics backend using the @code(InitializeXxx)
        method.)
      @item(Use the remaining methods, like @link(Event), to report events
        in your app.)
      @item(
        To include the necessary integration code in your Android project,
        you must declare your Android project type as "integrated".
        See https://sourceforge.net/p/castle-engine/wiki/Android%20development/ .

        Also make sure that your game uses @link(TMessaginig) class,
        in particular that it calls @code(Messaging.Update) continuously and exports
        an extra JNI function from CastleMessaging unit in the Android library.
        See @link(TMessaginig) docs for details.)
    )
  }
  TAnalytics = class
  public
    { Initialize Google Analytics https://www.google.com/analytics/ .
      Usually called from @link(TCastleApplication.OnInitializeJavaActivity). }
    procedure InitializeGoogleAnalytics(const AnalyticsPropertyId: string);

    { Initialize Game Analytics http://www.gameanalytics.com/ .
      Usually called from @link(TCastleApplication.OnInitializeJavaActivity). }
    procedure InitializeGameAnalytics(const GameKey, SecretKey: string);

    { Send to analytics view of the screen, e.g. when user switches
      between UI states. }
    procedure ScreenView(const ScreenName: string);

    { Send to analytics a general event. }
    procedure Event(const Category, Action, ALabel: string; const Value: Int64);

    { Send to analytics a timing event. }
    procedure Timing(const Category, AVariable, ALabel: string; const Time: TFloatTime);
  end;

implementation

uses SysUtils,
  CastleMessaging;

procedure TAnalytics.InitializeGoogleAnalytics(const AnalyticsPropertyId: string);
begin
  Messaging.Send(['google-analytics-initialize', AnalyticsPropertyId]);
end;

procedure TAnalytics.InitializeGameAnalytics(const GameKey, SecretKey: string);
begin
  Messaging.Send(['game-analytics-initialize', GameKey, SecretKey]);
end;

procedure TAnalytics.ScreenView(const ScreenName: string);
begin
  Messaging.Send(['analytics-send-screen-view', ScreenName]);
end;

procedure TAnalytics.Event(const Category, Action, ALabel: string; const Value: Int64);
begin
  Messaging.Send(['analytics-send-event', Category, Action, ALabel, IntToStr(Value)]);
end;

procedure TAnalytics.Timing(const Category, AVariable, ALabel: string; const Time: TFloatTime);
begin
  Messaging.Send(['analytics-send-timing', Category, AVariable, ALabel, IntToStr(Trunc(Time * 1000))]);
end;

end.
