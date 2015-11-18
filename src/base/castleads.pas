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

{ Ads (advertisements) in game (TAds). }
unit CastleAds;

interface

uses Classes, CastleRectangles, CastleStringUtils;

const
  { Test banner ad "unit id". You can use it with @link(TAds.InitializeAdMob) for testing purposes
    (but eventually you want to create your own, to show non-testing ads!).

    From https://developers.google.com/mobile-ads-sdk/docs/admob/android/quick-start }
  TestAdMobBannerUnitId = 'ca-app-pub-3940256099942544/6300978111';

  { Test interstitial ad "unit id". You can use it with @link(TAds.InitializeAdMob) for testing purposes
    (but eventually you want to create your own, to show non-testing ads!).

    From http://stackoverflow.com/questions/12553929/is-there-any-admob-dummy-id and
    https://github.com/googleads/googleads-mobile-android-examples/blob/master/admob/InterstitialExample/app/src/main/res/values/strings.xml }
  TestAdMobInterstitialUnitId = 'ca-app-pub-3940256099942544/1033173712';

type
  TAdType = (atAdMob, atChartboost, atStartApp);

  { Advertisements in game.
    Right now only on Android (does nothing on other platforms,
    as CastleMessaging does nothing on non-Android platforms).

    Usage:

    @orderedList(
      @item(Create an instance of this class (only a single instance allowed).)

      @item(Initialize at least one ad network using one of the
        @code(InitializeXxx) methods. Usually you want to call the initialization
        from @link(TCastleApplication.OnInitializeJavaActivity).)

      @item(Use remaining methods of this class to show / hide ads, like
        @link(ShowInterstitial), @link(ShowBanner), @link(HideBanner).)

      @item(To include the necessary integration code in your Android project,
        you must declare your Android project type as "integrated".
        See https://sourceforge.net/p/castle-engine/wiki/Android%20development/ .

        Also make sure that your game uses @link(TMessaginig) class,
        in particular that it calls @code(Messaging.Update) continuously and exports
        an extra JNI function from CastleMessaging unit in the Android library.
        See @link(TMessaginig) docs for details.)
    )
  }
  TAds = class
  private
    FOnInterstitialShown: TNotifyEvent;
    function MessageReceived(const Received: TCastleStringList): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { Initialize AdMob ads. You need to create the unit ids on AdMob website
      (or use TestAdMobBannerUnitId, TestAdMobInterstitialUnitId for testing).

      TestDeviceIds may be empty, or it may contain a list of devices
      where you always want to see test ads --- even with real (non-test) ad units.
      This is useful when you test ads in your application (with real ad units,
      on a real phone). Paste here the list of your devices
      (see https://developers.google.com/mobile-ads-sdk/docs/admob/android/quick-start ,
      you can see your device ids in "adb logcat" output) in order to avoid
      getting banned for clicking on your own ads.

      Usually called from @link(TCastleApplication.OnInitializeJavaActivity). }
    procedure InitializeAdMob(const BannerUnitId, InterstitialUnitId: string;
      const TestDeviceIds: array of string);

    { Initialize StartApp ads.
      You need to register your game on http://startapp.com/ to get app id.

      Usually called from @link(TCastleApplication.OnInitializeJavaActivity). }
    procedure InitializeStartapp(const AppId: string);

    { Initialize Chartboost ads.
      You need to register your game on http://chartboost.com/ to get app id and signature.

      Usually called from @link(TCastleApplication.OnInitializeJavaActivity). }
    procedure InitializeChartboost(const AppId, AppSignature: string);

    { Show interstitial (full-screen) ad. }
    procedure ShowInterstitial(const AdType: TAdType; const WaitUntilLoaded: boolean);

    { Show banner ad. TODO: right now, this is only implemented with AdMob (google ads). }
    procedure ShowBanner(const HorizontalGravity: THorizontalPosition;
      const VerticalPosition: TVerticalPosition);

    { Hide banner ad. TODO: right now, this is only implemented with AdMob (google ads). }
    procedure HideBanner;

    property OnInterstitialShown: TNotifyEvent read FOnInterstitialShown write FOnInterstitialShown;
  end;

implementation

uses SysUtils,
  CastleUtils, CastleMessaging;

constructor TAds.Create;
begin
  inherited;
  Messaging.OnReceive.Add(@MessageReceived);
end;

destructor TAds.Destroy;
begin
  if Messaging <> nil then
    Messaging.OnReceive.Remove(@MessageReceived);
  inherited;
end;

function TAds.MessageReceived(const Received: TCastleStringList): boolean;
begin
  Result := false;

  if (Received.Count = 2) and
     ( (Received[0] = 'ads-google-interstitial-display') or
       (Received[0] = 'ads-chartboost-interstitial-display') or
       (Received[0] = 'ads-startapp-interstitial-display')
     ) and
     (Received[1] = 'shown') then
  begin
    if Assigned(OnInterstitialShown) then
      OnInterstitialShown(Self);
    Result := true;
  end;
end;

procedure TAds.ShowInterstitial(const AdType: TAdType; const WaitUntilLoaded: boolean);
begin
  case AdType of
    atAdMob:
      if WaitUntilLoaded then
        Messaging.Send(['ads-google-interstitial-display', 'wait-until-loaded']) else
        Messaging.Send(['ads-google-interstitial-display', 'no-wait']);
    atChartboost: Messaging.Send(['ads-chartboost-show-interstitial']);
    atStartApp: Messaging.Send(['ads-startapp-show-interstitial']);
    else raise EInternalError.Create('Unimplemented AdType');
  end;
end;

procedure TAds.InitializeAdMob(const BannerUnitId, InterstitialUnitId: string;
  const TestDeviceIds: array of string);
var
  TestDeviceIdsGlued: string;
begin
  TestDeviceIdsGlued := GlueStrings(TestDeviceIds, ',');
  Messaging.Send(['ads-google-initialize', BannerUnitId, InterstitialUnitId, TestDeviceIdsGlued]);
end;

procedure TAds.InitializeChartboost(const AppId, AppSignature: string);
begin
  Messaging.Send(['ads-chartboost-initialize', AppId, AppSignature]);
end;

procedure TAds.InitializeStartapp(const AppId: string);
begin
  Messaging.Send(['ads-startapp-initialize', AppId]);
end;

procedure TAds.ShowBanner(const HorizontalGravity: THorizontalPosition;
  const VerticalPosition: TVerticalPosition);
const
  { Gravity constants for some messages, for example to indicate ad placement.
    Equal to constants on
    http://developer.android.com/reference/android/view/Gravity.html .
    @groupBegin }
  GravityLeft = $00000003; //< Push object to the left of its container, not changing its size.
  GravityRight = $00000005; //< Push object to the right of its container, not changing its size.
  GravityTop = $00000030; //< Push object to the top of its container, not changing its size.
  GravityBottom = $00000050; //< Push object to the bottom of its container, not changing its size.
  GravityCenterHorizontal = $00000001; //< Place object in the horizontal center of its container, not changing its size.
  GravityCenterVertical = $00000010; //< Place object in the vertical center of its container, not changing its size.
  //GravityNo = 0; //< Constant indicating that no gravity has been set.
  { @groupEnd }
var
  Gravity: Integer;
begin
  Gravity := 0;
  case HorizontalGravity of
    hpLeft: Gravity := Gravity or GravityLeft;
    hpRight: Gravity := Gravity or GravityRight;
    hpMiddle: Gravity := Gravity or GravityCenterHorizontal;
    else raise EInternalError.Create('ShowBanner:HorizontalGravity?');
  end;
  case VerticalPosition of
    vpTop: Gravity := Gravity or GravityTop;
    vpBottom: Gravity := Gravity or GravityBottom;
    vpMiddle: Gravity := Gravity or GravityCenterVertical;
    else raise EInternalError.Create('ShowBanner:VerticalPosition?');
  end;
  Messaging.Send(['ads-google-banner-show', IntToStr(Gravity)]);
end;

procedure TAds.HideBanner;
begin
  Messaging.Send(['ads-google-banner-hide']);
end;

end.
