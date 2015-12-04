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
  TAdNetwork = (anAdMob, anChartboost, anStartApp, anHeyzap);

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
        declare your Android project type as "integrated" with
        the appropriate components (admob, chartboost, startapp, heyzap...)
        inside CastleEngineManifest.xml .
        See https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine .)
    )
  }
  TAds = class(TComponent)
  private
    type
      TAdNetworkHandler = class abstract
      strict private
        FOnInterstitialShown: TNotifyEvent;
        function MessageReceived(const Received: TCastleStringList): boolean;
      strict protected
        function GravityToInt(
          const HorizontalGravity: THorizontalPosition;
          const VerticalGravity: TVerticalPosition): Integer;
      public
        constructor Create;
        destructor Destroy; override;
        property OnInterstitialShown: TNotifyEvent
          read FOnInterstitialShown write FOnInterstitialShown;
        class function Name: string; virtual; abstract;
        procedure ShowBanner(
          const HorizontalGravity: THorizontalPosition;
          const VerticalGravity: TVerticalPosition); virtual;
        procedure HideBanner; virtual;
        procedure ShowInterstitial(const WaitUntilLoaded: boolean;
          const Static: boolean); virtual;
        procedure StartTestActivity; virtual;
      end;

      TAdMobHandler = class(TAdNetworkHandler)
      public
        constructor Create(const BannerUnitId, InterstitialUnitId: string;
          const TestDeviceIds: array of string);
        class function Name: string; override;
        procedure ShowBanner(
          const HorizontalGravity: THorizontalPosition;
          const VerticalGravity: TVerticalPosition); override;
        procedure HideBanner; override;
        procedure ShowInterstitial(const WaitUntilLoaded: boolean;
          const Static: boolean); override;
      end;

      TChartboostHandler = class(TAdNetworkHandler)
      public
        constructor Create(const AppId, AppSignature: string);
        class function Name: string; override;
        procedure ShowInterstitial(const WaitUntilLoaded: boolean;
          const Static: boolean); override;
      end;

      TStartappHandler = class(TAdNetworkHandler)
      public
        constructor Create(const AppId: string);
        class function Name: string; override;
        procedure ShowInterstitial(const WaitUntilLoaded: boolean;
          const Static: boolean); override;
      end;

      THeyzapHandler = class(TAdNetworkHandler)
      public
        constructor Create(const PublisherId: string);
        class function Name: string; override;
        procedure ShowBanner(
          const HorizontalGravity: THorizontalPosition;
          const VerticalGravity: TVerticalPosition); override;
        procedure HideBanner; override;
        procedure ShowInterstitial(const WaitUntilLoaded: boolean;
          const Static: boolean); override;
        procedure StartTestActivity; override;
      end;
    var
    FNetworks: array [TAdNetwork] of TAdNetworkHandler;
    FOnInterstitialShown: TNotifyEvent;
    procedure InterstitialShownNotification(Sender: TObject);
  public
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

    { Initialize Heyzap ads.
      You need to register your game on https://www.heyzap.com/ to get publisher id.

      Usually called from @link(TCastleApplication.OnInitializeJavaActivity). }
    procedure InitializeHeyzap(const PublisherId: string);

    { Show interstitial (full-screen) ad. }
    procedure ShowInterstitial(const AdNetwork: TAdNetwork;
      const WaitUntilLoaded: boolean;
      const Static: boolean = true);

    { Show banner ad.
      Banners are not supported by all ad networks (only AdMob and Heyzap now),
      in case they are unsupported this call is silently ignored. }
    procedure ShowBanner(const AdNetwork: TAdNetwork;
      const HorizontalGravity: THorizontalPosition;
      const VerticalGravity: TVerticalPosition);

    { Hide banner ad.
      Banners are not supported by all ad networks (only AdMob and Heyzap now),
      in case they are unsupported this call is silently ignored. }
    procedure HideBanner(const AdNetwork: TAdNetwork);

    { Show Heyzap activity to test various ad networks integrations.
      This is for now supported only by anHeyzap. }
    procedure StartTestActivity(const AdNetwork: TAdNetwork);

    property OnInterstitialShown: TNotifyEvent
      read FOnInterstitialShown write FOnInterstitialShown;
  end;

implementation

uses SysUtils,
  CastleUtils, CastleMessaging;

{ TAdNetworkHandler ---------------------------------------------------------- }

constructor TAds.TAdNetworkHandler.Create;
begin
  inherited;
  Messaging.OnReceive.Add(@MessageReceived);
end;

destructor TAds.TAdNetworkHandler.Destroy;
begin
  if Messaging <> nil then
    Messaging.OnReceive.Remove(@MessageReceived);
  inherited;
end;

function TAds.TAdNetworkHandler.MessageReceived(const Received: TCastleStringList): boolean;
begin
  Result := false;

  if (Received.Count = 2) and
     (Received[0] = 'ads-' + Name + '-interstitial-display') and
     (Received[1] = 'shown') then
  begin
    if Assigned(OnInterstitialShown) then
      OnInterstitialShown(Self);
    Result := true;
  end;
end;

procedure TAds.TAdNetworkHandler.ShowBanner(
  const HorizontalGravity: THorizontalPosition;
  const VerticalGravity: TVerticalPosition);
begin
end;

procedure TAds.TAdNetworkHandler.HideBanner;
begin
end;

procedure TAds.TAdNetworkHandler.ShowInterstitial(const WaitUntilLoaded: boolean;
  const Static: boolean);
begin
  { if the network doesn't support showing interstitial, pretend it's shown,
    in case user code waits for OnInterstitialShown. }
  if Assigned(OnInterstitialShown) then
    OnInterstitialShown(Self);
end;

procedure TAds.TAdNetworkHandler.StartTestActivity;
begin
end;

function TAds.TAdNetworkHandler.GravityToInt(
  const HorizontalGravity: THorizontalPosition;
  const VerticalGravity: TVerticalPosition): Integer;
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
begin
  Result := 0;
  case HorizontalGravity of
    hpLeft: Result := Result or GravityLeft;
    hpRight: Result := Result or GravityRight;
    hpMiddle: Result := Result or GravityCenterHorizontal;
    else raise EInternalError.Create('ShowBanner:HorizontalGravity?');
  end;
  case VerticalGravity of
    vpTop: Result := Result or GravityTop;
    vpBottom: Result := Result or GravityBottom;
    vpMiddle: Result := Result or GravityCenterVertical;
    else raise EInternalError.Create('ShowBanner:VerticalGravity?');
  end;
end;

{ TAdMobHandler -------------------------------------------------------------- }

constructor TAds.TAdMobHandler.Create(const BannerUnitId, InterstitialUnitId: string;
  const TestDeviceIds: array of string);
var
  TestDeviceIdsGlued: string;
begin
  inherited Create;
  TestDeviceIdsGlued := GlueStrings(TestDeviceIds, ',');
  Messaging.Send(['ads-' + Name + '-initialize', BannerUnitId, InterstitialUnitId, TestDeviceIdsGlued]);
end;

class function TAds.TAdMobHandler.Name: string;
begin
  Result := 'admob';
end;

procedure TAds.TAdMobHandler.ShowBanner(
  const HorizontalGravity: THorizontalPosition;
  const VerticalGravity: TVerticalPosition);
begin
  Messaging.Send(['ads-' + Name + '-banner-show',
    IntToStr(GravityToInt(HorizontalGravity, VerticalGravity))]);
end;

procedure TAds.TAdMobHandler.HideBanner;
begin
  Messaging.Send(['ads-' + Name + '-banner-hide']);
end;

procedure TAds.TAdMobHandler.ShowInterstitial(const WaitUntilLoaded, Static: boolean);
begin
  if WaitUntilLoaded then
    Messaging.Send(['ads-' + Name + '-interstitial-display', 'wait-until-loaded']) else
    Messaging.Send(['ads-' + Name + '-interstitial-display', 'no-wait']);
end;

{ TChartboostHandler --------------------------------------------------------- }

constructor TAds.TChartboostHandler.Create(const AppId, AppSignature: string);
begin
  inherited Create;
  Messaging.Send(['ads-' + Name + '-initialize', AppId, AppSignature]);
end;

class function TAds.TChartboostHandler.Name: string;
begin
  Result := 'chartboost';
end;

procedure TAds.TChartboostHandler.ShowInterstitial(const WaitUntilLoaded, Static: boolean);
begin
  Messaging.Send(['ads-' + Name + '-show-interstitial']);
end;

{ TStartappHandler --------------------------------------------------------- }

constructor TAds.TStartappHandler.Create(const AppId: string);
begin
  inherited Create;
  Messaging.Send(['ads-' + Name + '-initialize', AppId]);
end;

class function TAds.TStartappHandler.Name: string;
begin
  Result := 'startapp';
end;

procedure TAds.TStartappHandler.ShowInterstitial(const WaitUntilLoaded, Static: boolean);
begin
  Messaging.Send(['ads-' + Name + '-show-interstitial']);
end;

{ THeyzapHandler --------------------------------------------------------- }

constructor TAds.THeyzapHandler.Create(const PublisherId: string);
begin
  inherited Create;
  Messaging.Send(['ads-' + Name + '-initialize', PublisherId]);
end;

class function TAds.THeyzapHandler.Name: string;
begin
  Result := 'heyzap';
end;

procedure TAds.THeyzapHandler.ShowBanner(
  const HorizontalGravity: THorizontalPosition;
  const VerticalGravity: TVerticalPosition);
begin
  Messaging.Send(['ads-' + Name + '-banner-show',
    IntToStr(GravityToInt(HorizontalGravity, VerticalGravity))]);
end;

procedure TAds.THeyzapHandler.HideBanner;
begin
  Messaging.Send(['ads-' + Name + '-banner-hide']);
end;

procedure TAds.THeyzapHandler.ShowInterstitial(const WaitUntilLoaded, Static: boolean);
begin
  if Static then
    Messaging.Send(['ads-' + Name + '-show-interstitial', 'static']) else
    Messaging.Send(['ads-' + Name + '-show-interstitial', 'video']);
end;

procedure TAds.THeyzapHandler.StartTestActivity;
begin
  Messaging.Send(['ads-' + Name + '-start-test-activity']);
end;

{ TAds ----------------------------------------------------------------------- }

destructor TAds.Destroy;
var
  AdNetwork: TAdNetwork;
begin
  for AdNetwork := Low(AdNetwork) to High(AdNetwork) do
    FreeAndNil(FNetworks[AdNetwork]);
  inherited;
end;

procedure TAds.InterstitialShownNotification(Sender: TObject);
begin
  if Assigned(OnInterstitialShown) then
    OnInterstitialShown(Self);
end;

procedure TAds.InitializeAdMob(const BannerUnitId, InterstitialUnitId: string;
  const TestDeviceIds: array of string);
begin
  if FNetworks[anAdMob] <> nil then
    FreeAndNil(FNetworks[anAdMob]);
  FNetworks[anAdMob] := TAdMobHandler.Create(BannerUnitId, InterstitialUnitId, TestDeviceIds);
  FNetworks[anAdMob].OnInterstitialShown := @InterstitialShownNotification;
end;

procedure TAds.InitializeStartapp(const AppId: string);
begin
  if FNetworks[anStartApp] <> nil then
    FreeAndNil(FNetworks[anStartApp]);
  FNetworks[anStartApp] := TStartAppHandler.Create(AppId);
  FNetworks[anStartApp].OnInterstitialShown := @InterstitialShownNotification;
end;

procedure TAds.InitializeChartboost(const AppId, AppSignature: string);
begin
  if FNetworks[anChartboost] <> nil then
    FreeAndNil(FNetworks[anChartboost]);
  FNetworks[anChartboost] := TChartboostHandler.Create(AppId, AppSignature);
  FNetworks[anChartboost].OnInterstitialShown := @InterstitialShownNotification;
end;

procedure TAds.InitializeHeyzap(const PublisherId: string);
begin
  if FNetworks[anHeyzap] <> nil then
    FreeAndNil(FNetworks[anHeyzap]);
  FNetworks[anHeyzap] := THeyzapHandler.Create(PublisherId);
  FNetworks[anHeyzap].OnInterstitialShown := @InterstitialShownNotification;
end;

procedure TAds.ShowInterstitial(const AdNetwork: TAdNetwork;
  const WaitUntilLoaded, Static: boolean);
begin
  if FNetworks[AdNetwork] <> nil then
    FNetworks[AdNetwork].ShowInterstitial(WaitUntilLoaded, Static) else
  { if the network is not initialized, pretend it's shown,
    in case user code waits for OnInterstitialShown. }
  if Assigned(OnInterstitialShown) then
    OnInterstitialShown(Self);
end;

procedure TAds.ShowBanner(const AdNetwork: TAdNetwork;
  const HorizontalGravity: THorizontalPosition;
  const VerticalGravity: TVerticalPosition);
begin
  if FNetworks[AdNetwork] <> nil then
    FNetworks[AdNetwork].ShowBanner(HorizontalGravity, VerticalGravity);
end;

procedure TAds.HideBanner(const AdNetwork: TAdNetwork);
begin
  if FNetworks[AdNetwork] <> nil then
    FNetworks[AdNetwork].HideBanner;
end;

procedure TAds.StartTestActivity(const AdNetwork: TAdNetwork);
begin
  if FNetworks[AdNetwork] <> nil then
    FNetworks[AdNetwork].StartTestActivity;
end;

end.
