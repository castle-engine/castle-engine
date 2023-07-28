{
  Copyright 2015-2022 Michalis Kamburelis.

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

{$I castleconf.inc}

interface

uses Classes, CastleRectangles, CastleStringUtils;

const
  { Test banner ad "unit id". You can use it with @link(TAds.InitializeAdMob) for testing purposes
    (but eventually you want to create your own, to show non-testing ads!).

    From https://developers.google.com/admob/android/test-ads }
  TestAdMobBannerUnitId = 'ca-app-pub-3940256099942544/6300978111';

  { Test interstitial static ad "unit id". You can use it with @link(TAds.InitializeAdMob) for testing purposes
    (but eventually you want to create your own, to show non-testing ads!).

    From https://developers.google.com/admob/android/test-ads }
  TestAdMobInterstitialUnitId = 'ca-app-pub-3940256099942544/1033173712';

  { Test interstitial video ad "unit id". You can use it with @link(TAds.InitializeAdMob) for testing purposes
    (but eventually you want to create your own, to show non-testing ads!).

    From https://developers.google.com/admob/android/test-ads }
  TestAdMobInterstitialVideoUnitId = 'ca-app-pub-3940256099942544/8691691433';

  { Test rewarded video ad "unit id". You can use it with @link(TAds.InitializeAdMob) for testing purposes
    (but eventually you want to create your own, to show non-testing ads!).

    From https://developers.google.com/admob/android/test-ads }
  TestAdMobRewardedUnitId = 'ca-app-pub-3940256099942544/5224354917';

type
  TAdNetwork = (anAdMob, anChartboost, anStartApp);

  TFullScreenAdType = (atInterstitialStatic, atInterstitialVideo, atReward);

  TAdWatchStatus = (wsWatched, wsUnknownError, wsNetworkNotAvailable, wsNoAdsAvailable,
    wsUserAborted, wsAdNotReady, wsAdNetworkNotInitialized, wsInvalidRequest,
    wsAdTypeUnsupported, wsApplicationReinitialized);

  TAdClosedEvent = procedure (const Sender: TObject; const WatchedStatus: TAdWatchStatus) of object;

  { Advertisements in game.
    Right now only on Android (does nothing on other platforms,
    as CastleMessaging does nothing on non-Android platforms).

    Usage:

    @orderedList(
      @item(Create an instance of this class (only a single instance allowed).)

      @item(Initialize at least one ad network using one of the
        @code(InitializeXxx) methods. Usually you want to call the initialization
        from @link(TCastleApplication.OnInitialize).)

      @item(Use remaining methods of this class to show / hide ads, like
        @link(ShowFullScreenAd), @link(ShowBanner), @link(HideBanner).)

      @item(To include the necessary integration code in your Android project,
        declare your Android project type as "integrated" with
        the appropriate services (admob, chartboost, startapp...)
        inside CastleEngineManifest.xml .
        See https://castle-engine.io/android-Project-Services-Integrated-with-Castle-Game-Engine .)
    )
  }
  TAds = class(TComponent)
  private
    type
      TAdNetworkHandler = class abstract
      strict private
        FParent: TAds;
        FBannerShowing: boolean;
        FBannerGravity: Integer;
        function MessageReceived(const Received: TCastleStringList;
          const ReceivedStream: TMemoryStream): boolean;
        procedure FullScreenAdClosed(const WatchedStatus: TAdWatchStatus);
      strict protected
        FFullScreenAdVisible: boolean;
        procedure ReinitializeJavaActivity(Sender: TObject); virtual;
      public
        constructor Create(const AParent: TAds);
        destructor Destroy; override;
        property Parent: TAds read FParent;
        class function Name: string; virtual; abstract;
        procedure ShowBanner(const Gravity: Integer); virtual;
        procedure HideBanner; virtual;
        procedure ShowFullScreenAd(const AdType: TFullScreenAdType;
          const WaitUntilLoaded: boolean); virtual;
      end;

      TAdMobHandler = class(TAdNetworkHandler)
      strict private
        FBannerUnitId, FInterstitialUnitId, FRewardedUnitId, FTestDeviceIdsGlued: string;
      strict protected
        procedure ReinitializeJavaActivity(Sender: TObject); override;
      public
        constructor Create(const AParent: TAds;
          const ABannerUnitId, AInterstitialUnitId, ARewardedUnitId: string;
          const TestDeviceIds: array of string);

        class function Name: string; override;
        procedure ShowBanner(const Gravity: Integer); override;
        procedure HideBanner; override;
        procedure ShowFullScreenAd(const AdType: TFullScreenAdType;
          const WaitUntilLoaded: boolean); override;
      end;

      TChartboostHandler = class(TAdNetworkHandler)
      strict private
        FAppId, FAppSignature: string;
      strict protected
        procedure ReinitializeJavaActivity(Sender: TObject); override;
      public
        constructor Create(const AParent: TAds;
          const AnAppId, AnAppSignature: string);
        class function Name: string; override;
        procedure ShowFullScreenAd(const AdType: TFullScreenAdType;
          const WaitUntilLoaded: boolean); override;
      end;

      TStartappHandler = class(TAdNetworkHandler)
      strict private
        FAppId: string;
      strict protected
        procedure ReinitializeJavaActivity(Sender: TObject); override;
      public
        constructor Create(const AParent: TAds;
          const AnAppId: string);
        class function Name: string; override;
        procedure ShowFullScreenAd(const AdType: TFullScreenAdType;
          const WaitUntilLoaded: boolean); override;
      end;
    var
      FBannerSize: TRectangle;
      FNetworks: array [TAdNetwork] of TAdNetworkHandler;
      FOnFullScreenAdClosed: TAdClosedEvent;
  protected
    procedure FullScreenAdClosed(const WatchedStatus: TAdWatchStatus); virtual;
  public
    constructor Create(AOwner: TComponent); override;
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

      Usually called from @link(TCastleApplication.OnInitialize). }
    procedure InitializeAdMob(const BannerUnitId, InterstitialUnitId, RewardedUnitId: string;
      const TestDeviceIds: array of string); overload;

    procedure InitializeAdMob(const BannerUnitId, InterstitialUnitId: string;
      const TestDeviceIds: array of string); overload;

    { Initialize StartApp ads.
      You need to register your game on http://startapp.com/ to get app id.

      Usually called from @link(TCastleApplication.OnInitialize). }
    procedure InitializeStartapp(const AppId: string);

    { Initialize Chartboost ads.
      You need to register your game on http://chartboost.com/ to get app id and signature.

      Usually called from @link(TCastleApplication.OnInitialize). }
    procedure InitializeChartboost(const AppId, AppSignature: string);

    { Show full-screen ad, interstitial (in-between) or reward ad.

      Not all ad networks support (or differentiate between) all full-screen
      ad types possible in TFullScreenAdType enumeration.
      So we eventually show other sensible ad type instead.

      If WaitUntilLoaded, then we wait until the ad contents are fetched
      (be sure to show some "loading" state to user, as it may take a while
      in case of a slow network). If not WaitUntilLoaded, then we immediately
      resign if ad is not loaded yet. Not all ad implementations support this
      setting reliably, so this is only a suggestion.

      In any case, after this method is called, we make sure to call callback
      OnFullScreenAdClosed sometime. }
    procedure ShowFullScreenAd(const AdNetwork: TAdNetwork;
      const AdType: TFullScreenAdType;
      const WaitUntilLoaded: boolean);

    { Show banner ad.
      Banners are not supported by all ad networks (only AdMob now),
      in case they are unsupported this call is silently ignored. }
    procedure ShowBanner(const AdNetwork: TAdNetwork;
      const HorizontalGravity: THorizontalPosition;
      const VerticalGravity: TVerticalPosition);

    { Hide banner ad.
      Banners are not supported by all ad networks (only AdMob now),
      in case they are unsupported this call is silently ignored. }
    procedure HideBanner(const AdNetwork: TAdNetwork);

    property OnFullScreenAdClosed: TAdClosedEvent
      read FOnFullScreenAdClosed write FOnFullScreenAdClosed;

    property BannerSize: TRectangle read FBannerSize;
  end;

implementation

uses SysUtils, StrUtils,
  CastleUtils, CastleMessaging, CastleLog, CastleApplicationProperties;

{ TAdNetworkHandler ---------------------------------------------------------- }

constructor TAds.TAdNetworkHandler.Create(const AParent: TAds);
begin
  inherited Create;
  FParent := AParent;
  Messaging.OnReceive.Add({$ifdef FPC}@{$endif} MessageReceived);
  ApplicationProperties.OnInitializeJavaActivity.Add({$ifdef FPC}@{$endif} ReinitializeJavaActivity);
end;

destructor TAds.TAdNetworkHandler.Destroy;
begin
  if Messaging <> nil then
    Messaging.OnReceive.Remove({$ifdef FPC}@{$endif} MessageReceived);
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnInitializeJavaActivity.Remove({$ifdef FPC}@{$endif} ReinitializeJavaActivity);
  inherited;
end;

function TAds.TAdNetworkHandler.MessageReceived(const Received: TCastleStringList;
  const ReceivedStream: TMemoryStream): boolean;
var
  AdWatchStatusInt: Integer;
  WatchStatus: TAdWatchStatus;
begin
  Result := false;

  if (Received.Count = 2) and
     (Received[0] = 'ads-' + Name + '-full-screen-ad-closed') then
  begin
    if Received[1] = 'true' then
      WatchStatus := wsWatched
    else
    if Received[1] = 'false' then
      WatchStatus := wsUnknownError
    else
    begin
      if not TryStrToInt(Received[1], AdWatchStatusInt) then
        raise Exception.CreateFmt('Invalid ad watch status: %s', [Received[1]]);
      if not Between(AdWatchStatusInt, 0, Ord(High(TAdWatchStatus))) then
        raise Exception.CreateFmt('Invalid ad watch status: %d', [AdWatchStatusInt]);
      WatchStatus := TAdWatchStatus(AdWatchStatusInt);
    end;
    FullScreenAdClosed(WatchStatus);
    Result := true;
  end else

  if (Received.Count = 5) and
     (Received[0] = 'ads-' + Name + '-banner-size') then
  begin
    Result := true;
    try
      Parent.FBannerSize := Rectangle(
        StrToInt(Received[1]),
        StrToInt(Received[2]),
        StrToInt(Received[3]),
        StrToInt(Received[4])
      );
    except
      on EConvertError do
        WritelnWarning('Ads', 'Cannot process banner size from ' + GlueStrings(Received, NL));
    end;
  end;
end;

procedure TAds.TAdNetworkHandler.ShowBanner(const Gravity: Integer);
begin
  FBannerShowing := true;
  FBannerGravity := Gravity;
end;

procedure TAds.TAdNetworkHandler.HideBanner;
begin
  FBannerShowing := false;
end;

procedure TAds.TAdNetworkHandler.ShowFullScreenAd(const AdType: TFullScreenAdType;
  const WaitUntilLoaded: boolean);
begin
  { if the network doesn't support showing full-screen ads, pretend it's shown,
    in case user code waits for OnFullScreenAdClosed. }
  FullScreenAdClosed(wsAdTypeUnsupported);
end;

procedure TAds.TAdNetworkHandler.ReinitializeJavaActivity(Sender: TObject);
begin
  { this is important if the Java application was killed, but native code survived,
    while waiting for ad to finish. Reproduce: turn on "kill app when sending to bg"
    in Android debug options, then use "watch ad" and return to game. }
  if FFullScreenAdVisible then
    FullScreenAdClosed(wsApplicationReinitialized);
  Parent.FBannerSize := TRectangle.Empty;
  { reshow banner, if necessary }
  if FBannerShowing then
    ShowBanner(FBannerGravity);
end;

procedure TAds.TAdNetworkHandler.FullScreenAdClosed(const WatchedStatus: TAdWatchStatus);
begin
  FFullScreenAdVisible := false;
  Parent.FullScreenAdClosed(WatchedStatus);
end;

{ TAdMobHandler -------------------------------------------------------------- }

constructor TAds.TAdMobHandler.Create(const AParent: TAds;
  const ABannerUnitId, AInterstitialUnitId, ARewardedUnitId: string;
  const TestDeviceIds: array of string);
begin
  inherited Create(AParent);
  FTestDeviceIdsGlued := GlueStrings(TestDeviceIds, ',');
  FBannerUnitId := ABannerUnitId;
  FInterstitialUnitId := AInterstitialUnitId;
  FRewardedUnitId := ARewardedUnitId;
  ReinitializeJavaActivity(nil);
end;

procedure TAds.TAdMobHandler.ReinitializeJavaActivity(Sender: TObject);
begin
  Messaging.Send(['ads-' + Name + '-initialize', FBannerUnitId, FInterstitialUnitId, FRewardedUnitId, FTestDeviceIdsGlued]);
  inherited;
end;

class function TAds.TAdMobHandler.Name: string;
begin
  Result := 'admob';
end;

procedure TAds.TAdMobHandler.ShowBanner(const Gravity: Integer);
begin
  inherited;
  Messaging.Send(['ads-' + Name + '-banner-show', IntToStr(Gravity)]);
end;

procedure TAds.TAdMobHandler.HideBanner;
begin
  Messaging.Send(['ads-' + Name + '-banner-hide']);
  inherited;
end;

procedure TAds.TAdMobHandler.ShowFullScreenAd(const AdType: TFullScreenAdType; const WaitUntilLoaded: boolean);
var
  AdTypeName: String;
  WaitUntilLoadedStr: String;
begin
  FFullScreenAdVisible := true;

  AdTypeName := IfThen(AdType = atReward, 'reward', 'interstitial');
  WaitUntilLoadedStr := IfThen(WaitUntilLoaded, 'wait-until-loaded', 'no-wait');
  Messaging.Send(['ads-' + Name + '-show-' + AdTypeName, WaitUntilLoadedStr]);
end;

{ TChartboostHandler --------------------------------------------------------- }

constructor TAds.TChartboostHandler.Create(const AParent: TAds;
  const AnAppId, AnAppSignature: string);
begin
  inherited Create(AParent);
  FAppId := AnAppId;
  FAppSignature := AnAppSignature;
  ReinitializeJavaActivity(nil);
end;

procedure TAds.TChartboostHandler.ReinitializeJavaActivity(Sender: TObject);
begin
  Messaging.Send(['ads-' + Name + '-initialize', FAppId, FAppSignature]);
  inherited;
end;

class function TAds.TChartboostHandler.Name: string;
begin
  Result := 'chartboost';
end;

procedure TAds.TChartboostHandler.ShowFullScreenAd(const AdType: TFullScreenAdType;
  const WaitUntilLoaded: boolean);
begin
  FFullScreenAdVisible := true;
  Messaging.Send(['ads-' + Name + '-show-interstitial']);
end;

{ TStartappHandler --------------------------------------------------------- }

constructor TAds.TStartappHandler.Create(const AParent: TAds;
  const AnAppId: string);
begin
  inherited Create(AParent);
  FAppId := AnAppId;
  ReinitializeJavaActivity(nil);
end;

procedure TAds.TStartappHandler.ReinitializeJavaActivity(Sender: TObject);
begin
  Messaging.Send(['ads-' + Name + '-initialize', FAppId]);
  inherited;
end;

class function TAds.TStartappHandler.Name: string;
begin
  Result := 'startapp';
end;

procedure TAds.TStartappHandler.ShowFullScreenAd(const AdType: TFullScreenAdType;
  const WaitUntilLoaded: boolean);
begin
  FFullScreenAdVisible := true;
  Messaging.Send(['ads-' + Name + '-show-interstitial']);
end;

{ TAds ----------------------------------------------------------------------- }

constructor TAds.Create(AOwner: TComponent);
begin
  inherited;
  FBannerSize := TRectangle.Empty;
end;

destructor TAds.Destroy;
var
  AdNetwork: TAdNetwork;
begin
  for AdNetwork := Low(AdNetwork) to High(AdNetwork) do
    FreeAndNil(FNetworks[AdNetwork]);
  inherited;
end;

procedure TAds.InitializeAdMob(const BannerUnitId, InterstitialUnitId, RewardedUnitId: string;
  const TestDeviceIds: array of string);
begin
  if FNetworks[anAdMob] <> nil then
    FreeAndNil(FNetworks[anAdMob]);
  FNetworks[anAdMob] := TAdMobHandler.Create(Self,
    BannerUnitId, InterstitialUnitId, RewardedUnitId, TestDeviceIds);
end;

procedure TAds.InitializeAdMob(const BannerUnitId, InterstitialUnitId: string;
  const TestDeviceIds: array of string);
begin
  InitializeAdMob(BannerUnitId, InterstitialUnitId, '', TestDeviceIds);
end;

procedure TAds.InitializeStartapp(const AppId: string);
begin
  if FNetworks[anStartApp] <> nil then
    FreeAndNil(FNetworks[anStartApp]);
  FNetworks[anStartApp] := TStartAppHandler.Create(Self, AppId);
end;

procedure TAds.InitializeChartboost(const AppId, AppSignature: string);
begin
  if FNetworks[anChartboost] <> nil then
    FreeAndNil(FNetworks[anChartboost]);
  FNetworks[anChartboost] := TChartboostHandler.Create(Self, AppId, AppSignature);
end;

procedure TAds.FullScreenAdClosed(const WatchedStatus: TAdWatchStatus);
begin
  if Assigned(OnFullScreenAdClosed) then
    OnFullScreenAdClosed(Self, WatchedStatus);
end;

procedure TAds.ShowFullScreenAd(const AdNetwork: TAdNetwork;
  const AdType: TFullScreenAdType; const WaitUntilLoaded: boolean);
begin
  {$ifdef ANDROID}
  if FNetworks[AdNetwork] <> nil then
    FNetworks[AdNetwork].ShowFullScreenAd(AdType, WaitUntilLoaded)
  else
  { if the network is not initialized, pretend it's shown,
    in case user code waits for OnFullScreenAdClosed. }
    FullScreenAdClosed(wsAdNetworkNotInitialized);
  {$else}
  { since this is not supported on non-Android now, just make
    FullScreenAdClosed(false) immediately, to avoid the app waiting
    for OnFullScreenAdClosed forever. }
  FullScreenAdClosed(wsAdNetworkNotInitialized);
  {$endif}
end;

procedure TAds.ShowBanner(const AdNetwork: TAdNetwork;
  const HorizontalGravity: THorizontalPosition;
  const VerticalGravity: TVerticalPosition);

  function GravityToInt(
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
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('ShowBanner:HorizontalGravity?');
      {$endif}
    end;
    case VerticalGravity of
      vpTop: Result := Result or GravityTop;
      vpBottom: Result := Result or GravityBottom;
      vpMiddle: Result := Result or GravityCenterVertical;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('ShowBanner:VerticalGravity?');
      {$endif}
    end;
  end;

begin
  if FNetworks[AdNetwork] <> nil then
    FNetworks[AdNetwork].ShowBanner(GravityToInt(HorizontalGravity, VerticalGravity));
end;

procedure TAds.HideBanner(const AdNetwork: TAdNetwork);
begin
  if FNetworks[AdNetwork] <> nil then
    FNetworks[AdNetwork].HideBanner;
end;

end.
