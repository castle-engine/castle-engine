/*
  Copyright 2026-2026 Hamid reza Kabiri.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

#import <UserMessagingPlatform/UserMessagingPlatform.h>

#import "AdMobService.h"

/* Same values, in the same order, as TAdWatchStatus in Pascal CastleAds unit
   (and in Java TAdWatchStatus). Sent to Pascal as an integer. */
typedef NS_ENUM(NSInteger, TAdWatchStatus) {
    wsWatched,
    wsUnknownError,
    wsNetworkNotAvailable,
    wsNoAdsAvailable,
    wsUserAborted,
    wsAdNotReady,
    wsAdNetworkNotInitialized,
    wsInvalidRequest,
    wsAdTypeUnsupported,
    wsApplicationReinitialized
};

/* Gravity constants used by Pascal to place the banner,
   same as Android Gravity (the Pascal code sends the same values to both).
   Horizontal center and bottom are the defaults, no constants needed for them. */
static const int GravityLeft = 0x03;
static const int GravityRight = 0x05;
static const int GravityTop = 0x30;
static const int GravityCenterVertical = 0x10;

/* No error when sending ads-admob-consent-gathered, same as on Android. */
static const NSInteger NoError = -1;

@implementation AdMobService
{
    NSString* bannerUnitId;
    NSString* interstitialUnitId;
    NSString* rewardedUnitId;
    NSArray<NSString*>* testDeviceIds;

    /* Got ads-admob-initialize. */
    bool initialized;
    /* GADMobileAds was started, and ads are being loaded. */
    bool mobileAdsInitialized;
    /* The consent is being gathered, so ads must not be requested yet. */
    bool consentGathering;

    GADBannerView* bannerView;

    GADInterstitialAd* interstitial;
    bool interstitialIsLoading;
    bool interstitialOpenWhenLoaded;
    TAdWatchStatus interstitialLastError;

    GADRewardedAd* rewarded;
    bool rewardedIsLoading;
    bool rewardedOpenWhenLoaded;
    bool rewardedWatched;
    TAdWatchStatus rewardedLastError;
}

- (UIViewController*)rootViewController
{
    return self.mainController;
}

/* Split a list glued by Pascal with Chr(2), an empty string is an empty list. */
- (NSArray<NSString*>*)splitList:(NSString*) glued
{
    if (glued.length == 0) {
        return @[];
    }
    return [glued componentsSeparatedByString: @"\x02"];
}

- (TAdWatchStatus)watchStatusFromError:(NSError*) error
{
    if (error == nil) {
        return wsUnknownError;
    }
    switch (error.code) {
        case GADErrorNoFill: return wsNoAdsAvailable;
        case GADErrorNetworkError: return wsNetworkNotAvailable;
        case GADErrorInvalidRequest: return wsInvalidRequest;
        default: return wsUnknownError;
    }
}

- (void)fullScreenAdClosed:(TAdWatchStatus) status
{
    [self messageSend: @[@"ads-admob-full-screen-ad-closed",
        [NSString stringWithFormat: @"%ld", (long)status]]];
}

- (void)rewardedReadySend:(bool) ready
{
    [self messageSend: @[@"ads-admob-reward-ready", [self boolToString: ready]]];
}

/* ---------------------------------------------------------------------------
   Initialization and the user consent. */

/* Start the ads SDK, unless we still wait for the user consent.
   Google requires that nothing requests an ad before the user has answered,
   see https://developers.google.com/admob/ios/privacy .
   Safe to call many times, does the work once.
   Called when Pascal initializes the ads, and when the consent is gathered,
   as these two can happen in any order. */
- (void)initializeMobileAdsIfAllowed
{
    if (!initialized || mobileAdsInitialized) {
        return;
    }
    if (consentGathering && !UMPConsentInformation.sharedInstance.canRequestAds) {
        NSLog(@"AdMobService: initialization waits for the user consent");
        return;
    }
    mobileAdsInitialized = TRUE;

    if (testDeviceIds.count != 0) {
        GADMobileAds.sharedInstance.requestConfiguration.testDeviceIdentifiers = testDeviceIds;
    }
    [GADMobileAds.sharedInstance startWithCompletionHandler: nil];

    [self loadInterstitial];
    [self loadRewarded];
    NSLog(@"AdMobService: initialized");
}

- (void)initialize:(NSString*) aBannerUnitId
  interstitial:(NSString*) aInterstitialUnitId
  rewarded:(NSString*) aRewardedUnitId
  testDevices:(NSArray<NSString*>*) aTestDeviceIds
{
    bannerUnitId = aBannerUnitId;
    interstitialUnitId = aInterstitialUnitId;
    rewardedUnitId = aRewardedUnitId;
    testDeviceIds = aTestDeviceIds;
    initialized = TRUE;
    [self initializeMobileAdsIfAllowed];
}

- (void)consentGathered:(NSError*) error
{
    consentGathering = FALSE;
    if (error != nil) {
        NSLog(@"AdMobService: gathering the consent failed: %@", error);
    }

    UMPConsentInformation* info = UMPConsentInformation.sharedInstance;
    bool canRequestAds = info.canRequestAds;
    bool privacyOptionsRequired =
        info.privacyOptionsRequirementStatus == UMPPrivacyOptionsRequirementStatusRequired;
    NSInteger errorCode = error != nil ? error.code : NoError;

    /* Only the error code is sent, not the message,
       as an arbitrary message could contain our message delimiter. */
    [self messageSend: @[@"ads-admob-consent-gathered",
        [self boolToString: canRequestAds],
        [self boolToString: privacyOptionsRequired],
        [NSString stringWithFormat: @"%ld", (long)errorCode]]];

    [self initializeMobileAdsIfAllowed];
}

/* Gather the user consent using Google's User Messaging Platform.
   Pascal sends this before ads-admob-initialize, and it immediately blocks
   the ads initialization (consentGathering = TRUE) until consentGathered. */
- (void)consentRequest:(bool) debugForceEea
  debugDeviceHashes:(NSArray<NSString*>*) debugDeviceHashes
{
    consentGathering = TRUE;

    UMPRequestParameters* parameters = [[UMPRequestParameters alloc] init];
    if (debugForceEea || debugDeviceHashes.count != 0) {
        UMPDebugSettings* debugSettings = [[UMPDebugSettings alloc] init];
        if (debugForceEea) {
            debugSettings.geography = UMPDebugGeographyEEA;
        }
        debugSettings.testDeviceIdentifiers = debugDeviceHashes;
        parameters.debugSettings = debugSettings;
    }

    [UMPConsentInformation.sharedInstance
        requestConsentInfoUpdateWithParameters: parameters
        completionHandler: ^(NSError* _Nullable requestError) {
            if (requestError != nil) {
                [self consentGathered: requestError];
                return;
            }
            [UMPConsentForm
                loadAndPresentIfRequiredFromViewController: [self rootViewController]
                completionHandler: ^(NSError* _Nullable formError) {
                    [self consentGathered: formError];
                }];
        }];
}

- (void)consentShowPrivacyOptions
{
    [UMPConsentForm
        presentPrivacyOptionsFormFromViewController: [self rootViewController]
        completionHandler: ^(NSError* _Nullable formError) {
            /* The answer may have changed, tell Pascal again. */
            [self consentGathered: formError];
        }];
}

- (void)consentReset
{
    [UMPConsentInformation.sharedInstance reset];
}

/* ---------------------------------------------------------------------------
   Banner. */

- (void)bannerShow:(int) gravity
{
    if (!mobileAdsInitialized || bannerUnitId.length == 0) {
        return;
    }
    [self bannerHide];

    bannerView = [[GADBannerView alloc] initWithAdSize: GADAdSizeBanner];
    bannerView.adUnitID = bannerUnitId;
    bannerView.rootViewController = [self rootViewController];
    bannerView.translatesAutoresizingMaskIntoConstraints = NO;

    UIView* parent = [self rootViewController].view;
    [parent addSubview: bannerView];

    /* Place the banner inside the safe area, following the gravity. */
    UILayoutGuide* safe = parent.safeAreaLayoutGuide;
    NSMutableArray<NSLayoutConstraint*>* constraints = [NSMutableArray array];

    int horizontal = gravity & 0x07;
    if (horizontal == GravityLeft) {
        [constraints addObject: [bannerView.leftAnchor constraintEqualToAnchor: safe.leftAnchor]];
    } else
    if (horizontal == GravityRight) {
        [constraints addObject: [bannerView.rightAnchor constraintEqualToAnchor: safe.rightAnchor]];
    } else {
        [constraints addObject: [bannerView.centerXAnchor constraintEqualToAnchor: safe.centerXAnchor]];
    }

    int vertical = gravity & 0x70;
    if (vertical == GravityTop) {
        [constraints addObject: [bannerView.topAnchor constraintEqualToAnchor: safe.topAnchor]];
    } else
    if (vertical == GravityCenterVertical) {
        [constraints addObject: [bannerView.centerYAnchor constraintEqualToAnchor: safe.centerYAnchor]];
    } else {
        [constraints addObject: [bannerView.bottomAnchor constraintEqualToAnchor: safe.bottomAnchor]];
    }
    [NSLayoutConstraint activateConstraints: constraints];

    [bannerView loadRequest: [GADRequest request]];
}

- (void)bannerHide
{
    if (bannerView != nil) {
        [bannerView removeFromSuperview];
        bannerView = nil;
    }
}

/* ---------------------------------------------------------------------------
   Interstitial. */

- (void)loadInterstitial
{
    if (interstitialUnitId.length == 0 || interstitialIsLoading) {
        return;
    }
    interstitialIsLoading = TRUE;
    [GADInterstitialAd loadWithAdUnitID: interstitialUnitId
        request: [GADRequest request]
        completionHandler: ^(GADInterstitialAd* _Nullable ad, NSError* _Nullable error) {
            self->interstitialIsLoading = FALSE;
            if (error != nil) {
                NSLog(@"AdMobService: interstitial failed to load: %@", error);
                self->interstitial = nil;
                self->interstitialLastError = [self watchStatusFromError: error];
                if (self->interstitialOpenWhenLoaded) {
                    self->interstitialOpenWhenLoaded = FALSE;
                    [self fullScreenAdClosed: self->interstitialLastError];
                }
                return;
            }
            self->interstitial = ad;
            self->interstitial.fullScreenContentDelegate = self;
            if (self->interstitialOpenWhenLoaded) {
                self->interstitialOpenWhenLoaded = FALSE;
                [self presentInterstitial];
            }
        }];
}

- (void)presentInterstitial
{
    GADInterstitialAd* ad = interstitial;
    interstitial = nil;
    [ad presentFromRootViewController: [self rootViewController]];
}

- (void)showInterstitial:(bool) waitUntilLoaded
{
    if (!mobileAdsInitialized || interstitialUnitId.length == 0) {
        [self fullScreenAdClosed: wsAdNetworkNotInitialized];
        return;
    }
    if (interstitial != nil) {
        [self presentInterstitial];
    } else
    if (waitUntilLoaded) {
        interstitialOpenWhenLoaded = TRUE;
        [self loadInterstitial];
    } else {
        [self fullScreenAdClosed: interstitialIsLoading ? wsAdNotReady : interstitialLastError];
        [self loadInterstitial];
    }
}

/* ---------------------------------------------------------------------------
   Rewarded. */

- (void)loadRewarded
{
    if (rewardedUnitId.length == 0 || rewardedIsLoading) {
        return;
    }
    rewardedIsLoading = TRUE;
    [GADRewardedAd loadWithAdUnitID: rewardedUnitId
        request: [GADRequest request]
        completionHandler: ^(GADRewardedAd* _Nullable ad, NSError* _Nullable error) {
            self->rewardedIsLoading = FALSE;
            if (error != nil) {
                NSLog(@"AdMobService: rewarded ad failed to load: %@", error);
                self->rewarded = nil;
                self->rewardedLastError = [self watchStatusFromError: error];
                [self rewardedReadySend: FALSE];
                if (self->rewardedOpenWhenLoaded) {
                    self->rewardedOpenWhenLoaded = FALSE;
                    [self fullScreenAdClosed: self->rewardedLastError];
                }
                return;
            }
            self->rewarded = ad;
            self->rewarded.fullScreenContentDelegate = self;
            // Ready, unless it is shown right now because someone waited for it.
            [self rewardedReadySend: !self->rewardedOpenWhenLoaded];
            if (self->rewardedOpenWhenLoaded) {
                self->rewardedOpenWhenLoaded = FALSE;
                [self presentRewarded];
            }
        }];
}

- (void)presentRewarded
{
    GADRewardedAd* ad = rewarded;
    rewarded = nil;
    rewardedWatched = FALSE;
    [self rewardedReadySend: FALSE];
    [ad presentFromRootViewController: [self rootViewController]
        userDidEarnRewardHandler: ^{
            self->rewardedWatched = TRUE;
        }];
}

- (void)showRewarded:(bool) waitUntilLoaded
{
    if (!mobileAdsInitialized || rewardedUnitId.length == 0) {
        [self fullScreenAdClosed: wsAdNetworkNotInitialized];
        return;
    }
    if (rewarded != nil) {
        [self presentRewarded];
    } else
    if (waitUntilLoaded) {
        rewardedOpenWhenLoaded = TRUE;
        [self loadRewarded];
    } else {
        [self fullScreenAdClosed: rewardedIsLoading ? wsAdNotReady : rewardedLastError];
        [self loadRewarded];
    }
}

/* ---------------------------------------------------------------------------
   GADFullScreenContentDelegate, for both interstitial and rewarded ads. */

- (void)adDidDismissFullScreenContent:(id<GADFullScreenPresentingAd>) ad
{
    if ([(NSObject*)ad isKindOfClass: [GADRewardedAd class]]) {
        [self fullScreenAdClosed: rewardedWatched ? wsWatched : wsUserAborted];
        rewardedWatched = FALSE;
        [self loadRewarded];
    } else {
        [self fullScreenAdClosed: wsWatched];
        [self loadInterstitial];
    }
}

- (void)ad:(id<GADFullScreenPresentingAd>) ad
  didFailToPresentFullScreenContentWithError:(NSError*) error
{
    NSLog(@"AdMobService: ad failed to present: %@", error);
    /* Report that the ad is closed, otherwise the Pascal code waits
       for OnFullScreenAdClosed forever. */
    [self fullScreenAdClosed: wsUnknownError];
    if ([(NSObject*)ad isKindOfClass: [GADRewardedAd class]]) {
        rewardedWatched = FALSE;
        [self loadRewarded];
    } else {
        [self loadInterstitial];
    }
}

/* ---------------------------------------------------------------------------
   Messages from Pascal, the same as for the Android admob service. */

- (bool)messageReceived:(NSArray* )message
{
    NSString* name = message.count > 0 ? [message objectAtIndex: 0] : @"";

    if (message.count == 5 && [name isEqualToString: @"ads-admob-initialize"]) {
        [self initialize: [message objectAtIndex: 1]
            interstitial: [message objectAtIndex: 2]
            rewarded: [message objectAtIndex: 3]
            testDevices: [self splitList: [message objectAtIndex: 4]]];
        return TRUE;
    } else
    if (message.count == 3 && [name isEqualToString: @"ads-admob-consent-request"]) {
        [self consentRequest: [self stringToBool: [message objectAtIndex: 1]]
            debugDeviceHashes: [self splitList: [message objectAtIndex: 2]]];
        return TRUE;
    } else
    if (message.count == 1 && [name isEqualToString: @"ads-admob-consent-show-privacy-options"]) {
        [self consentShowPrivacyOptions];
        return TRUE;
    } else
    if (message.count == 1 && [name isEqualToString: @"ads-admob-consent-reset"]) {
        [self consentReset];
        return TRUE;
    } else
    if (message.count == 2 && [name isEqualToString: @"ads-admob-banner-show"]) {
        [self bannerShow: [[message objectAtIndex: 1] intValue]];
        return TRUE;
    } else
    if (message.count == 1 && [name isEqualToString: @"ads-admob-banner-hide"]) {
        [self bannerHide];
        return TRUE;
    } else
    if (message.count == 2 && [name isEqualToString: @"ads-admob-show-interstitial"]) {
        [self showInterstitial: [[message objectAtIndex: 1] isEqualToString: @"wait-until-loaded"]];
        return TRUE;
    } else
    if (message.count == 2 && [name isEqualToString: @"ads-admob-show-reward"]) {
        [self showRewarded: [[message objectAtIndex: 1] isEqualToString: @"wait-until-loaded"]];
        return TRUE;
    }

    return FALSE;
}

@end
