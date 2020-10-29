/* -*- tab-width: 4 -*- */

/*
  Copyright 2018-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

package net.sourceforge.castleengine;

import android.util.Log;
import android.view.View;

import com.google.android.gms.ads.AdRequest;
import com.google.android.gms.ads.AdSize;
import com.google.android.gms.ads.AdView;
import com.google.android.gms.ads.InterstitialAd;
import com.google.android.gms.ads.AdListener;
import com.google.android.gms.ads.MobileAds;
import com.google.android.gms.ads.reward.RewardedVideoAd;
import com.google.android.gms.ads.reward.RewardedVideoAdListener;
import com.google.android.gms.ads.reward.RewardItem;

/**
 * Integration of Google Ads (AdMob) with Castle Game Engine.
 */
public class ServiceAdMob extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceAdMob";
    private final boolean debug = false; // set to true for debug (more logs)

    private static final int NO_ERROR = -1; // no error constant

    private boolean initialized;
    private String mBannerUnitId, mInterstitialUnitId, mRewardedUnitId;

    private ActivityPopup adPopup = null;

    private InterstitialAd interstitial = null;
    private Boolean interstitialOpenWhenLoaded = false; // used when you want to wait for interstitial ad
    private Boolean failedToLoadInterstitialLastTime = false; // when loading failed last time, this is needed to try again on next call
    private Boolean interstitialIsLoading = false; // used to when waitUntilLoaded = false to better error reporting (true when ad is loading now)
    private int interstitialLastErroCode = NO_ERROR; // store last ad loading error code (interstitial)

    private RewardedVideoAd rewarded = null;
    private Boolean rewardedWatched = false; // should we set rewarded video as watched
    private Boolean rewardedOpenWhenLoaded = false; // used when you want to wait for rewarded ad
    private Boolean failedToLoadRewardedLastTime = false; // when loading failed last time, this is needed to try again on next call
    private Boolean rewardedIsLoading = false; // used to when waitUntilLoaded = false to better error reporting (true when ad is loading now)
    private int rewardedLastErroCode = NO_ERROR; // store last ad loading error code (rewarded)

    private String[] testDeviceIds;

    public ServiceAdMob(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "admob";
    }

    private void initialize(String bannerUnitId, String interstitialUnitId, String rewardedUnitId, String[] aTestDeviceIds)
    {
        if (initialized) {
            return;
        }

        initialized = true;
        mBannerUnitId = bannerUnitId;
        mInterstitialUnitId = interstitialUnitId;
        mRewardedUnitId = rewardedUnitId;
        testDeviceIds = aTestDeviceIds;

        // MobileAds initialize - should be done before loading of any ad:
        MobileAds.initialize(getActivity(), "${ANDROID.ADMOB.APP_ID}");

        interstitialInitialize();
        rewardedInitialize();
        logInfo(CATEGORY, "AdMob initialized");
    }

    private void fullScreenAdClosed(TAdWatchStatus watchedStatus)
    {
        if (debug) {
            logInfo(CATEGORY, "fullScreenAdClosed - watchedStatus: " + watchedStatus.toString());
        }
        messageSend(new String[]{"ads-admob-full-screen-ad-closed", Integer.toString(watchedStatus.ordinal()) });
    }

    private void fullScreenAdClosedWithError(int errorCode)
    {
        switch(errorCode) {
            case AdRequest.ERROR_CODE_INTERNAL_ERROR:
                fullScreenAdClosed(TAdWatchStatus.wsUnknownError);
                break;
            case AdRequest.ERROR_CODE_INVALID_REQUEST:
                fullScreenAdClosed(TAdWatchStatus.wsInvalidRequest);
                break;
            case AdRequest.ERROR_CODE_NETWORK_ERROR:
                fullScreenAdClosed(TAdWatchStatus.wsNetworkNotAvailable);
                break;
            case AdRequest.ERROR_CODE_NO_FILL:
                fullScreenAdClosed(TAdWatchStatus.wsNoAdsAvailable);
                break;
            default:
                fullScreenAdClosed(TAdWatchStatus.wsUnknownError);
        }
    }

    private void interstitialInitialize()
    {
        if (mInterstitialUnitId.equals(""))
            return;

        // Create the interstitial.
        interstitial = new InterstitialAd(getActivity());
        interstitial.setAdUnitId(mInterstitialUnitId);
        // Set an AdListener.
        interstitial.setAdListener(new AdListener() {
            @Override
            public void onAdLoaded() {
                if (debug) {
                    logInfo(CATEGORY, "onAdLoaded");
                }
                failedToLoadInterstitialLastTime = false;
                interstitialIsLoading = false;
                if (interstitialOpenWhenLoaded) {
                    interstitialOpenWhenLoaded = false;
                    logInfo(CATEGORY, "Show ad after waiting for ad.");
                    interstitial.show();
                }
            }

            @Override
            public void onAdFailedToLoad(int errorCode)  {
                if (debug) {
                    logInfo(CATEGORY, "onAdFailedToLoad");
                }
                failedToLoadInterstitialLastTime = true;
                interstitialLastErroCode = errorCode;
                interstitialIsLoading = false;
                if (interstitialOpenWhenLoaded) {
                    // this is correct only when we waited for ad and now need return
                    fullScreenAdClosedWithError(errorCode);
                }
                interstitialOpenWhenLoaded = false;
            }

            @Override
            public void onAdClosed() {
                if (debug) {
                    logInfo(CATEGORY, "Ad Closed");
                }
                fullScreenAdClosed(TAdWatchStatus.wsWatched);
                loadInterstitial(); // load next ad
            }
        });

        // Begin loading your interstitial.
        loadInterstitial();
    }

    public void loadInterstitial() {
        failedToLoadInterstitialLastTime = false;
        if (!interstitial.isLoaded()) {
            if (debug) {
                logInfo(CATEGORY, "Started loading interstitial.");
            }
            interstitialIsLoading = true;
            interstitialLastErroCode = NO_ERROR;
            interstitial.loadAd(buildAdRequest());
        }
    }


    private void rewardedInitialize()
    {
        if (mRewardedUnitId.equals(""))
            return;

        rewarded = MobileAds.getRewardedVideoAdInstance(getActivity());

        // Set a RewardedVideoAdListener.
        rewarded.setRewardedVideoAdListener(new RewardedVideoAdListener()
        {
            @Override
            public void onRewarded(RewardItem reward) {
                if (debug) {
                    logInfo(CATEGORY, "onRewarded");
                }   
                rewardedWatched = true;
            }
            
            @Override
            public void onRewardedVideoAdClosed() {
                if (debug) {
                    logInfo(CATEGORY, "onRewardedVideoAdClosed");
                }
                loadRewarded();
                if (rewardedWatched)
                    fullScreenAdClosed(TAdWatchStatus.wsWatched);
                else
                    fullScreenAdClosed(TAdWatchStatus.wsUserAborted);
                rewardedWatched = false;
            }

            @Override
            public void onRewardedVideoAdFailedToLoad(int errorCode) {
                if (debug) {
                    logInfo(CATEGORY, "onRewardedVideoAdFailedToLoad");
                }
                failedToLoadRewardedLastTime = true;
                rewardedIsLoading = false;
                rewardedLastErroCode = errorCode;
                if (rewardedOpenWhenLoaded) {
                    // this is correct only when we waited for ad and now need return 
                    fullScreenAdClosedWithError(errorCode);
                }
                rewardedOpenWhenLoaded = false;
            }

            @Override
            public void onRewardedVideoAdLoaded() {
                if (debug) {
                    logInfo(CATEGORY, "onRewardedVideoAdLoaded");
                }
                rewardedIsLoading = false;
                if (rewardedOpenWhenLoaded) {
                    rewardedOpenWhenLoaded = false;
                    logInfo(CATEGORY, "Show ad after waiting for ad.");
                    rewarded.show();
                }
            }

            @Override
            public void onRewardedVideoAdLeftApplication() {
                if (debug) {
                    logInfo(CATEGORY, "onRewardedVideoAdLeftApplication");
                }
            }

            @Override
            public void onRewardedVideoAdOpened() {
                if (debug) {
                    logInfo(CATEGORY, "onRewardedVideoAdOpened");
                }
            }

            @Override
            public void onRewardedVideoStarted() {
                if (debug) {
                    logInfo(CATEGORY, "onRewardedVideoStarted");
                }
            }

            @Override
            public void onRewardedVideoCompleted() {
                if (debug) {
                    logInfo(CATEGORY, "onRewardedVideoCompleted");
                }
            }
        });

        // Begin loading your rewarded ad.
        loadRewarded();
    }

    public void loadRewarded() {
        failedToLoadRewardedLastTime = false;
        if (!rewarded.isLoaded()) {
            if (debug) {
                logInfo(CATEGORY, "Started loading rewarded video.");
            }
            rewardedIsLoading = true;
            rewardedLastErroCode = NO_ERROR;
            rewarded.loadAd(mRewardedUnitId, buildAdRequest());
        }
    }

    private void bannerShow(int gravity)
    {
        if (!initialized) {
            return;
        }

        if (adPopup != null) {
            return; // nothing to do, already showing
        }

        // Place the AdView in a PopupWindow.
        AdView adView = new AdView(getActivity());
        adView.setAdUnitId(mBannerUnitId);
        adView.setAdSize(AdSize.SMART_BANNER);

        adPopup = new ActivityPopup(this, gravity, adView);

        adView.setVisibility(View.VISIBLE);
        adView.loadAd(buildAdRequest());
    }

    private AdRequest buildAdRequest()
    {
        AdRequest.Builder builder = new AdRequest.Builder();

        /* addTestDevice calls are not strictly needed,
           https://developers.google.com/mobile-ads-sdk/docs/admob/android/quick-start
           if you use a "test ad id".
           But they are useful to test ads with real ad id,
           this way on specific devices the ads are still test. */
        builder = builder.addTestDevice(AdRequest.DEVICE_ID_EMULATOR);
        for (String id : testDeviceIds) {
            builder = builder.addTestDevice(id);
        }
        return builder.build();
    }

    private void bannerHide()
    {
        if (adPopup != null) {
            adPopup.dispose();
            adPopup = null;
        }
    }

    /*
     * Invoke this when you are ready to display an interstitial.
     *
     * If waitUntilLoaded == true, we will set interstitialOpenWhenLoaded to true 
     * to show ad when it will be ready
     *
     * If waitUntilLoaded == false, we will ignore the request is the ad
     * is not ready yet (e.g. because Internet connection is slow/broken now).
     */
    private void interstitialDisplay(boolean waitUntilLoaded)
    {
        if (initialized && !mInterstitialUnitId.equals("")) {
            if (waitUntilLoaded || interstitial.isLoaded()) {
                if (waitUntilLoaded && !interstitial.isLoaded()) {
                    // calling show() when ad is not loaded do nothing, so we show ad when it will be available
                    logInfo(CATEGORY, "Requested showing interstitial ad with waitUntilLoaded, and ad not ready yet. Will wait until ad is ready.");
                    interstitialOpenWhenLoaded = true;
                    if (failedToLoadInterstitialLastTime) //loading ad failed last time so there was no load in onAdClose()
                        loadInterstitial();
                    return;
                }
                else if (interstitial.isLoaded()) {
                    interstitial.show();
                }
            } else {
                // ad not loaded and we don't want to wait or loading failed
                if (interstitialIsLoading) {
                    // ad not loaded and we don't want to wait
                    fullScreenAdClosed(TAdWatchStatus.wsAdNotReady);
                }
                else {
                    // ad loading failed so return error and try load ad again (for next request)
                    fullScreenAdClosedWithError(interstitialLastErroCode);
                    loadInterstitial();
                }
            }
        } else {
            // network or interstitial ads not initialized
            fullScreenAdClosed(TAdWatchStatus.wsAdNetworkNotInitialized);
        }
    }

    /*
     * Invoke this when you are ready to display an rewarded ad.
     *
     * If waitUntilLoaded == true, we will set rewardedOpenWhenLoaded to true 
     * to show ad when it will be ready
     *
     * If waitUntilLoaded == false, we will ignore the request if the ad
     * is not ready yet (e.g. because Internet connection is slow/broken now).
     */
    private void rewardedDisplay(boolean waitUntilLoaded)
    {
        rewardedWatched = false;
        if (initialized && !mRewardedUnitId.equals("")) {
            if (waitUntilLoaded || rewarded.isLoaded()) {
                if (waitUntilLoaded && !rewarded.isLoaded()) {
                    logInfo(CATEGORY, "Requested showing reward ad with waitUntilLoaded, and ad not ready yet. Will wait until ad is ready.");
                    rewardedOpenWhenLoaded = true;
                    if (failedToLoadRewardedLastTime) //loading ad failed last time so there was no load in onRewardedAdClose()
                        loadRewarded();
                    return;
                }
                else if (rewarded.isLoaded()) {
                    rewarded.show();
                }
            } else {
                // ad not loaded and we don't want to wait or loading failed
                if (rewardedIsLoading) {
                    // ad not loaded and we don't want to wait
                    fullScreenAdClosed(TAdWatchStatus.wsAdNotReady);
                }
                else {
                    // ad loading failed so return error and try load ad again (for next request)
                    fullScreenAdClosedWithError(rewardedLastErroCode);
                    loadRewarded();
                }
            }
        } else {
            // network or interstitial ads not initialized
            fullScreenAdClosed(TAdWatchStatus.wsAdNetworkNotInitialized);
        }
    }


    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 5 && parts[0].equals("ads-admob-initialize")) {
            initialize(parts[1], parts[2], parts[3], parts[4].split(","));
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-admob-banner-show")) {
            int gravity = Integer.parseInt(parts[1]);
            bannerShow(gravity);
            return true;
        } else
        if (parts.length == 1 && parts[0].equals("ads-admob-banner-hide")) {
            bannerHide();
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-admob-show-interstitial") && parts[1].equals("wait-until-loaded")) {
            interstitialDisplay(true);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-admob-show-interstitial") && parts[1].equals("no-wait")) {
            interstitialDisplay(false);
            return true;
        } else 
        if (parts.length == 2 && parts[0].equals("ads-admob-show-reward") && parts[1].equals("wait-until-loaded")) {
            rewardedDisplay(true);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-admob-show-reward") && parts[1].equals("no-wait")) {
            rewardedDisplay(false);
            return true;
        } else {
            return false;
        }
    }
}
