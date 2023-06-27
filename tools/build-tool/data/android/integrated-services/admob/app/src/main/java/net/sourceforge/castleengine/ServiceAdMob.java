/* -*- tab-width: 4 -*- */

/*
  Copyright 2018-2023 Michalis Kamburelis, Andrzej Kilijanski.

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

// needed for @NotNull
import androidx.annotation.NonNull;

import com.google.android.gms.ads.MobileAds;
import com.google.android.gms.ads.initialization.InitializationStatus;
import com.google.android.gms.ads.initialization.OnInitializationCompleteListener;
import com.google.android.gms.ads.FullScreenContentCallback;
import com.google.android.gms.ads.LoadAdError;
import com.google.android.gms.ads.AdRequest;

import java.util.Arrays;
import java.util.List;

import com.google.android.gms.ads.AdError;
import com.google.android.gms.ads.AdSize;
import com.google.android.gms.ads.RequestConfiguration;

// banner ads
import com.google.android.gms.ads.AdView;

// interstitial ads
import com.google.android.gms.ads.interstitial.InterstitialAd;
import com.google.android.gms.ads.interstitial.InterstitialAdLoadCallback;

// rewarded ads
import com.google.android.gms.ads.rewarded.RewardedAd;
import com.google.android.gms.ads.rewarded.RewardItem;
import com.google.android.gms.ads.OnUserEarnedRewardListener;
import com.google.android.gms.ads.rewarded.RewardedAdLoadCallback;

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
    private FullScreenContentCallback interstitialFullScreenContentCallback = null; // created once in initialization

    private RewardedAd rewarded = null;
    private Boolean rewardedWatched = false; // should we set rewarded video as watched
    private Boolean rewardedOpenWhenLoaded = false; // used when you want to wait for rewarded ad
    private Boolean failedToLoadRewardedLastTime = false; // when loading failed last time, this is needed to try again on next call
    private Boolean rewardedIsLoading = false; // used to when waitUntilLoaded = false to better error reporting (true when ad is loading now)
    private int rewardedLastErroCode = NO_ERROR; // store last ad loading error code (rewarded)
    private FullScreenContentCallback rewardedFullScreenContentCallback = null; // created once in initialization

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
        MobileAds.initialize(getActivity(), new OnInitializationCompleteListener() {
            @Override
            public void onInitializationComplete(InitializationStatus initializationStatus) {
                // TODO: should we return initialization errors? Older admob version didn't have that
            }
        });

        setConfiguration();
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

        interstitialFullScreenContentCallback = new FullScreenContentCallback() {
        @Override
        public void onAdClicked() {
            // Called when a click is recorded for an ad.
            if (debug) {
                logInfo(CATEGORY, "interstitial - onAdClicked");
            }
        }

        @Override
        public void onAdDismissedFullScreenContent() {
            if (debug) {
                logInfo(CATEGORY, "interstitial - Ad Closed");
            }
            interstitial = null;
            fullScreenAdClosed(TAdWatchStatus.wsWatched);
            loadInterstitial(); // load next ad
        }

        @Override
        public void onAdFailedToShowFullScreenContent(AdError adError) {
            // Called when ad fails to show.
            if (debug) {
                logInfo(CATEGORY, "interstitial - onAdFailedToShowFullScreenContent");
            }
            interstitial = null;
            loadInterstitial(); // load next ad
        }

        @Override
        public void onAdImpression() {
            // Called when an impression is recorded for an ad.
            if (debug) {
                logInfo(CATEGORY, "interstitial - onAdImpression");
            }
        }

        @Override
        public void onAdShowedFullScreenContent() {
            // Called when ad is shown.
            if (debug) {
                logInfo(CATEGORY, "interstitial - onAdShowedFullScreenContent");
            }
        }

        };

        // Begin loading your interstitial.
        loadInterstitial();
    }

    public void loadInterstitial() {
        if (mInterstitialUnitId.equals(""))
            return;

        failedToLoadInterstitialLastTime = false;
        interstitialIsLoading = true;
        interstitialLastErroCode = NO_ERROR;

        AdRequest adRequest = new AdRequest.Builder().build();

        InterstitialAd.load(getActivity(), mInterstitialUnitId, adRequest,
            new InterstitialAdLoadCallback() {
            @Override
            public void onAdLoaded(@NonNull InterstitialAd interstitialAd) {
                if (debug) {
                    logInfo(CATEGORY, "interstitial - onAdLoaded");
                }
                // The interstitial reference will be null until an ad is loaded.
                interstitial = interstitialAd;
                interstitial.setFullScreenContentCallback(interstitialFullScreenContentCallback);
                failedToLoadInterstitialLastTime = false;
                interstitialIsLoading = false;
                if (interstitialOpenWhenLoaded) {
                    interstitialOpenWhenLoaded = false;
                    logInfo(CATEGORY, "Show ad after waiting for ad.");
                    interstitial.show(getActivity());
                    }
            }

            @Override
            public void onAdFailedToLoad(@NonNull LoadAdError loadAdError) {
                if (debug) {
                    logInfo(CATEGORY, "onAdFailedToLoad");
                }
                failedToLoadInterstitialLastTime = true;
                interstitialLastErroCode = loadAdError.getCode();
                interstitialIsLoading = false;
                if (interstitialOpenWhenLoaded) {
                    // this is correct only when we waited for ad and now need return
                    fullScreenAdClosedWithError(loadAdError.getCode());
                }
                interstitialOpenWhenLoaded = false;
                logInfo(CATEGORY, loadAdError.toString());
                interstitial = null;
            }
        });
    }


    private void rewardedInitialize()
    {
        if (mRewardedUnitId.equals(""))
            return;

        rewardedFullScreenContentCallback = new FullScreenContentCallback() {
        @Override
        public void onAdClicked() {
            // Called when a click is recorded for an ad.
            if (debug) {
                logInfo(CATEGORY, "rewarded - onAdClicked");
            }
        }

        @Override
        public void onAdDismissedFullScreenContent() {
            // Called when ad is dismissed.
            if (debug) {
                logInfo(CATEGORY, "rewarded - onAdDismissedFullScreenContent");
            }
            // Set the ad reference to null so you don't show the ad a second time.
            rewarded = null;
            loadRewarded();
            if (rewardedWatched)
                fullScreenAdClosed(TAdWatchStatus.wsWatched);
            else
                fullScreenAdClosed(TAdWatchStatus.wsUserAborted);
            rewardedWatched = false;
        }

        @Override
        public void onAdFailedToShowFullScreenContent(AdError adError) {
            // Called when ad fails to show.
            if (debug) {
                logInfo(CATEGORY, "rewarded - onAdFailedToShowFullScreenContent");
            }
            rewarded = null;
            loadRewarded();
            rewardedWatched = false;
        }

        @Override
        public void onAdImpression() {
            // Called when an impression is recorded for an ad.
            if (debug) {
                logInfo(CATEGORY, "rewarded - onAdImpression");
            }
        }

        @Override
        public void onAdShowedFullScreenContent() {
            // Called when ad is shown.
            if (debug) {
                logInfo(CATEGORY, "rewarded - onAdShowedFullScreenContent");
            }
        }
        };

        // Begin loading your rewarded ad.
        loadRewarded();
    }

    public void loadRewarded() {
        if (mRewardedUnitId.equals(""))
            return;

        failedToLoadRewardedLastTime = false;
        rewardedIsLoading = true;
        rewardedLastErroCode = NO_ERROR;

        AdRequest adRequest = new AdRequest.Builder().build();

        RewardedAd.load(getActivity(), mRewardedUnitId,
            adRequest, new RewardedAdLoadCallback() {
            @Override
            public void onAdFailedToLoad(@NonNull LoadAdError loadAdError) {
                if (debug) {
                    logInfo(CATEGORY, "onRewardedVideoAdFailedToLoad");
                }
                failedToLoadRewardedLastTime = true;
                rewardedIsLoading = false;
                rewardedLastErroCode = loadAdError.getCode();
                if (rewardedOpenWhenLoaded) {
                    // this is correct only when we waited for ad and now need return 
                    fullScreenAdClosedWithError(loadAdError.getCode());
                }
                rewardedOpenWhenLoaded = false;
                
                logInfo(CATEGORY, loadAdError.toString());
                rewarded = null;
            }

            @Override
            public void onAdLoaded(@NonNull RewardedAd ad) {
                if (debug) {
                    logInfo(CATEGORY, "onRewardedVideoAdLoaded");
                }
                rewardedIsLoading = false;
                rewarded = ad;
                rewarded.setFullScreenContentCallback(rewardedFullScreenContentCallback);
                if (rewardedOpenWhenLoaded) {
                    rewardedOpenWhenLoaded = false;
                    logInfo(CATEGORY, "Show ad after waiting for ad.");
                    rewarded.show(getActivity(), new OnUserEarnedRewardListener() {
                        @Override
                        public void onUserEarnedReward(@NonNull RewardItem rewardItem) {
                            if (debug) {
                                logInfo(CATEGORY, "onUserEarnedReward");
                            }   
                            rewardedWatched = true;
                        }
                    });
                }
            }
        });
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
        AdSize size = AdSize.getCurrentOrientationAnchoredAdaptiveBannerAdSize(getActivity(), AdSize.FULL_WIDTH);
        if (size == AdSize.INVALID) {
            if (debug) {
                logInfo(CATEGORY, "getCurrentOrientationAnchoredAdaptiveBannerAdSize - failed");
            }
            size = AdSize.FLUID;

        } else {
            if (debug) {
                logInfo(CATEGORY, "getCurrentOrientationAnchoredAdaptiveBannerAdSize - succeed");
            }
        }
        
        adView.setAdSize(size);

        adPopup = new ActivityPopup(this, gravity, adView);

        adView.setVisibility(View.VISIBLE);
        AdRequest adRequest = new AdRequest.Builder().build();
        adView.loadAd(adRequest);
    }

    private void setConfiguration()
    {
        // https://developers.google.com/admob/android/test-ads?hl=pl#enable_test_devices
        List<String> testDeviceIdsList = Arrays.asList(testDeviceIds);
        RequestConfiguration configuration = new RequestConfiguration.Builder().setTestDeviceIds(testDeviceIdsList).build();
        MobileAds.setRequestConfiguration(configuration);
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
            if (waitUntilLoaded || interstitial != null) {
                if (waitUntilLoaded && interstitial == null) {
                    // calling show() when ad is not loaded do nothing, so we show ad when it will be available
                    logInfo(CATEGORY, "Requested showing interstitial ad with waitUntilLoaded, and ad not ready yet. Will wait until ad is ready.");
                    interstitialOpenWhenLoaded = true;
                    if (failedToLoadInterstitialLastTime) //loading ad failed last time so there was no load in onAdClose()
                        loadInterstitial();
                    return;
                }
                else if (interstitial != null) {
                    interstitial.show(getActivity());
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
            if (waitUntilLoaded || rewarded != null) {
                if (waitUntilLoaded && rewarded == null ) {
                    logInfo(CATEGORY, "Requested showing reward ad with waitUntilLoaded, and ad not ready yet. Will wait until ad is ready.");
                    rewardedOpenWhenLoaded = true;
                    if (failedToLoadRewardedLastTime) //loading ad failed last time so there was no load in onRewardedAdClose()
                        loadRewarded();
                    return;
                }
                else if (rewarded != null) {
                    rewarded.show(getActivity(), new OnUserEarnedRewardListener() {
                        @Override
                        public void onUserEarnedReward(@NonNull RewardItem rewardItem) {
                            if (debug) {
                                logInfo(CATEGORY, "onUserEarnedReward");
                            }   
                            rewardedWatched = true;
                        }
                    });
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
