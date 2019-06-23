/* -*- tab-width: 4 -*- */
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

    private boolean initialized;
    private String mBannerUnitId, mInterstitialUnitId, mRewardedUnitId;

    private ActivityPopup adPopup = null;

    private InterstitialAd interstitial = null;
    private Boolean interstitialOpenWhenLoaded = false; // used when you want to wait for interstitial ad
    private Boolean failedToLoadInterstitialLastTime = false; // when loading failed last time, this is needed to try again on next call

    private RewardedVideoAd rewarded = null;
    private Boolean rewardedWatched = false; // should we set rewarded video as watched
    private Boolean rewardedOpenWhenLoaded = false; // used when you want to wait for rewarded ad
    private Boolean failedToLoadRewardedLastTime = false; // when loading failed last time, this is needed to try again on next call

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
        interstitialInitialize();
        rewardedInitialize();
        logInfo(CATEGORY, "AdMob initialized");
    }

    private void fullScreenAdClosed(boolean watched)
    {
        messageSend(new String[]{"ads-admob-full-screen-ad-closed", booleanToString(watched)});
    }

    private void interstitialInitialize()
    {
        if (mInterstitialUnitId == "") 
            return;

        // Create the interstitial.
        interstitial = new InterstitialAd(getActivity());
        interstitial.setAdUnitId(mInterstitialUnitId);
        // Set an AdListener.
        interstitial.setAdListener(new AdListener() {
            @Override
            public void onAdLoaded() {
                logInfo(CATEGORY, "onAdLoaded");
                failedToLoadInterstitialLastTime = false;
                if (interstitialOpenWhenLoaded) {
                    interstitialOpenWhenLoaded = false;
                    logInfo(CATEGORY, "Show ad after waiting for add.");
                    interstitial.show();
                }

            }

            @Override
            public void onAdFailedToLoad(int errorCode)  {
                logInfo(CATEGORY, "onAdFailedToLoad");
                failedToLoadInterstitialLastTime = true;
                if (interstitialOpenWhenLoaded)
                    fullScreenAdClosed(false);
                interstitialOpenWhenLoaded = false;
            }

            @Override
            public void onAdClosed() {
                logInfo(CATEGORY, "Ad Closed");
                fullScreenAdClosed(true);
                loadInterstitial(); // load next ad
            }
        });

        // Begin loading your interstitial.
        loadInterstitial();
    }

    public void loadInterstitial() {
        failedToLoadInterstitialLastTime = false;
        if (!interstitial.isLoaded()) {
            logInfo(CATEGORY, "start loading interstitial");
            interstitial.loadAd(buildAdRequest());
        }
    }


    private void rewardedInitialize()
    {
        if (mRewardedUnitId == "")
            return;

        MobileAds.initialize(getActivity(),"${ANDROID.ADMOB.APP_ID}");
        rewarded = MobileAds.getRewardedVideoAdInstance(getActivity());

        // Set an AdListener.
        rewarded.setRewardedVideoAdListener(new RewardedVideoAdListener()
        {
            @Override
            public void onRewarded(RewardItem reward) {
                logInfo(CATEGORY, "onRewarded");
                rewardedWatched = true;
            }
            
            @Override
            public void onRewardedVideoAdClosed() {
                logInfo(CATEGORY, "onRewardedVideoAdClosed");
                loadRewarded();
                fullScreenAdClosed(rewardedWatched);
                rewardedWatched = false;
            }

            @Override
            public void onRewardedVideoAdFailedToLoad(int errorCode) {
                logInfo(CATEGORY, "onRewardedVideoAdFailedToLoad");
                failedToLoadRewardedLastTime = true;
                if (rewardedOpenWhenLoaded)
                    fullScreenAdClosed(false);
                rewardedOpenWhenLoaded = false;
            }

            @Override
            public void onRewardedVideoAdLoaded() {
                logInfo(CATEGORY, "onRewardedVideoAdLoaded");
                if (rewardedOpenWhenLoaded) {
                    rewardedOpenWhenLoaded = false;
                    logInfo(CATEGORY, "Show ad after waiting for add.");
                    rewarded.show();
                }
            }

            @Override
            public void onRewardedVideoAdLeftApplication() {
                logInfo(CATEGORY, "onRewardedVideoAdLeftApplication");
            }

            @Override
            public void onRewardedVideoAdOpened() {
                logInfo(CATEGORY, "onRewardedVideoAdOpened");
            }

            @Override
            public void onRewardedVideoStarted() {
                logInfo(CATEGORY, "onRewardedVideoStarted");
            }

            @Override
            public void onRewardedVideoCompleted() {
                logInfo(CATEGORY, "onRewardedVideoCompleted");
            }
        });

        // Begin loading your rewarded ad.
        loadRewarded();
    }

    public void loadRewarded() {
        failedToLoadRewardedLastTime = false;
        if (!rewarded.isLoaded()) {
            logInfo(CATEGORY, "start loading rewarded video");
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
        if (initialized && mInterstitialUnitId != "") {
            if (waitUntilLoaded || interstitial.isLoaded()) {
                if (waitUntilLoaded && !interstitial.isLoaded()) {
                    // calling show() when add is not loaded do nothing, so we show add when it will be aviable
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
                // pretend that ad was displayed, in case native app waits for it
                fullScreenAdClosed(false);
            }
        } else {
            // pretend that ad was displayed, in case native app waits for it
            fullScreenAdClosed(false);
        }
    }

    /*
     * Invoke this when you are ready to display an rewarded ad.
     *
     * If waitUntilLoaded == true, we will set rewardedOpenWhenLoaded to true 
     * to show ad when it will be ready
     *
     * If waitUntilLoaded == false, we will ignore the request is the ad
     * is not ready yet (e.g. because Internet connection is slow/broken now).
     */
    private void rewardedDisplay(boolean waitUntilLoaded)
    {
        rewardedWatched = false;
        if (initialized && mRewardedUnitId != "") {
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
                // pretend that ad was displayed, in case native app waits for it
                fullScreenAdClosed(false);
            }
        } else {
            // pretend that ad was displayed, in case native app waits for it
            fullScreenAdClosed(false);
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
