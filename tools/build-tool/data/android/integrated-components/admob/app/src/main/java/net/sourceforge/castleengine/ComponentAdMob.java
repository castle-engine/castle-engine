/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.util.Log;
import android.view.View;

import com.google.android.gms.ads.AdRequest;
import com.google.android.gms.ads.AdSize;
import com.google.android.gms.ads.AdView;
import com.google.android.gms.ads.InterstitialAd;
import com.google.android.gms.ads.AdListener;

/**
 * Integration of Google Ads (AdMob) with Castle Game Engine.
 */
public class ComponentAdMob extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentAdMob";

    private boolean initialized;
    private String mBannerUnitId, mInterstitialUnitId;
    private ActivityPopup adPopup;
    private InterstitialAd interstitial;
    private String[] testDeviceIds;

    public ComponentAdMob(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "admob";
    }

    private void initialize(String bannerUnitId, String interstitialUnitId, String[] aTestDeviceIds)
    {
        if (initialized) {
            return;
        }

        initialized = true;
        mBannerUnitId = bannerUnitId;
        mInterstitialUnitId = interstitialUnitId;
        testDeviceIds = aTestDeviceIds;
        interstitialInitialize();
        Log.i(TAG, "AdMob initialized");
    }

    private void fullScreenAdClosed(boolean watched)
    {
        messageSend(new String[]{"ads-admob-full-screen-ad-closed", booleanToString(watched)});
    }

    private void interstitialInitialize()
    {
        // Create the interstitial.
        interstitial = new InterstitialAd(getActivity());
        interstitial.setAdUnitId(mInterstitialUnitId);
        // Set an AdListener.
        interstitial.setAdListener(new AdListener() {
            // unused now
            // @Override
            // public void onAdLoaded() {
            // }

            @Override
            public void onAdClosed() {
                Log.i(TAG, "Ad Closed");
                fullScreenAdClosed(true);
            }
        });

        // Begin loading your interstitial.
        interstitial.loadAd(buildAdRequest());
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
     * If waitUntilLoaded == true, we will wait until the ad is loaded.
     *
     * If waitUntilLoaded == false, we will ignore the request is the ad
     * is not ready yet (e.g. because Internet connection is slow/broken now).
     */
    private void interstitialDisplay(boolean waitUntilLoaded)
    {
        if (initialized) {
            if (waitUntilLoaded || interstitial.isLoaded()) {
                if (waitUntilLoaded && !interstitial.isLoaded()) {
                    Log.i(TAG, "Requested showing interstitial ad with waitUntilLoaded, and ad not ready yet. Will wait until ad is ready.");
                }
                interstitial.show();
                interstitialInitialize(); // load next interstitial ad
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
        if (parts.length == 4 && parts[0].equals("ads-admob-initialize")) {
            initialize(parts[1], parts[2], parts[3].split(","));
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
        } else {
            return false;
        }
    }
}
