/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.widget.LinearLayout;
import android.widget.PopupWindow;
import android.view.ViewGroup.LayoutParams;
import android.view.ViewGroup.MarginLayoutParams;
import android.view.View;
import android.util.Log;

import com.google.android.gms.ads.AdRequest;
import com.google.android.gms.ads.AdSize;
import com.google.android.gms.ads.AdView;
import com.google.android.gms.ads.InterstitialAd;
import com.google.android.gms.ads.AdListener;

/**
 * Integration of Google Ads with Castle Game Engine.
 */
public class ComponentGoogleAds extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentGoogleAds";

    private boolean initialized;
    private String mBannerUnitId, mInterstitialUnitId;
    private PopupWindow adPopup;
    private InterstitialAd interstitial;
    private String[] testDeviceIds;

    public ComponentGoogleAds(MainActivity activity)
    {
        super(activity);
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
                messageSend(new String[]{"ads-google-interstitial-display", "shown"});
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
        adView.setAdSize(AdSize.BANNER);

        // Inspired by http://www.dynadream.com/ddweb/index.php/Special_Blog?id=20
        adPopup = new PopupWindow(getActivity());

        // This is the minimum size for AdMob, we need to set this in case our target device run at 320x480 resolution (Otherwise no ad will be shown, see the padding kill below)
        // Trick from http://www.dynadream.com/ddweb/index.php/Special_Blog?id=20
        adPopup.setWidth(320);
        adPopup.setHeight(50);
        adPopup.setWindowLayoutMode(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
        adPopup.setClippingEnabled(false);

        LinearLayout popupLayout = new LinearLayout(getActivity());
        // The layout system for the PopupWindow will kill some pixels due
        // to margins/paddings etc... (No way to remove it), so padd it to adjust
        // Trick from http://www.dynadream.com/ddweb/index.php/Special_Blog?id=20
        popupLayout.setPadding(-10, -10, -10, -10);
        MarginLayoutParams params = new MarginLayoutParams(
            LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
        params.setMargins(0, 0, 0, 0);
        popupLayout.setOrientation(LinearLayout.VERTICAL);
        popupLayout.addView(adView, params);
        adPopup.setContentView(popupLayout);

        //ViewGroup decorView = (ViewGroup)getWindow().getDecorView();
        //View gameView = decorView.getChildAt(0);
        View gameView = getActivity().findViewById(android.R.id.content);

        // Note: do not call adPopup.showAtLocation earlier,
        // see http://stackoverflow.com/questions/17787011/android-view-windowmanagerbadtokenexception-unable-to-add-window-token-null
        adPopup.showAtLocation(gameView, gravity, 0, 0);

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
            adPopup.dismiss();
            adPopup = null;
        }
    }

    /*
     * Invoke this when you are ready to display an interstitial.
     *
     * If waitUntilLoaded == true, we will wait until the ad is loaded.
     * This guarantees you get message back ad-interstitial-display=shown
     * when the ad is eventually closed.
     *
     * If waitUntilLoaded == false, we will ignore the request is the ad
     * is not ready yet (e.g. because Internet connection is slow/broken now).
     * So you cannot depend on getting back ad-interstitial-display=shown!
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
            }
        } else {
            // pretend that ad was displayed, in case native app waits for it
            messageSend(new String[]{"ads-google-interstitial-display", "shown"});
        }
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 4 && parts[0].equals("ads-google-initialize")) {
            initialize(parts[1], parts[2], parts[3].split(","));
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-google-banner-show")) {
            int gravity = Integer.parseInt(parts[1]);
            bannerShow(gravity);
            return true;
        } else
        if (parts.length == 1 && parts[0].equals("ads-google-banner-hide")) {
            bannerHide();
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-google-interstitial-display") && parts[1].equals("wait-until-loaded")) {
            interstitialDisplay(true);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("ads-google-interstitial-display") && parts[1].equals("no-wait")) {
            interstitialDisplay(false);
            return true;
        } else {
            return false;
        }
    }
}
