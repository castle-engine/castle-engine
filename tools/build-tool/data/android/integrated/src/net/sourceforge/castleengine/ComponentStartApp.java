/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.util.Log;

import com.startapp.android.publish.Ad;
import com.startapp.android.publish.AdDisplayListener;
import com.startapp.android.publish.StartAppAd;
import com.startapp.android.publish.StartAppSDK;

/**
 * StartApp (https://portal.startapp.com/)
 * integration with Castle Game Engine Android application.
 *
 * See https://github.com/StartApp-SDK
 * https://github.com/StartApp-SDK/Documentation/wiki/Android-InApp-Documentation
 * https://github.com/StartApp-SDK/Documentation/wiki/android-advanced-usage
 */
public class ComponentStartApp extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentStartApp";

    private boolean mInitialized, scheduledStart, scheduledResume;
    private StartAppAd startAppAd;

    public ComponentStartApp(MainActivity activity)
    {
        super(activity);
    }

    private void initialize(String appId)
    {
        if (mInitialized) {
            return;
        }

        startAppAd = new StartAppAd(getActivity());
        StartAppSDK.init(getActivity(), appId, false);
        Log.i(TAG, "StartApp initialized (will send delayed onStart: " + scheduledStart + ", will send delayed onResume: " + scheduledResume + ")");
        mInitialized = true;

        if (scheduledStart) {
            onStart();
            scheduledStart = false;
        }
        if (scheduledResume) {
            onResume();
            scheduledResume = false;
        }
    }

    @Override
    public void onDestroy()
    {
        if (!mInitialized) {
            return;
        }
    }

    @Override
    public void onResume()
    {
        if (!mInitialized) {
            scheduledResume = true; // send onResume to startapp when it will be initialized
            return;
        }
        startAppAd.onResume();
    }

    @Override
    public void onPause()
    {
        scheduledResume = false;
        if (!mInitialized) {
            return;
        }
        startAppAd.onPause();
    }

    @Override
    public void onStart()
    {
        if (!mInitialized) {
            scheduledStart = true; // send onStart to startapp when it will be initialized
            return;
        }
        startAppAd.loadAd();
    }

    @Override
    public void onStop()
    {
        scheduledStart = false;
        if (!mInitialized) {
            return;
        }
    }

    private void showInterstitial()
    {
        if (mInitialized) {
            startAppAd.showAd(new AdDisplayListener() {
                @Override
                public void adHidden(Ad ad) {
                    Log.i(TAG, "StartApp adHidden");
                    messageSend(new String[]{"ads-startapp-interstitial-display", "shown"});
                }
                @Override
                public void adDisplayed(Ad ad) {
                    Log.i(TAG, "StartApp adDisplayed");
                }
                @Override
                public void adClicked(Ad ad) {
                    Log.i(TAG, "StartApp adClicked");
                }

                // https://github.com/StartApp-SDK/Documentation/wiki/Android-InApp-Documentation
                // says that in case you call showAd() while the ad hasn't been
                // successfully loaded yet, nothing will be displayed.
                //
                // By overriding adNotDisplayed below, we handle it, pretending that ad
                // was displayed --- important in case native code waits for ad to finish.

                @Override
                public void adNotDisplayed(Ad arg0) {
                    Log.i(TAG, "StartApp adNotDisplayed");
                    messageSend(new String[]{"ads-startapp-interstitial-display", "shown"});
                }
            }); // show the ad

            startAppAd.loadAd(); // load the next ad
        } else {
            // pretend that ad was displayed, in case native app waits for it
            messageSend(new String[]{"ads-startapp-interstitial-display", "shown"});
        }
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("ads-startapp-initialize")) {
            initialize(parts[1]);
            return true;
        } else
        if (parts.length == 1 && parts[0].equals("ads-startapp-show-interstitial")) {
            showInterstitial();
            return true;
        } else {
            return false;
        }
    }
}
