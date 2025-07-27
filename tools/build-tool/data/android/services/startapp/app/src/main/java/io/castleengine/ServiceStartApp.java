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

package io.castleengine;

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
public class ServiceStartApp extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceStartApp";

    private boolean initialized, scheduledStart, scheduledResume;
    private StartAppAd startAppAd;

    public ServiceStartApp(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "startapp";
    }

    private void initialize(String appId)
    {
        if (initialized) {
            return;
        }

        startAppAd = new StartAppAd(getActivity());
        StartAppSDK.init(getActivity(), appId, false);
        logInfo(CATEGORY, "StartApp initialized (will send delayed onStart: " + scheduledStart + ", will send delayed onResume: " + scheduledResume + ")");
        initialized = true;

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
        if (!initialized) {
            return;
        }
    }

    @Override
    public void onResume()
    {
        if (!initialized) {
            scheduledResume = true; // send onResume to startapp when it will be initialized
            return;
        }
        startAppAd.onResume();
    }

    @Override
    public void onPause()
    {
        scheduledResume = false;
        if (!initialized) {
            return;
        }
        startAppAd.onPause();
    }

    @Override
    public void onStart()
    {
        if (!initialized) {
            scheduledStart = true; // send onStart to startapp when it will be initialized
            return;
        }
        startAppAd.loadAd();
    }

    @Override
    public void onStop()
    {
        scheduledStart = false;
        if (!initialized) {
            return;
        }
    }

    private void fullScreenAdClosed(boolean watched)
    {
        messageSend(new String[]{"ads-startapp-full-screen-ad-closed", booleanToString(watched)});
    }

    private void showInterstitial()
    {
        if (initialized) {
            startAppAd.showAd(new AdDisplayListener() {
                @Override
                public void adHidden(Ad ad) {
                    logInfo(CATEGORY, "StartApp adHidden");
                    fullScreenAdClosed(true);
                }
                @Override
                public void adDisplayed(Ad ad) {
                    logInfo(CATEGORY, "StartApp adDisplayed");
                }
                @Override
                public void adClicked(Ad ad) {
                    logInfo(CATEGORY, "StartApp adClicked");
                }

                // https://github.com/StartApp-SDK/Documentation/wiki/Android-InApp-Documentation
                // says that in case you call showAd() while the ad hasn't been
                // successfully loaded yet, nothing will be displayed.
                //
                // By overriding adNotDisplayed below, we handle it, pretending that ad
                // was displayed --- important in case native code waits for ad to finish.

                @Override
                public void adNotDisplayed(Ad arg0) {
                    logInfo(CATEGORY, "StartApp adNotDisplayed");
                    fullScreenAdClosed(false);
                }
            }); // show the ad

            startAppAd.loadAd(); // load the next ad
        } else {
            // pretend that ad was displayed, in case native app waits for it
            fullScreenAdClosed(false);
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
