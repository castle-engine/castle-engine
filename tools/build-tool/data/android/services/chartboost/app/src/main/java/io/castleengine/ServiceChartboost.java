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

import com.chartboost.sdk.CBLocation;
import com.chartboost.sdk.Chartboost;
import com.chartboost.sdk.ChartboostDelegate;
import com.chartboost.sdk.Libraries.CBLogging.Level;
import com.chartboost.sdk.Model.CBError.CBImpressionError;

/**
 * Chartboost (https://www.chartboost.com/)
 * integration with Castle Game Engine Android application.
 */
public class ServiceChartboost extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceChartboost";

    private boolean initialized, scheduledStart, scheduledResume;

    public ServiceChartboost(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "chartboost";
    }

    private void fullScreenAdClosed(boolean watched, boolean cacheNext)
    {
        messageSend(new String[]{"ads-chartboost-full-screen-ad-closed", booleanToString(watched)});
        if (cacheNext) {
            // cache next interstitial.
            // Don't do this in case loading of previous one failed,
            // as we would spam console with failures.
            Chartboost.cacheInterstitial(CBLocation.LOCATION_DEFAULT);
        }
    }

    ChartboostDelegate delegate = new ChartboostDelegate()
    {
        // Override the Chartboost delegate callbacks you wish to track and control
        @Override
        public void didCloseInterstitial(String location)
        {
            super.didCloseInterstitial(location);
            logInfo(CATEGORY, "Chartbooost Interstitial Close, location: "+ (location != null ? location : "null"));
            fullScreenAdClosed(true, true);
        }

        @Override
        public void didDismissInterstitial(String location) {
            super.didDismissInterstitial(location);
            logInfo(CATEGORY, "Chartbooost Interstitial Dismiss, location: "+ (location != null ? location : "null"));
            // react to dismiss (this actually happens before ad is closed,
            // it's when user switches out of our app).
            // Needed, since we don't get Closed callback
            // when user presses on the app.
            fullScreenAdClosed(true, true);
        }

        @Override
        public void didFailToLoadInterstitial(String location, CBImpressionError error) {
            logInfo(CATEGORY, "Chartbooost Interstitial FAIL TO LOAD, location: " +
                (location != null ? location : "null") + ", error: " + error.name());
            fullScreenAdClosed(false, false);
        }

        @Override
        public void didClickInterstitial(String location) {
            super.didClickInterstitial(location);
            logInfo(CATEGORY, "Chartbooost Interstitial Click, location: "+ (location != null ? location : "null"));
        }

        @Override
        public void didDisplayInterstitial(String location) {
            super.didDisplayInterstitial(location);
            logInfo(CATEGORY, "Chartbooost Interstitial Display, location: " +  (location != null ? location : "null"));
        }
    };

    private void initialize(String appId, String appSignature)
    {
        if (initialized) {
            return;
        }

        Chartboost.startWithAppId(getActivity(), appId, appSignature);

        // Necessary following
        //   https://github.com/freshplanet/ANE-Chartboost/issues/12
        //   https://answers.chartboost.com/hc/en-us/articles/201219545-Android-Integration
        // Later version of
        //   https://answers.chartboost.com/hc/en-us/articles/201219545-Android-Integration
        //   suggests it's no longer required for SDK 6.x, it's also marked as deprecated in Java.
        //   We'll remove this call soon.
        //Chartboost.setImpressionsUseActivities(true);

        //Chartboost.setLoggingLevel(Level.ALL); // not on prod!
        Chartboost.setDelegate(delegate);
        Chartboost.onCreate(getActivity());
        logInfo(CATEGORY, "Chartboost initialized (will send delayed onStart: " + scheduledStart + ", will send delayed onResume: " + scheduledResume + ")");
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
        Chartboost.onDestroy(getActivity());
    }

    @Override
    public void onResume()
    {
        if (!initialized) {
            scheduledResume = true; // send onResume to Chartboost SDK when we will be initialized
            return;
        }
        Chartboost.onResume(getActivity());
    }

    @Override
    public void onPause()
    {
        scheduledResume = false;
        if (!initialized) {
            return;
        }
        Chartboost.onPause(getActivity());
    }

    @Override
    public void onStart()
    {
        if (!initialized) {
            scheduledStart = true; // send onStart to Chartboost SDK when we will be initialized
            return;
        }
        Chartboost.onStart(getActivity());
        // we cannot call Chartboost.cacheInterstitial in onCreate, chartboost makes exception then
        // java.lang.Exception: Session not started: Check if Chartboost.onStart() is called, if not the session won't be invoked
        Chartboost.cacheInterstitial(CBLocation.LOCATION_DEFAULT);
    }

    @Override
    public void onStop()
    {
        scheduledStart = false;
        if (!initialized) {
            return;
        }
        Chartboost.onStop(getActivity());
    }

    /* TODO: Upgrade to latest Chartboost SDK, that doesn't have
       Chartboost.onBackPressed() looking at
       https://developers.chartboost.com/docs/monetization-android-get-started

       onBackPressed was deprecated in Android for Activity,
       we removed it from ServiceAbstract.

    @Override
    public boolean onBackPressed()
    {
        if (!initialized) {
            return false; // let default activity onBackPressed to work
        }

        // If an interstitial is on screen, close it.
        return Chartboost.onBackPressed();
    }
    */

    private void showInterstitial()
    {
        if (initialized) {
            if (!Chartboost.hasInterstitial(CBLocation.LOCATION_DEFAULT)) {
                logInfo(CATEGORY, "Interstitial not in cache yet, will wait for it");
            }
            logInfo(CATEGORY, "Interstitial showing");
            Chartboost.showInterstitial(CBLocation.LOCATION_DEFAULT);
        } else {
            // pretend that ad was displayed, in case native app waits for it
            fullScreenAdClosed(false, false);
        }
    }

    /* TODO: track purchases using chartboost analytics, see

    `Google`

            CBAnalytics.trackInAppGooglePlayPurchaseEvent("xxx-title",
                            "xxx-description",
                            "$0.99",
                            "USD",
                            "xxx-id",
                            "xxx-data",
                            "xxx-signature");

    `Amazon`

            CBAnalytics.trackInAppAmazonStorePurchaseEvent("xxx-title",
                            "xxx-description",
                            "$0.99",
                            "USD",
                            "xxx-id",
                            "xxx-userId",
                            "xxx-token");

    */

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 3 && parts[0].equals("ads-chartboost-initialize")) {
            initialize(parts[1], parts[2]);
            return true;
        } else
        if (parts.length == 1 && parts[0].equals("ads-chartboost-show-interstitial")) {
            showInterstitial();
            return true;
        } else {
            return false;
        }
    }
}
