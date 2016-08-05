/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.util.Log;

import com.google.android.gms.analytics.Logger;
import com.google.android.gms.analytics.GoogleAnalytics;
import com.google.android.gms.analytics.HitBuilders;
import com.google.android.gms.analytics.Tracker;
import com.google.android.gms.analytics.ecommerce.Product;
import com.google.android.gms.analytics.ecommerce.ProductAction;

/**
 * Google Analytics integration with Castle Game Engine Android application.
 */
public class ComponentGoogleAnalytics extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentGoogleAnalytics";
    /* To enable debug logging on a device run:
       adb shell setprop log.tag.GAv4 DEBUG
       adb logcat -s GAv4
    */
    private final boolean debug = false;

    private Tracker mTracker;

    private String mAnalyticsPropertyId;

    public ComponentGoogleAnalytics(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "google-analytics";
    }

    private synchronized Tracker getAppTracker() {
        if (mTracker == null && mAnalyticsPropertyId != null) {
            GoogleAnalytics analytics = GoogleAnalytics.getInstance(
                getActivity().getApplication());
            if (debug) {
                analytics.getLogger().setLogLevel(Logger.LogLevel.VERBOSE);
            }
            mTracker = analytics.newTracker(mAnalyticsPropertyId);
            mTracker.enableAdvertisingIdCollection(true);
            Log.i(TAG, "Created Google Analytics tracker with tracking id " + mAnalyticsPropertyId);
        }
        return mTracker;
    }

    private void initialize(String analyticsPropertyId)
    {
        mAnalyticsPropertyId = analyticsPropertyId;
    }

    /**
     * Send analytics screen view.
     * Following https://developers.google.com/analytics/devguides/collection/analyticsjs/screens
     *
     *   Screens in Google Analytics represent content users are viewing
     *   within an app. The equivalent concept for a website is pages.
     *   Measuring screen views allows you to see which content is being
     *   viewed most by your users, and how are they are navigating between
     *   different pieces of content.
     */
    private void sendScreenView(String screenName)
    {
        Tracker t = getAppTracker();
        if (t == null) {
            return;
        }

        // Set screen name.
        t.setScreenName(screenName);

        // Send a screen view.
        t.send(new HitBuilders.ScreenViewBuilder().build());
    }

    /**
     * Send analytics event.
     * See https://developers.google.com/analytics/solutions/mobile-implementation-guide
     * about the meaning of events and such.
     */
    private void sendEvent(String category, String action, String label,
        long value, int dimensionIndex, String dimensionValue)
    {
        Tracker t = getAppTracker();
        if (t == null) {
            return;
        }
        HitBuilders.EventBuilder e = new HitBuilders.EventBuilder()
            .setCategory(category)
            .setAction(action)
            .setLabel(label)
            .setValue(value);
        if (dimensionIndex > 0 && !dimensionValue.equals("")) {
            e = e.setCustomDimension(dimensionIndex, dimensionValue);
        }
        t.send(e.build());
    }

/*
    This is the wrong place to do it. It should be called
    by ComponentGoogleBiling when transaction is successfully finished.

    private void sendEventPurchase(String category, String action, String label,
        long value, String productName)
    {
        Tracker t = getAppTracker();
        if (t == null) {
            return;
        }

        Product product = new Product()
            .setName(productName)
            .setPrice(1);
        ProductAction productAction = new ProductAction(ProductAction.ACTION_PURCHASE);
        t.send(new HitBuilders.EventBuilder()
            .setCategory(category)
            .setAction(action)
            .setLabel(label)
            .setValue(value)
            .addProduct(product)
            .setProductAction(productAction)
            .build());
    }
*/

    private void sendTiming(String category, String variable,
        String label, long timeMiliseconds)
    {
        Tracker t = getAppTracker();
        if (t == null) {
            return;
        }
        t.send(new HitBuilders.TimingBuilder()
            .setCategory(category)
            .setVariable(variable)
            .setLabel(label)
            .setValue(timeMiliseconds)
            .build());
    }

    private void sendProgress(int status, String world, String level, String phase, int score)
    {
        String strStatus;
        switch (status) {
            case 0: strStatus = "start"; break;
            case 1: strStatus = "fail"; break;
            case 2: strStatus = "complete"; break;
            default:
                Log.w(TAG, "Invalid sendProgress status " + status);
                return;
        }
        sendEvent("progress", strStatus, world + "-" + level + "-" + phase, score, 0, "");
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("google-analytics-initialize")) {
            initialize(parts[1]);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("analytics-send-screen-view")) {
            sendScreenView(parts[1]);
            return true;
        } else
        if (parts.length == 7 && parts[0].equals("analytics-send-event")) {
            sendEvent(parts[1], parts[2], parts[3], Long.parseLong(parts[4]), Integer.parseInt(parts[5]), parts[6]);
            return true;
        } else
        // if (parts.length == 6 && parts[0].equals("analytics-send-event-purchase")) {
        //     sendEventPurchase(parts[1], parts[2], parts[3], Long.parseLong(parts[4]), parts[5]);
        //     return true;
        // } else
        if (parts.length == 5 && parts[0].equals("analytics-send-timing")) {
            sendTiming(parts[1], parts[2], parts[3], Long.parseLong(parts[4]));
            return true;
        } else
        if (parts.length == 6 && parts[0].equals("analytics-send-progress")) {
            sendProgress(Integer.parseInt(parts[1]), parts[2], parts[3], parts[4], Integer.parseInt(parts[5]));
            return true;
        } else
        {
            return false;
        }
    }
}
