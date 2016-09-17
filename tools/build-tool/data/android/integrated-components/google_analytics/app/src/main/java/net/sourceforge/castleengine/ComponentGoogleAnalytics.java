/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.util.Log;

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
       (no need for the variable below)
    */
    // private final boolean debug = false;

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
    public void onPurchase(AvailableProduct availableProduct, String purchaseData, String signature)
    {
        Tracker t = getAppTracker();
        if (t == null) {
            return;
        }

        // https://developers.google.com/analytics/devguides/collection/android/v4/mobile-implementation-guide#ecommerce
        // https://developers.google.com/android/reference/com/google/android/gms/analytics/ecommerce/Product

        Product product = new Product()
            // .setName(availableProduct.title) // would not make much sense, as it's translated?
            .setId(availableProduct.id)
            .setCategory(availableProduct.category)
            .setPrice(availableProduct.priceAmountMicros / 1000000.0);

        ProductAction productAction = new ProductAction(ProductAction.ACTION_PURCHASE);

        // Add the transaction data to the event.
        HitBuilders.EventBuilder builder = new HitBuilders.EventBuilder()
            .setCategory("defaultCart")
            .setAction("purchase")
            /* not sure, but possibly setProductAction should be done before addProduct,
               at least all examples do it like this. */
            .setProductAction(productAction)
            .addProduct(product);

        /* Crazy Google did not give normal API to set currency code at ProductAction.
         *
         * Below should work, according to
         * https://developers.google.com/analytics/devguides/collection/android/v4/enhanced-ecommerce
         * at "Specifying Currency".
         * In older API versions, this was saner:
         * https://developers.google.com/analytics/devguides/collection/android/v2/ecommerce#specifying
         * but in new API, there is no Transaction class:
         * https://developers.google.com/android/reference/com/google/android/gms/analytics/ecommerce/ProductAction
         */
        t.set("&cu", availableProduct.priceCurrencyCode);

        // Send the transaction data with the event.
        t.send(builder.build());
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
