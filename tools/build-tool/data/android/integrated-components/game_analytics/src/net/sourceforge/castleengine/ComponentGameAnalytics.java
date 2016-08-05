/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.app.Activity;
import android.util.Log;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.PackageInfo;
import android.os.Build;

import com.gameanalytics.sdk.*;

/**
 * Game Analytics (http://www.gameanalytics.com/)
 * integration with Castle Game Engine Android application.
 */
public class ComponentGameAnalytics extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentGameAnalytics";

    private boolean initialized;

    public ComponentGameAnalytics(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "game-analytics";
    }

    /**
     * Initialize the integration. Pass gameKey and secretKey that should
     * be passed to GameAnalytics.initializeWithGameKey method ---
     * you should generate
     * these values when creating new game in your gameanalytics.com panel.
     */
    private void initialize(String gameKey, String secretKey)
    {
        // Set build version
        PackageManager manager = getActivity().getPackageManager();
        String version;
        try {
            PackageInfo info = manager.getPackageInfo(getActivity().getPackageName(), 0);
            version = info.versionName;
        } catch (NameNotFoundException e) {
            version = "unknown (NameNotFoundException)";
        }
        version = "android " + version;
        GameAnalytics.configureBuild(version);

        // Enable log
        // Enable info log when implementing the SDK - remember to turn it off in production!
        // watch by adb logcat | grep -i gameanalytics
        // GameAnalytics.setEnabledInfoLog(true);
        // GameAnalytics.setEnabledVerboseLog(true);

        // Initialize
        GameAnalytics.initializeWithGameKey(getActivity(), gameKey, secretKey);

        Log.i(TAG, "GameAnalytics initialized with application version " + version);

        initialized = true;
    }

    @Override
    public void onResume()
    {
        if (!initialized) {
            return;
        }
        // Only needed if your API level is below 14.
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.ICE_CREAM_SANDWICH) {
            GAPlatform.onActivityResumed(getActivity());
        }
    }

    @Override
    public void onPause()
    {
        if (!initialized) {
            return;
        }
        // Only needed if your API level is below 14.
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.ICE_CREAM_SANDWICH) {
            GAPlatform.onActivityPaused(getActivity());
        }
    }

    @Override
    public void onStop()
    {
        if (!initialized) {
            return;
        }
        // Only needed if your API level is below 14.
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.ICE_CREAM_SANDWICH) {
            GAPlatform.onActivityStopped(getActivity());
        }
    }

    private void sendScreenView(String screenName)
    {
        if (!initialized) {
            return;
        }
        GameAnalytics.addDesignEventWithEventId("screenView:" + screenName);
    }

    private void sendEvent(String category, String action, String label,
        long value, int dimensionIndex, String dimensionValue)
    {
        if (!initialized) {
            return;
        }
        String id = category + ":" + action + ":" + label;
        if (dimensionIndex > 0 && !dimensionValue.equals("")) {
            id = id + ":dimension" + dimensionIndex + "." + dimensionValue;
        }
        GameAnalytics.addDesignEventWithEventId(id, (float)value);
    }

/*
    This is the wrong place to do it. It should be called
    by ComponentGoogleBiling when transaction is successfully finished.

    private void sendEventPurchase(String category, String action, String label,
        long value, String productName)
    {
        if (!initialized) {
            return;
        }
        // For now, we use productName as "currency".
        // This will need to change once we start to experiment with different
        // prices for the same item.
        GameAnalytics.addBusinessEventWithCurrency(productName, 1,
          "items", productName, category + ":" + action + ":" + label,
          // careful: contrary to docs, you cannot put here null as receipt/signature, it will crash with
          // E/AndroidRuntime(15299): java.lang.NullPointerException: null string
          // E/AndroidRuntime(15299): 	at com.gameanalytics.sdk.GameAnalyticsSDKJNI.CppWrapper_addBusinessEventWithCurrency__SWIG_1(Native Method)
          "", "google_play", "");
    }
*/

    private void sendTiming(String category, String variable,
        String label, long timeMiliseconds)
    {
        if (!initialized) {
            return;
        }
        GameAnalytics.addDesignEventWithEventId(
            "timing:" + category + ":" + variable + ":" + label, (float)timeMiliseconds);
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 3 && parts[0].equals("game-analytics-initialize")) {
            initialize(parts[1], parts[2]);
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
        } else {
            return false;
        }
    }
}
