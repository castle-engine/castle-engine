/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.annotation.SuppressLint;
import android.app.NativeActivity;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import java.util.ArrayList;
import java.util.List;

import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.common.api.Status;

public class MainActivity extends NativeActivity implements
    GoogleApiClient.ConnectionCallbacks,
    GoogleApiClient.OnConnectionFailedListener
{
    private static final String TAG = "${NAME}.castleengine.MainActivity";

    public static int REQUEST_SIGN_IN = 9001;
    public static int REQUEST_ACHIEVEMENTS = 9101;
    public static int REQUEST_LEADERBOARD = 9102;
    public static int REQUEST_PURCHASE = 9200;

    private ComponentMessaging messaging;
    private ComponentGameAnalytics gameAnalytics;
    private ComponentGoogleAnalytics googleAnalytics;
    private ComponentMiscellaneous miscellaneous;
    private ComponentGoogleGames googleGames;
    private ComponentGoogleInAppPurchases googleInAppPurchases;
    private ComponentGoogleAds googleAds;
    private ComponentChartboost chartboost;
    private ComponentStartApp startApp;

    private List<ComponentAbstract> components = new ArrayList<ComponentAbstract>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.i(TAG, "Custom castleengine.MainActivity created");

        components.add(messaging = new ComponentMessaging(this));
        components.add(gameAnalytics = new ComponentGameAnalytics(this));
        components.add(googleAnalytics = new ComponentGoogleAnalytics(this));
        components.add(miscellaneous = new ComponentMiscellaneous(this));
        components.add(googleGames = new ComponentGoogleGames(this));
        components.add(googleInAppPurchases = new ComponentGoogleInAppPurchases(this));
        components.add(googleAds = new ComponentGoogleAds(this));
        components.add(chartboost = new ComponentChartboost(this));
        components.add(startApp = new ComponentStartApp(this));

        for (ComponentAbstract component : components) {
            component.onCreate();
        }
    }

    @Override
    public void onDestroy() {
        for (ComponentAbstract component : components) {
            component.onDestroy();
        }
        super.onDestroy();
    }

    @SuppressLint("NewApi") @Override
    public void onWindowFocusChanged(boolean hasFocus) {
        super.onWindowFocusChanged(hasFocus);
        for (ComponentAbstract component : components) {
            component.onWindowFocusChanged(hasFocus);
        }
    }

    @Override
    protected void onStart() {
        super.onStart();
        Log.i(TAG, "onStart");
        for (ComponentAbstract component : components) {
            component.onStart();
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
        Log.i(TAG, "onStop");
        for (ComponentAbstract component : components) {
            component.onStop();
        }
    }

    @Override
    public void onConnected(Bundle connectionHint)
    {
        googleGames.onConnected();
    }

    @Override
    public void onConnectionFailed(ConnectionResult connectionResult)
    {
        googleGames.onConnectionFailed(connectionResult);
    }

    @Override
    public void onConnectionSuspended(int i)
    {
        googleGames.onConnectionSuspended();
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent intent)
    {
        super.onActivityResult(requestCode, resultCode, intent);
        for (ComponentAbstract component : components) {
            component.onActivityResult(requestCode, resultCode, intent);
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        Log.i(TAG, "onResume");
        for (ComponentAbstract component : components) {
            component.onResume();
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        Log.i(TAG, "onPause");
        for (ComponentAbstract component : components) {
            component.onPause();
        }
    }

    @Override
    public void onBackPressed()
    {
        Log.i(TAG, "onBackPressed");
        for (ComponentAbstract component : components) {
            if (component.onBackPressed()) {
                return;
            }
        }
        // No component says that it's processed?
        // Let default activity handler.
        super.onBackPressed();
    }

    /**
     * Send a message to native code.
     * Components (except ComponentMessaging) may call this
     * to send message to native code.
     */
    public void messageSend(String[] s)
    {
        messaging.sendFromMainActivity(s);
    }

    /**
     * Process a message from the native code.
     * Called only by ComponentMessaging.
     */
    public boolean messageReceived(String[] parts)
    {
        boolean result = false;
        boolean r;

        // Call messageReceived of all components.
        //
        // Do not stop on 1st success, our analytics components depend on it.
        // We pass all analytics-xxx messages to all analytics
        // components (google analytics and game analytics).
        // Only the ones initialized (by xxx-initialize) will actually
        // do something with it.

        for (ComponentAbstract component : components) {
            r = component.messageReceived(parts);
            result = result || r;
        }

        return result;
    }

    /* JNI ------------------------------------------------------------------- */

    public native String jniMessage(String javaToNative);

    private static final void safeLoadLibrary(String libName)
    {
        try {
            System.loadLibrary(libName);
            Log.i(TAG, "JNI: Successfully loaded lib" + libName + ".so");
        } catch(UnsatisfiedLinkError ule) {
            Log.e(TAG, "JNI: Could not load lib" + libName + ".so");
            ule.printStackTrace();
        }
    }

    static {
        safeLoadLibrary("GameAnalytics");

        /* TODO: for now, disable sounds, slow on Android
        safeLoadLibrary("openal");
        */

        safeLoadLibrary("${ANDROID_LIBRARY_NAME}");
    }
}
