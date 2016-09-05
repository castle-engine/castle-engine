/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.util.ArrayList;
import java.util.List;

import android.annotation.SuppressLint;
import android.app.NativeActivity;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;

public class MainActivity extends NativeActivity
{
    private static final String TAG = "${NAME}.castleengine.MainActivity";

    private ComponentMessaging messaging;
    private List<ComponentAbstract> components = new ArrayList<ComponentAbstract>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.i(TAG, "Custom castleengine.MainActivity created");

        components.add(messaging = new ComponentMessaging(this));
        components.add(new ComponentMiscellaneous(this));

        /* ANDROID-COMPONENTS-INITIALIZATION */

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

    public void onPurchase(AvailableProduct product, String purchaseData, String signature)
    {
        Log.i(TAG, "purchase " + product.id);
        for (ComponentAbstract component : components) {
            component.onPurchase(product, purchaseData, signature);
        }
    }

    /* JNI ------------------------------------------------------------------- */

    public native String jniMessage(String javaToNative);

    public static final void safeLoadLibrary(String libName)
    {
        try {
            System.loadLibrary(libName);
            Log.i(TAG, "JNI: Successfully loaded lib" + libName + ".so");
        } catch(UnsatisfiedLinkError e) {
            Log.e(TAG, "JNI: Could not load lib" + libName + ".so, exception UnsatisfiedLinkError: " + e.getMessage());
        }
    }

    static {
        /* OpenAL may be loaded from here. It must be loaded before
           our game's native code will try to open openal library,
           so it's most reliable to place it here before loading game's library. */

        ${ANDROID_ACTIVITY_LOAD_LIBRARIES}

        safeLoadLibrary("${ANDROID_LIBRARY_NAME}");
    }
}
