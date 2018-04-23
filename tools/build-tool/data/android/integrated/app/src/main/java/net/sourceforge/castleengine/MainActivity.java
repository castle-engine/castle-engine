/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import android.annotation.SuppressLint;
import android.app.NativeActivity;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;

public class MainActivity extends NativeActivity
{
    private static final String CATEGORY = "MainActivity";

    private ServiceMessaging messaging;
    private List<ServiceAbstract> services = new ArrayList<ServiceAbstract>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        ServiceAbstract.logInfo(CATEGORY, "Custom castleengine.MainActivity created");

        services.add(messaging = new ServiceMessaging(this));
        services.add(new ServiceMiscellaneous(this));

        /* ANDROID-SERVICES-INITIALIZATION */

        for (ServiceAbstract service : services) {
            service.onCreate();
        }

       jniLanguage(Locale.getDefault().toString());
    }

    @Override
    public void onDestroy() {
        for (ServiceAbstract service : services) {
            service.onDestroy();
        }
        super.onDestroy();
    }

    @SuppressLint("NewApi") @Override
    public void onWindowFocusChanged(boolean hasFocus) {
        super.onWindowFocusChanged(hasFocus);
        for (ServiceAbstract service : services) {
            service.onWindowFocusChanged(hasFocus);
        }
    }

    @Override
    protected void onStart() {
        super.onStart();
        ServiceAbstract.logInfo(CATEGORY, "onStart");
        for (ServiceAbstract service : services) {
            service.onStart();
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
        ServiceAbstract.logInfo(CATEGORY, "onStop");
        for (ServiceAbstract service : services) {
            service.onStop();
        }
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent intent)
    {
        super.onActivityResult(requestCode, resultCode, intent);
        for (ServiceAbstract service : services) {
            service.onActivityResult(requestCode, resultCode, intent);
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        ServiceAbstract.logInfo(CATEGORY, "onResume");
        for (ServiceAbstract service : services) {
            service.onResume();
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        ServiceAbstract.logInfo(CATEGORY, "onPause");
        for (ServiceAbstract service : services) {
            service.onPause();
        }
    }

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        ServiceAbstract.logInfo(CATEGORY, "onNewIntent");
        for (ServiceAbstract service : services) {
            service.onNewIntent(intent);
        }
    }

    @Override
    public void onBackPressed()
    {
        ServiceAbstract.logInfo(CATEGORY, "onBackPressed");
        for (ServiceAbstract service : services) {
            if (service.onBackPressed()) {
                return;
            }
        }
        // No service says that it's processed?
        // Let default activity handler.
        super.onBackPressed();
    }

    /**
     * Send a message to native code.
     * Services (except ServiceMessaging) may call this
     * to send message to native code.
     */
    public void messageSend(String[] s)
    {
        messaging.sendFromMainActivity(s);
    }

    /**
     * Process a message from the native code.
     * Called only by ServiceMessaging.
     */
    public boolean messageReceived(String[] parts)
    {
        boolean result = false;
        boolean r;

        // Call messageReceived of all services.
        //
        // Do not stop on 1st success, our analytics services depend on it.
        // We pass all analytics-xxx messages to all analytics
        // services (google analytics and game analytics).
        // Only the ones initialized (by xxx-initialize) will actually
        // do something with it.

        for (ServiceAbstract service : services) {
            r = service.messageReceived(parts);
            result = result || r;
        }

        return result;
    }

    public void onPurchase(AvailableProduct product, String purchaseData, String signature)
    {
        ServiceAbstract.logInfo(CATEGORY, "purchase " + product.id);
        for (ServiceAbstract service : services) {
            service.onPurchase(product, purchaseData, signature);
        }
    }

    /* JNI ------------------------------------------------------------------- */

    public native String jniMessage(String javaToNative);
    public native String jniLanguage(String javaToNative);

    public static final void safeLoadLibrary(String libName)
    {
        try {
            System.loadLibrary(libName);
            ServiceAbstract.logInfo(CATEGORY, "JNI: Successfully loaded lib" + libName + ".so");
        } catch(UnsatisfiedLinkError e) {
            ServiceAbstract.logError(CATEGORY, "JNI: Could not load lib" + libName + ".so, exception UnsatisfiedLinkError: " + e.getMessage());
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
