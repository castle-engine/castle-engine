/* -*- tab-width: 4 -*- */
package io.castleengine;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.NativeActivity;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.content.pm.PackageManager;

import androidx.core.app.ActivityCompat;

public class MainActivity extends NativeActivity
{
    private static final String CATEGORY = "MainActivity";

    private ServiceMessaging messaging;
    private final List<ServiceAbstract> services = new ArrayList<>();

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

    /**
     * Send a message to native code.
     * Services (except ServiceMessaging) may call this
     * to send message to native code.
     */
    public void messageSend(String[] message, byte[] messageStream)
    {
        messaging.sendFromMainActivity(message, messageStream);
    }

    public void messageSend(String[] message)
    {
        messageSend(message, null);
    }

    /**
     * Process a message from the native code.
     * Called only by ServiceMessaging.
     */
    public boolean messageReceived(String[] parts)
    {
        boolean result = false;
        boolean r;

        // Check messages handled directly in this class
        if (parts.length == 2 && parts[0].equals("permission-request")) {
            requestPermission(parts[1]);
            result = true;
        }

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

    public void onPurchase(AvailableProduct product, String originalJson, String signature)
    {
        ServiceAbstract.logInfo(CATEGORY, "purchase " + product.id);
        for (ServiceAbstract service : services) {
            service.onPurchase(product, originalJson, signature);
        }
    }

    /* JNI ------------------------------------------------------------------- */

    public native String jniMessage(String messageToPascal, byte[] messageToPascalStream);
    public native void jniLanguage(String javaToNative);

    public static void safeLoadLibrary(String libName)
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

    private static final int MY_PERMISSIONS_REQUEST = 1;

    /* Requires Android permission name.

       @param permission Can be one of the Manifest.permission constants,
         like Manifest.permission.WRITE_EXTERNAL_STORAGE,
         see https://developer.android.com/reference/android/Manifest.permission.html .
         You can also just type a string like "android.permission.WRITE_EXTERNAL_STORAGE"
         (this is useful when permission is not specified in Java code,
         so it's easier to just provide a string value of it).

       See
       https://developer.android.com/training/permissions/requesting#java
       https://developer.android.com/training/permissions/usage-notes

       In response, will always (but maybe asynchronously) answer with
       "permission-granted" or "permission-cancelled".
    */
    public void requestPermission(String permission)
    {
        if (ActivityCompat.checkSelfPermission(this, permission)
          != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(this,
                new String[]{permission},
                MY_PERMISSIONS_REQUEST);
        } else {
            messageSend(new String[]{"permission-granted", permission});
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode,
        String[] permissions, int[] grantResults)
    {
        switch (requestCode) {
            case MY_PERMISSIONS_REQUEST: {
                // If request is cancelled, the result arrays are empty.
                if (grantResults.length > 0
                    && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    messageSend(new String[]{"permission-granted", permissions[0]});
                } else {
                    messageSend(new String[]{"permission-cancelled", permissions[0]});
                }
                return;
            }
        }
    }
}
