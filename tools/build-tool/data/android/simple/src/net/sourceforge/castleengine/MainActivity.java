/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.app.NativeActivity;
import android.app.Activity;
import android.os.Bundle;
import android.util.Log;

public class MainActivity extends NativeActivity
{
    private static final String TAG = "${NAME}.castleengine.MainActivity";

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
        ${ANDROID_ACTIVITY_LOAD_LIBRARIES}

        safeLoadLibrary("${ANDROID_LIBRARY_NAME}");
    }
}
