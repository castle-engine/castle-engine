/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.util.Log;
import android.content.Intent;

import android.app.Activity;

/**
 * Abstract component of MainActivity.
 */
public abstract class ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentAbstract";

    private MainActivity mActivity;

    public ComponentAbstract(MainActivity activity)
    {
        mActivity = activity;
    }

    public abstract String getName();

    public MainActivity getActivity()
    {
        return mActivity;
    }

    protected void messageSend(String[] s)
    {
        getActivity().messageSend(s);
    }

    protected static boolean stringToBoolean(String value)
    {
        if (value.equals("true")) {
            return true;
        } else
        if (value.equals("false")) {
            return false;
        } else {
            Log.w(TAG, "Invalid boolean value in message: " + value);
            return false;
        }
    }

    protected static String booleanToString(boolean value)
    {
        return value ? "true" : "false";
    }

    /**
     * Called only by MainActivity to allow this component to process this message.
     * Return is it processed.
     */
    public boolean messageReceived(String[] parts)
    {
        return false;
    }

    public void onCreate() { }
    public void onDestroy() { }
    public void onWindowFocusChanged(boolean hasFocus) { }
    public void onStart() { }
    public void onStop() { }
    public void onActivityResult(int requestCode, int resultCode, Intent intent) { }
    public void onResume() { }
    public void onPause() { }
    public boolean onBackPressed() { return false; }
}
