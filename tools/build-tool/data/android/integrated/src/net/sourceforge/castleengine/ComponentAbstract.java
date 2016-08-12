/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.util.regex.Pattern;

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
     * Split the string by char code.
     *
     * The splitting is done "strictly", which means that we always return exactly
     * one more part than the occurences of delimiter in the source string.
     *
     * This is an equivalent of Object Pascal SplitString function in CastleStringUtils,
     * and the reverse of Object Pascal GlueStrings function in CastleStringUtils.
     */
    protected static String[] splitString(String input, int delimiterCharCode)
    {
        String delimiter = Character.toString((char) delimiterCharCode);

        /* Use -1 to avoid cutting off empty trailing strings.
           Which are possible in case of e.g. ads-admob-initialize
           or analytics-send-event (they very often have last parameter empty).
           See http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#split%28java.lang.String%29
           http://docs.oracle.com/javase/6/docs/api/java/lang/String.html#split%28java.lang.String,%20int%29

           Use Pattern.compile with Pattern.LITERAL to (hopefully) take advantage
           underneath of the fact that our delimiter is not a regular expression.
        */
        Pattern p = Pattern.compile(delimiter, Pattern.LITERAL);
        return p.split(input, -1);
        //return input.split(delimiter, -1);
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
    public void onPurchase(AvailableProduct product, String purchaseData, String signature) { }
}
