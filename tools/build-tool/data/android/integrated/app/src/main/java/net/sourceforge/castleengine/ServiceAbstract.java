/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.util.regex.Pattern;

import android.util.Log;
import android.content.Intent;
import android.app.Activity;

/**
 * Abstract service of MainActivity.
 */
public abstract class ServiceAbstract
{
    private static final String CATEGORY = "ServiceAbstract";

    private MainActivity mActivity;

    public ServiceAbstract(MainActivity activity)
    {
        mActivity = activity;
    }

    /* Log information.
       Use this instead of Log.i throughout the whole Java code.
       This way we always pass the same "tag" for logging,
       and our logs can be filtered using "adb logcat" filtering features.
       And the length of "category" is not limited this way (contrary
       to Android log "tag", that has limited length). */
    public static final void logInfo(String category, String message)
    {
        Log.i("${ANDROID_LOG_TAG}", category + ": " + message);
    }

    /* Log error. See logInfo for more comments. */
    public static final void logError(String category, String message)
    {
        Log.e("${ANDROID_LOG_TAG}", category + ": " + message);
    }

    /* Log warning. See logInfo for more comments. */
    public static final void logWarning(String category, String message)
    {
        Log.w("${ANDROID_LOG_TAG}", category + ": " + message);
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
            logWarning(CATEGORY, "Invalid boolean value in message: " + value);
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
     * Called only by MainActivity to allow this service to process this message.
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
    public void onNewIntent(Intent intent) { }
    public boolean onBackPressed() { return false; }
    public void onPurchase(AvailableProduct product, String purchaseData, String signature) { }
}
