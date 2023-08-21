/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.util.regex.Pattern;

import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.content.Intent;
import android.app.Activity;

/**
 * Abstract service of MainActivity.
 */
public abstract class ServiceAbstract
{
    private static final String CATEGORY = "ServiceAbstract";

    private final MainActivity mActivity;

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
    public static void logInfo(String category, String message)
    {
        Log.i("${ANDROID_LOG_TAG}", category + ": " + message);
    }

    /* Log error. See logInfo for more comments. */
    public static void logError(String category, String message)
    {
        Log.e("${ANDROID_LOG_TAG}", category + ": " + message);
    }

    /* Log warning. See logInfo for more comments. */
    public static void logWarning(String category, String message)
    {
        Log.w("${ANDROID_LOG_TAG}", category + ": " + message);
    }

    public abstract String getName();

    public MainActivity getActivity()
    {
        return mActivity;
    }

    /*
     * Send message to Pascal, will be received by CastleMessaging unit.
     * Use this when working in main Java thread.
     *
     * Warning: Do not modify the messageStream contents after calling this.
     * The instance of messageStream may be stored in a message queue for
     * undefined time period. IOW, message and messageStream should
     * be constant after you call messageSend.
     */
    protected void messageSend(String[] message, byte[] messageStream)
    {
        getActivity().messageSend(message, messageStream);
    }

    protected void messageSend(String[] message)
    {
        messageSend(message, null);
    }

    /*
     * Send message to Pascal, will be received by CastleMessaging unit.
     * Use this when (possibly) working from other thread than the main Java thread.
     *
     * Warning: Do not modify the messageStream contents after calling this.
     * The instance of messageStream may be stored in a message queue for
     * undefined time period. IOW, message and messageStream should
     * be constant after you call messageSend.
     */
    protected void messageSendFromThread(final String[] message, final byte[] messageStream)
    {
        new Handler(Looper.getMainLooper()).post(new Runnable() // run in main thread
            {
                @Override
                public void run()
                {
                    messageSend(message, messageStream);
                }
            }
        );
    }

    protected void messageSendFromThread(String[] message)
    {
        messageSendFromThread(message, null);
    }

    /*
     * Convert message part (from Pascal) to Boolean.
     * Use this in messageReceived overrides.
     * The counter-part of this is Pascal CastleMessaging.BoolToStr.
     */
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

    /*
     * Convert boolean into message part (for Pascal).
     * Use this with messageSend.
     * The counter-part of this is Pascal CastleMessaging.MessageToBoolean.
     */
    protected static String booleanToString(boolean value)
    {
        return value ? "true" : "false";
    }

    /**
     * Split the string by char code.
     *
     * The splitting is done "strictly", which means that we always return exactly
     * one more part than the occurrences of delimiter in the source string.
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
    public void onPurchase(AvailableProduct product, String originalJson, String signature) { }
}
