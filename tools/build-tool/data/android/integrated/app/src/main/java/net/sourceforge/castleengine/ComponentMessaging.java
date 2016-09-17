/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import java.util.ArrayList;
import java.util.List;

import android.os.Handler;
import android.util.Log;

/**
 * Simple communication between Java and native code using text messages,
 * for Castle Game Engine.
 */
public class ComponentMessaging extends ComponentAbstract
{
    private static final String TAG = "${NAME}.castleengine.ComponentMessaging";

    public String getName()
    {
        return "messaging";
    }

    /**
     * Allow debug stuff. Right now this means to
     * log native messages. Do not show this on production, to make it harder
     * for cheaters to fake their achievements/scores by guessing what JNI
     * messages to send.
     */
    private final boolean debug = false;

    public ComponentMessaging(MainActivity activity)
    {
        super(activity);
    }

    private final int MESSAGE_DELIMITER_CODE = 1;
    private final String MESSAGE_DELIMITER = Character.toString ((char) MESSAGE_DELIMITER_CODE);

    /**
     * Helper utility to glue string array.
     */
    private static String glueStringArray(String[] input, int startIndex,
        String separator)
    {
        StringBuilder builder = new StringBuilder(input[startIndex]);
        for (int i = startIndex + 1; i < input.length; i++) {
            builder.append(separator + input[i]);
        }
        return builder.toString();
    }

    private List<String> toNative = new ArrayList<String>();
    public void sendFromMainActivity(String[] messageParts)
    {
        String s = glueStringArray(messageParts, 0, MESSAGE_DELIMITER);
        toNative.add(s);
    }

    /* Timer code inspired by
     * http://stackoverflow.com/questions/4597690/android-timer-how
     */
    Handler timerHandler = new Handler();
    Runnable timerRunnable = new Runnable()
    {
        /**
         * Do periodic stuff here.
         */
        @Override
        public void run()
        {
            boolean somethingHappened = false;

            String toNativeStr = null;
            if (toNative.size() != 0) {
                toNativeStr = toNative.get(0);
                if (debug) {
                    Log.i(TAG, "JNI: Java posting a message to the native code: " + toNativeStr);
                }
                somethingHappened = true;
                toNative.remove(0);
            }

            String message = getActivity().jniMessage(toNativeStr);
            if (message != null && message.length() != 0) {
                if (debug) {
                    Log.i(TAG, "JNI: Java received message from native code: " + message);
                }
                somethingHappened = true;
                String[] parts = splitString(message, MESSAGE_DELIMITER_CODE);
                if (!getActivity().messageReceived(parts)) {
                    Log.w(TAG, "Message unhandled: " + glueStringArray(parts, 0, "\n"));
                }
            }

            // run again in a short time (shorter if something happened,
            // since next messages may follow)
            timerHandler.postDelayed(this, somethingHappened ? 10 : 500);
        }
    };

    @Override
    public void onResume() {
        // start timer
        timerHandler.postDelayed(timerRunnable, 0);
    }

    @Override
    public void onPause() {
        // stop timer
        timerHandler.removeCallbacks(timerRunnable);
    }
}
