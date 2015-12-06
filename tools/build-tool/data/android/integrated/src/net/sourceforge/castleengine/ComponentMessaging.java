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

    private final String MESSAGE_DELIMITER = "=";

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
        @Override
        public void run() {
            // do periodic stuff here
            String toNativeStr = null;
            if (toNative.size() != 0) {
                toNativeStr = toNative.get(0);
                if (debug) {
                    Log.i(TAG, "JNI: Java posting a message to the native code: " + toNativeStr);
                }
                toNative.remove(0);
            }

            String message = getActivity().jniMessage(toNativeStr);
            boolean wasMessage = message != null && message.length() != 0;
            if (wasMessage) {
                if (debug) {
                    Log.i(TAG, "JNI: Java received message from native code: " + message);
                }
                String[] parts = message.split("=");
                if (!getActivity().messageReceived(parts)) {
                    Log.w(TAG, "Message unhandled: " + message);
                }
            }

            // run again in a short time (shorter if there was a message, since next one may follow)
            timerHandler.postDelayed(this, wasMessage ? 10 : 500);
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
