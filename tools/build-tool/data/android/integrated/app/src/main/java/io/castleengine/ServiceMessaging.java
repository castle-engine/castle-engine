/* -*- tab-width: 4 -*- */
package io.castleengine;

import java.util.ArrayList;
import java.util.List;

import android.os.Handler;
import android.os.Looper;
import android.util.Log;

/**
 * Simple communication between Java and native code using text messages,
 * for Castle Game Engine.
 */
public class ServiceMessaging extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceMessaging";

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

    public ServiceMessaging(MainActivity activity)
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

    private class MessagePair {
        public String message;
        public byte[] messageStream;
    }

    private List<MessagePair> toNative = new ArrayList<MessagePair>();

    public void sendFromMainActivity(String[] messageParts, byte[] messageStream)
    {
        String messageGlued = glueStringArray(messageParts, 0, MESSAGE_DELIMITER);
        MessagePair messagePair = new MessagePair();
        messagePair.message = messageGlued;
        messagePair.messageStream = messageStream;
        toNative.add(messagePair);
    }

    /* Timer code inspired by
     * http://stackoverflow.com/questions/4597690/android-timer-how
     */
    Handler timerHandler = new Handler(Looper.getMainLooper());
    Runnable timerRunnable = new Runnable()
    {
        /**
         * Do periodic stuff here.
         */
        @Override
        public void run()
        {
            boolean somethingHappened = false;

            MessagePair toNativePair = null;
            if (toNative.size() != 0) {
                toNativePair = toNative.get(0);
                if (debug) {
                    logInfo(CATEGORY, "JNI: Java posting a message to the native code: " + toNativePair.message);
                }
                somethingHappened = true;
                toNative.remove(0);
            }

            String messageFromNative;
            if (toNativePair != null) {
                messageFromNative = getActivity().jniMessage(toNativePair.message, toNativePair.messageStream);
            } else {
                messageFromNative = getActivity().jniMessage(null, null);
            }

            if (messageFromNative != null &&
                messageFromNative.length() != 0) {
                if (debug) {
                    logInfo(CATEGORY, "JNI: Java received message from native code: " + messageFromNative);
                }
                somethingHappened = true;
                String[] parts = splitString(messageFromNative, MESSAGE_DELIMITER_CODE);
                if (!getActivity().messageReceived(parts)) {
                    logWarning(CATEGORY, "Message unhandled: " + glueStringArray(parts, 0, "\n"));
                }
            }

            // Run again in a short time (shorter if something happened,
            // since next messages may follow).
            // Important e.g. to download stuff with good speed.
            timerHandler.postDelayed(this, somethingHappened ? 1 : 100);
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
