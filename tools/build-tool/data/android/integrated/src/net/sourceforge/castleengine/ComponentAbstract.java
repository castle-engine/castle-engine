/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.content.Intent;

import android.app.Activity;

/**
 * Abstract component of MainActivity.
 */
public class ComponentAbstract
{
    private MainActivity mActivity;

    public ComponentAbstract(MainActivity activity)
    {
        mActivity = activity;
    }

    protected MainActivity getActivity()
    {
        return mActivity;
    }

    protected void messageSend(String[] s)
    {
        getActivity().messageSend(s);
    }

    /**
     * Helper utility to glue string array.
     */
    protected static String glueStringArray(String[] input, int startIndex,
        String separator)
    {
        StringBuilder builder = new StringBuilder(input[startIndex]);
        for (int i = startIndex + 1; i < input.length; i++) {
            builder.append(separator + input[i]);
        }
        return builder.toString();
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
