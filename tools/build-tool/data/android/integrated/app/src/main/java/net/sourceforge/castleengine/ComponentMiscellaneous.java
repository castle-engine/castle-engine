/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.view.View;
import android.os.Build;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.widget.Toast;

/**
 * Integration of various Android small stuff with
 * Castle Game Engine.
 */
public class ComponentMiscellaneous extends ComponentAbstract
{
    public ComponentMiscellaneous(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "miscellaneous";
    }

    /** Immersive mode. */
    @Override
    public void onWindowFocusChanged(boolean hasFocus) {
        if (hasFocus) {
            /* To have all the flags and methods below available
             * (in particular, SYSTEM_UI_FLAG_IMMERSIVE_STICKY)
             * wee need Android API version 19. Check the version at runtime,
             * to handle various API versions with the same apk.
             */
            if (Build.VERSION.SDK_INT >= 19) {
                View decorView = getActivity().getWindow().getDecorView();
                decorView.setSystemUiVisibility(
                    View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                    | View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                    | View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
                    | View.SYSTEM_UI_FLAG_HIDE_NAVIGATION
                    | View.SYSTEM_UI_FLAG_FULLSCREEN
                    | View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY
                );
            }
        }
    }

    /* Shares ---------------------------------------------------------------- */

    /**
     * Share a text with other applications.
     * See https://developer.android.com/training/sharing/send.html
     */
    private void intentSendText(String title, String subject, String text)
    {
        Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.putExtra(Intent.EXTRA_TEXT, text);
        sendIntent.putExtra(Intent.EXTRA_SUBJECT, subject);
        sendIntent.setType("text/plain");
        getActivity().startActivity(Intent.createChooser(sendIntent, title));
    }

    /**
     * View uri.
     * See http://stackoverflow.com/questions/4969217/share-application-link-in-android
     */
    private void intentViewUri(String uri)
    {
        Intent intent = new Intent(Intent.ACTION_VIEW);
        intent.setData(Uri.parse(uri));
        getActivity().startActivity(intent);
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("intent-view-uri")) {
            intentViewUri(parts[1]);
            return true;
        } else
        if (parts.length == 4 && parts[0].equals("intent-send-text")) {
            intentSendText(parts[1], parts[2], parts[3]);
            return true;
        } else
        if (parts.length == 2 && parts[0].equals("on-screen-notification")) {
            Toast.makeText(getActivity().getApplicationContext(), parts[1], Toast.LENGTH_SHORT).show();
            return true;
        } else
        {
            return false;
        }
    }
}
