/* -*- tab-width: 4 -*- */
package net.sourceforge.castleengine;

import android.view.View;
import android.os.Build;
import android.content.Intent;
import android.content.Context;
import android.content.res.TypedArray;
import android.net.Uri;
import android.widget.Toast;
import android.app.Service;
import android.view.inputmethod.InputMethodManager;
import android.view.KeyEvent;

import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsControllerCompat;
import androidx.core.view.WindowInsetsCompat;

/**
 * Integration of various Android small stuff with
 * Castle Game Engine.
 */
public class ServiceMiscellaneous extends ServiceAbstract
{
    public ServiceMiscellaneous(MainActivity activity)
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
        int[] attrs = {android.R.attr.windowFullscreen};
        TypedArray ta = getActivity().getTheme().obtainStyledAttributes(attrs);
        if (ta.getBoolean(0, true) && hasFocus) {
            View decorView = getActivity().getWindow().getDecorView();

            // Implementation follows https://developer.android.com/training/system-ui/immersive
            WindowInsetsControllerCompat windowInsetsController =
                ViewCompat.getWindowInsetsController(decorView);
            if (windowInsetsController == null) {
                return;
            }
            // Configure the behavior of the hidden system bars
            windowInsetsController.setSystemBarsBehavior(
                WindowInsetsControllerCompat.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE
            );
            // Hide both the status bar and the navigation bar
            windowInsetsController.hide(WindowInsetsCompat.Type.systemBars());
        }
    }

    /* Shares ---------------------------------------------------------------- */

    /**
     * Share a text with other applications.
     * See https://developer.android.com/training/sharing/send.html
     */
    private void shareText(String title, String subject, String text)
    {
        Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.putExtra(Intent.EXTRA_TEXT, text);
        sendIntent.putExtra(Intent.EXTRA_SUBJECT, subject);
        sendIntent.setType("text/plain");
        getActivity().startActivity(Intent.createChooser(sendIntent, title));
    }

    /**
     * View URL.
     * See http://stackoverflow.com/questions/4969217/share-application-link-in-android
     */
    private void viewUrl(String url)
    {
        Intent intent = new Intent(Intent.ACTION_VIEW);
        intent.setData(Uri.parse(url));
        getActivity().startActivity(intent);
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if (parts.length == 2 && parts[0].equals("view-url")) {
            viewUrl(parts[1]);
            return true;
        } else
        if (parts.length == 4 && parts[0].equals("share-text")) {
            shareText(parts[1], parts[2], parts[3]);
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
