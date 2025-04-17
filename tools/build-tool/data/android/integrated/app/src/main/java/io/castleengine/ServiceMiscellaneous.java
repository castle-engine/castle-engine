/* -*- tab-width: 4 -*- */
package io.castleengine;

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
import android.view.Window;

import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsControllerCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.core.graphics.Insets;

/**
 * Integration of various Android small stuff with
 * Castle Game Engine.
 */
public class ServiceMiscellaneous extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceMiscellaneous";

    public ServiceMiscellaneous(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "miscellaneous";
    }

    @Override
    public void onCreate()
    {
        super.onCreate();

        Window window = getActivity().getWindow();
        View decorView = window.getDecorView();

        /* Use setOnApplyWindowInsetsListener to get insets values,
           following https://developer.android.com/develop/ui/views/layout/edge-to-edge
           Note: Trying to get insets later, by various ways (I tried many...)
           generally results in nothing, as insets have been already consumed.
           Using setOnApplyWindowInsetsListener is critical to get the insets. */

        ViewCompat.setOnApplyWindowInsetsListener(decorView, (v, windowInsets) -> {
            Insets insets = windowInsets.getInsets(WindowInsetsCompat.Type.systemBars());

            /* Send to Pascal code value of safe borders.
               As 4D vector, in CSS order (top, right, bottom, left,
               https://developer.mozilla.org/en-US/docs/Web/CSS/Shorthand_properties ). */
            messageSend(new String[]{"safe-borders",
                Integer.toString(insets.top),
                Integer.toString(insets.right),
                Integer.toString(insets.bottom),
                Integer.toString(insets.left)
            });

            // Return CONSUMED if you don't want want the window insets to keep passing
            // down to descendant views.
            // It doesn't really matter for CGE usage, most likely,
            // as we don't have descendant views?
            return WindowInsetsCompat.CONSUMED;
        });
    }

    /** Immersive mode. */
    private void hideSystemBars(View decorView)
    {
        // Implementation follows
        // https://developer.android.com/develop/ui/views/layout/immersive?hl=pl#java
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

    /** Immersive mode. */
    @Override
    public void onWindowFocusChanged(boolean hasFocus)
    {
        if (hasFocus) {
            Window window = getActivity().getWindow();
            View decorView = window.getDecorView();

            /* This effectively calculates whether our CastleEngineManifest.xml
            has fullscreen_immersive="true" .
            The resulting Java bool "fullscreen" should reflect this. */
            int[] attrs = {android.R.attr.windowFullscreen};
            TypedArray ta = getActivity().getTheme().obtainStyledAttributes(attrs);
            boolean fullscreen = ta.getBoolean(0, false);

            if (fullscreen) {
                hideSystemBars(decorView);
            }
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
